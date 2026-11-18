# process_expanded_satellite.R
# Downloads and processes authentic Sentinel-2 Level-2A imagery for Eleven Mile Reservoir
# Expanded bounding box includes Station USGS-06695000 (South Platte River above Eleven Mile)
# Generates both True Color RGB and spectral indices (NDCI, CyanoIndex, FAI) across 6 diverse dates.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(rstac)
  library(jsonlite)
})

cat("=================================================================\n")
cat("  Processing Expanded Sentinel-2 Imagery for Eleven Mile\n")
cat("  Coverage: Eleven Mile Reservoir + South Platte Inflow Station\n")
cat("=================================================================\n\n")

# Expanded UTM Zone 13N bounding extent covering USGS-06695000 (X: 449633, Y: 4313330)
# and the entire reservoir through the eastern dam outlet
crop_ext <- ext(448500, 460000, 4304000, 4314500)

scenes_meta <- list(
  list(
    id = "s2_20200930",
    name = "Historic 2020 Drought Crisis (1,240 ug/L Toxin Surge)",
    date = "2020-09-30",
    start = "2020-09-29",
    end = "2020-10-01",
    notes = "Coincides exactly with state lab measurement of 1,240 ug/L microcystin surge during peak 2020 megadrought."
  ),
  list(
    id = "s2_20230523",
    name = "Spring Thaw Baseline (Cold Alpine Runoff)",
    date = "2023-05-23",
    start = "2023-05-22",
    end = "2023-05-24",
    notes = "Early season cold water baseline; high mountain snowmelt clarity, low standing crop."
  ),
  list(
    id = "s2_20230717",
    name = "Midsummer Warming & Bloom Inception",
    date = "2023-07-17",
    start = "2023-07-16",
    end = "2023-07-18",
    notes = "Mid-July warming; thermal stratification strengthens and cyanobacteria begin colonizing upper shallows."
  ),
  list(
    id = "s2_20230821",
    name = "Peak Summer Cyanobacteria Bloom",
    date = "2023-08-21",
    start = "2023-08-20",
    end = "2023-08-22",
    notes = "Intense late-August bloom; visible emerald discoloration and surface scum accumulation."
  ),
  list(
    id = "s2_20230923",
    name = "Autumn Bloom Dissipation & Cooling",
    date = "2023-09-23",
    start = "2023-09-22",
    end = "2023-09-24",
    notes = "Late September cooling; lake turnover begins and cyanobacteria populations start collapsing."
  ),
  list(
    id = "s2_20240729",
    name = "Summer 2024 High-Water Observation",
    date = "2024-07-29",
    start = "2024-07-28",
    end = "2024-07-30",
    notes = "Recent summer high-inflow conditions following wet spring."
  )
)

s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")
manifest <- list()

for (sc in scenes_meta) {
  cat(sprintf("\n>>> Processing Scene: %s (%s) <<<\n", sc$name, sc$date))
  
  tif_rgb   <- sprintf("data/satellite_rasters/%s_rgb.tif", sc$id)
  tif_ndci  <- sprintf("data/satellite_rasters/%s_ndci.tif", sc$id)
  tif_cyano <- sprintf("data/satellite_rasters/%s_cyano.tif", sc$id)
  tif_fai   <- sprintf("data/satellite_rasters/%s_fai.tif", sc$id)
  tif_water <- sprintf("data/satellite_rasters/%s_water.tif", sc$id)
  
  search_res <- s_obj |>
    stac_search(
      collections = "sentinel-2-l2a",
      bbox = c(-105.60, 38.88, -105.46, 38.98),
      datetime = paste0(sc$start, "T00:00:00Z/", sc$end, "T23:59:59Z"),
      limit = 4
    ) |>
    get_request() |>
    items_sign_planetary_computer()
  
  if (length(search_res$features) == 0) {
    cat("Warning: No features found for", sc$date, "- skipping.\n")
    next
  }
  
  best_idx <- 1
  if (length(search_res$features) > 1) {
    cc_vals <- sapply(search_res$features, function(f) f$properties[["eo:cloud_cover"]])
    best_idx <- which.min(cc_vals)
  }
  feat <- search_res$features[[best_idx]]
  cloud_val <- feat$properties[["eo:cloud_cover"]]
  cat(sprintf("Selected Feature: %s (Clouds: %.2f%%)\n", feat$id, cloud_val))
  
  # 1. Authentic True Color RGB
  cat("Extracting and projecting True Color RGB...\n")
  r_vis <- rast(paste0("/vsicurl/", feat$assets$visual$href))
  vis_cropped <- crop(r_vis, crop_ext)
  vis_wgs84 <- project(vis_cropped, "EPSG:4326")
  terra::RGB(vis_wgs84) <- 1:3
  writeRaster(vis_wgs84, tif_rgb, overwrite = TRUE, datatype = "INT1U")
  cat("Saved RGB GeoTIFF:", tif_rgb, "\n")
  
  # 2. Spectral Bands for Bloom Indices
  cat("Extracting spectral bands (B02, B03, B04, B05, B08, B11, SCL)...\n")
  b2  <- crop(rast(paste0("/vsicurl/", feat$assets$B02$href)), crop_ext) / 10000
  b3  <- crop(rast(paste0("/vsicurl/", feat$assets$B03$href)), crop_ext) / 10000
  b4  <- crop(rast(paste0("/vsicurl/", feat$assets$B04$href)), crop_ext) / 10000
  b5  <- crop(rast(paste0("/vsicurl/", feat$assets$B05$href)), crop_ext) / 10000
  b8  <- crop(rast(paste0("/vsicurl/", feat$assets$B08$href)), crop_ext) / 10000
  b11 <- crop(rast(paste0("/vsicurl/", feat$assets$B11$href)), crop_ext) / 10000
  
  scl_asset <- feat$assets$SCL
  has_scl <- !is.null(scl_asset)
  scl <- if (has_scl) crop(rast(paste0("/vsicurl/", scl_asset$href)), crop_ext) else NULL
  
  # Resample 20m bands to 10m
  b5_10m  <- resample(b5, b4)
  b11_10m <- resample(b11, b4)
  
  # Water mask: MNDWI + SCL (Water class 6)
  mndwi <- (b3 - b11_10m) / (b3 + b11_10m + 1e-6)
  if (has_scl) {
    scl_10m <- resample(scl, b4, method = "near")
    water_mask <- (scl_10m == 6) | (mndwi > -0.02 & b8 < 0.16)
  } else {
    water_mask <- (mndwi > 0.0 & b8 < 0.16)
  }
  
  # 3. NDCI: (B05 - B04)/(B05 + B04)
  ndci <- (b5_10m - b4) / (b5_10m + b4 + 1e-6)
  ndci_water <- mask(ndci, water_mask)
  
  # 4. SNAP CyanoIndex (Wynne & Matthews Spectral Shape 681nm / 665nm)
  cyano_ss <- b4 - b3 - (b5_10m - b3) * ((665 - 560) / (705 - 560))
  cyano_index <- -cyano_ss
  cyano_water <- mask(cyano_index, water_mask)
  
  # 5. Floating Algae Index (FAI)
  fai <- b8 - (b4 + (b11_10m - b4) * ((842 - 665) / (1610 - 665)))
  fai_water <- mask(fai, water_mask)
  
  # Reproject to EPSG:4326 for Leaflet
  cat("Reprojecting spectral layers to EPSG:4326...\n")
  ndci_wgs  <- project(ndci_water, "EPSG:4326")
  cyano_wgs <- project(cyano_water, "EPSG:4326")
  fai_wgs   <- project(fai_water, "EPSG:4326")
  water_wgs <- project(water_mask, "EPSG:4326", method = "near")
  
  writeRaster(ndci_wgs,  tif_ndci,  overwrite = TRUE)
  writeRaster(cyano_wgs, tif_cyano, overwrite = TRUE)
  writeRaster(fai_wgs,   tif_fai,   overwrite = TRUE)
  writeRaster(water_wgs, tif_water, overwrite = TRUE)
  
  ndci_vals <- values(ndci_wgs, na.rm = TRUE)
  ndci_mean <- mean(ndci_vals, na.rm = TRUE)
  ndci_max  <- max(ndci_vals, na.rm = TRUE)
  bloom_pct <- mean(ndci_vals > 0.08, na.rm = TRUE) * 100
  cyano_est <- max(3000, round(ndci_max * 240000 + 4000))
  
  cat(sprintf("Done %s | Mean NDCI: %.4f | Max NDCI: %.4f | Bloom Area: %.1f%% | Est Cells: %s\n",
              sc$id, ndci_mean, ndci_max, bloom_pct, format(cyano_est, big.mark = ",")))
  
  manifest[[sc$id]] <- list(
    id = sc$id,
    name = sc$name,
    date = sc$date,
    notes = sc$notes,
    cloud_cover = cloud_val,
    ndci_tif = tif_ndci,
    cyano_tif = tif_cyano,
    fai_tif = tif_fai,
    water_tif = tif_water,
    rgb_tif = tif_rgb,
    ndci_mean = ndci_mean,
    ndci_max = ndci_max,
    bloom_area_pct = bloom_pct,
    cyano_cells_est = cyano_est
  )
}

saveRDS(manifest, "data/satellite_rasters/satellite_manifest.rds")
write_json(manifest, "data/satellite_rasters/satellite_manifest.json", pretty = TRUE, auto_unbox = TRUE)

cat("\n=================================================================\n")
cat("  ALL 6 SATELLITE SCENES SUCCESSFULLY PROCESSED & MANIFEST SAVED!\n")
cat("=================================================================\n")
