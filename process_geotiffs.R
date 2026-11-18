# process_geotiffs.R
# Re-extracts cropped Sentinel-2 multi-spectral bands over Eleven Mile Reservoir
# and saves them as standalone GeoTIFFs (.tif) in WGS84 EPSG:4326 for Leaflet rendering.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(rstac)
  library(jsonlite)
})

cat("Loading reservoir polygon...\n")
em_res <- readRDS("data/eleven_mile_reservoir_polygon.rds")
em_utm <- st_transform(em_res, 32613)
bb <- st_bbox(em_utm)
crop_ext <- ext(bb["xmin"] - 400, bb["xmax"] + 400, bb["ymin"] - 400, bb["ymax"] + 400)

scenes_meta <- list(
  list(id = "s2_20230612", name = "Spring Baseline (Low Bloom)", date = "2023-06-25", start = "2023-06-24", end = "2023-06-26"),
  list(id = "s2_20230821", name = "Peak Summer (Active Bloom)", date = "2023-08-21", start = "2023-08-20", end = "2023-08-22"),
  list(id = "s2_20230831", name = "Late Summer (Dense Bloom)",  date = "2023-08-31", start = "2023-08-30", end = "2023-09-01")
)

s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")
manifest <- list()

for (sc in scenes_meta) {
  cat(sprintf("\n=== Processing Scene %s (%s) ===\n", sc$name, sc$date))
  tif_ndci  <- sprintf("data/satellite_rasters/%s_ndci.tif", sc$id)
  tif_cyano <- sprintf("data/satellite_rasters/%s_cyano.tif", sc$id)
  tif_fai   <- sprintf("data/satellite_rasters/%s_fai.tif", sc$id)
  tif_water <- sprintf("data/satellite_rasters/%s_water.tif", sc$id)
  
  if (file.exists(tif_ndci) && file.exists(tif_cyano)) {
    cat("GeoTIFFs already exist for", sc$id, "- loading.\n")
    r_ndci <- rast(tif_ndci)
    manifest[[sc$id]] <- list(
      id = sc$id,
      name = sc$name,
      date = sc$date,
      ndci_tif = tif_ndci,
      cyano_tif = tif_cyano,
      fai_tif = tif_fai,
      water_tif = tif_water,
      ndci_mean = mean(values(r_ndci, na.rm = TRUE)),
      ndci_max = max(values(r_ndci, na.rm = TRUE)),
      bloom_area_pct = mean(values(r_ndci, na.rm = TRUE) > 0.08) * 100
    )
    next
  }
  
  search_res <- s_obj |>
    stac_search(
      collections = "sentinel-2-l2a",
      bbox = c(-105.58, 38.89, -105.46, 38.97),
      datetime = paste0(sc$start, "T00:00:00Z/", sc$end, "T23:59:59Z"),
      limit = 3
    ) |>
    get_request() |>
    items_sign_planetary_computer()
  
  best_idx <- 1
  if (length(search_res$features) > 1) {
    cc_vals <- sapply(search_res$features, function(f) f$properties[["eo:cloud_cover"]])
    best_idx <- which.min(cc_vals)
  }
  feat <- search_res$features[[best_idx]]
  cat(sprintf("Selected feature: %s (Clouds: %.1f%%)\n", feat$id, feat$properties[["eo:cloud_cover"]]))
  
  # Fetch cropped bands
  b2 <- crop(rast(paste0("/vsicurl/", feat$assets$B02$href)), crop_ext)
  b3 <- crop(rast(paste0("/vsicurl/", feat$assets$B03$href)), crop_ext)
  b4 <- crop(rast(paste0("/vsicurl/", feat$assets$B04$href)), crop_ext)
  b5 <- crop(rast(paste0("/vsicurl/", feat$assets$B05$href)), crop_ext)
  b8 <- crop(rast(paste0("/vsicurl/", feat$assets$B08$href)), crop_ext)
  b11 <- crop(rast(paste0("/vsicurl/", feat$assets$B11$href)), crop_ext)
  
  # Resample to 10m
  b5_10m <- resample(b5, b4)
  b11_10m <- resample(b11, b4)
  
  r_b2 <- b2 / 10000
  r_b3 <- b3 / 10000
  r_b4 <- b4 / 10000
  r_b5 <- b5_10m / 10000
  r_b8 <- b8 / 10000
  r_b11 <- b11_10m / 10000
  
  # 1. MNDWI & Water Mask
  mndwi <- (r_b3 - r_b11) / (r_b3 + r_b11 + 1e-6)
  water_mask <- mndwi > 0.02
  
  # 2. NDCI: (B05 - B04)/(B05 + B04)
  ndci <- (r_b5 - r_b4) / (r_b5 + r_b4 + 1e-6)
  ndci_water <- mask(ndci, water_mask)
  
  # 3. SNAP CyanoIndex (Wynne & Matthews Spectral Shape 681nm / 665nm)
  # SS = rho(B04) - rho(B03) - (rho(B05) - rho(B03)) * ((665 - 560)/(705 - 560))
  # CI = -SS
  cyano_ss <- r_b4 - r_b3 - (r_b5 - r_b3) * ((665 - 560) / (705 - 560))
  cyano_index <- -cyano_ss
  cyano_water <- mask(cyano_index, water_mask)
  
  # 4. Floating Algae Index (FAI)
  fai <- r_b8 - (r_b4 + (r_b11 - r_b4) * ((842 - 665) / (1610 - 665)))
  fai_water <- mask(fai, water_mask)
  
  # Reproject to WGS84 (EPSG:4326) for Leaflet
  cat("Reprojecting to EPSG:4326 and writing GeoTIFFs...\n")
  ndci_wgs  <- project(ndci_water, "EPSG:4326")
  cyano_wgs <- project(cyano_water, "EPSG:4326")
  fai_wgs   <- project(fai_water, "EPSG:4326")
  water_wgs <- project(water_mask, "EPSG:4326")
  
  writeRaster(ndci_wgs, tif_ndci, overwrite = TRUE)
  writeRaster(cyano_wgs, tif_cyano, overwrite = TRUE)
  writeRaster(fai_wgs, tif_fai, overwrite = TRUE)
  writeRaster(water_wgs, tif_water, overwrite = TRUE)
  
  stats <- list(
    id = sc$id,
    name = sc$name,
    date = sc$date,
    ndci_tif = tif_ndci,
    cyano_tif = tif_cyano,
    fai_tif = tif_fai,
    water_tif = tif_water,
    cloud_cover = feat$properties[["eo:cloud_cover"]],
    ndci_mean = mean(values(ndci_water, na.rm = TRUE)),
    ndci_max = max(values(ndci_water, na.rm = TRUE)),
    bloom_area_pct = mean(values(ndci_water, na.rm = TRUE) > 0.08) * 100,
    cyano_cells_est = max(0, mean(values(ndci_water, na.rm = TRUE)) * 180000 + 5000)
  )
  manifest[[sc$id]] <- stats
  cat(sprintf("Saved %s GeoTIFFs. Mean NDCI: %.4f | Max NDCI: %.4f | Bloom Pct: %.1f%%\n",
              sc$id, stats$ndci_mean, stats$ndci_max, stats$bloom_area_pct))
}

saveRDS(manifest, "data/satellite_rasters/satellite_manifest.rds")
write_json(manifest, "data/satellite_rasters/satellite_manifest.json", pretty = TRUE, auto_unbox = TRUE)
cat("\n=== ALL GEOTIFFS PROCESSED SUCCESSFULLY! ===\n")
