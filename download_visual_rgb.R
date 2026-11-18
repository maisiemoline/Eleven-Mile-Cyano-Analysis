# download_visual_rgb.R
# Downloads authentic Copernicus Sentinel-2 True Color (RGB / TCI) satellite imagery
# over Eleven Mile Reservoir for each target date.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(rstac)
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

for (sc in scenes_meta) {
  out_rgb <- sprintf("data/satellite_rasters/%s_rgb.tif", sc$id)
  if (file.exists(out_rgb)) {
    cat("RGB GeoTIFF already exists for", sc$id, "- skipping.\n")
    next
  }
  
  cat(sprintf("\nFetching True Color RGB for Scene %s (%s)...\n", sc$name, sc$date))
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
  
  vis_url <- paste0("/vsicurl/", feat$assets$visual$href)
  r_vis <- rast(vis_url)
  vis_cropped <- crop(r_vis, crop_ext)
  
  # Project to WGS84 for Leaflet
  cat("Reprojecting True Color RGB to EPSG:4326...\n")
  vis_wgs84 <- project(vis_cropped, "EPSG:4326")
  
  # Ensure RGB flags are set
  terra::RGB(vis_wgs84) <- 1:3
  
  writeRaster(vis_wgs84, out_rgb, overwrite = TRUE, datatype = "INT1U")
  cat("Saved authentic RGB satellite photograph to:", out_rgb, "\n")
}

cat("\n=== ALL TRUE-COLOR SATELLITE PHOTOGRAPHS EXTRACTED! ===\n")
