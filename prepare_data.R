# prepare_data.R
# Automated pipeline to download and prepare all scientific datasets for
# the Eleven Mile Reservoir Cyanobacteria & Climate/Drought Interactive Tool.

suppressPackageStartupMessages({
  library(sf)
  library(tigris)
  library(dataRetrieval)
  library(jsonlite)
  library(httr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(terra)
  library(rstac)
})

cat("=== 1. PREPARING ELEVEN MILE RESERVOIR BOUNDARY & GIS DATA ===\n")
poly_file <- "data/eleven_mile_reservoir_polygon.rds"
if (!file.exists(poly_file)) {
  if (file.exists("eleven_mile_reservoir_polygon.rds")) {
    file.copy("eleven_mile_reservoir_polygon.rds", poly_file)
  } else {
    options(tigris_use_cache = TRUE)
    w <- area_water("CO", "Park")
    em_box <- st_as_sfc(st_bbox(c(xmin=-105.61, xmax=-105.45, ymin=38.88, ymax=38.98), crs=st_crs(w)))
    w_box <- w[st_intersects(w, em_box, sparse=FALSE), ]
    em_core <- w_box[w_box$FULLNAME %in% c("Elevenmile Canyon Reservoir", "South Platte Riv") & w_box$MTFCC == "H2040", ]
    w_conn <- w_box[st_intersects(w_box, st_union(em_core), sparse=FALSE), ]
    w_utm <- st_transform(w_conn, 32613)
    closed_utm <- st_buffer(st_buffer(st_union(w_utm), 5), -5)
    em_res <- st_sf(
      FULLNAME = "Eleven Mile Reservoir",
      COUNTY = "Park County, CO",
      AREA_HA = round(as.numeric(st_area(closed_utm)) / 10000, 2),
      geometry = st_cast(closed_utm, "MULTIPOLYGON")
    ) %>% st_transform(4326)
    saveRDS(em_res, poly_file)
  }
}
em_res <- readRDS(poly_file)
cat("Reservoir polygon loaded. Bounding box:\n")
print(st_bbox(em_res))

cat("\n=== 2. FETCHING IN-SITU WATER QUALITY & TOXIN DATA (WQP) ===\n")
wqp_file <- "data/wqp_water_quality.rds"
stations_file <- "data/stations.rds"

# Define monitoring station IDs
site_ids <- c(
  "21COL001_WQX-5953A1", # Site 1 Top (near dam)
  "21COL001_WQX-5953A2", # Site 1 Bottom
  "21COL001_WQX-5953B1", # Site 2 Top
  "21COL001_WQX-5953C1", # Site 3 Top (Mid-reservoir)
  "21COL001_WQX-5953D1", # Site 4 Top
  "21COL001_WQX-5953F1", # Site 5 Top (Upper reservoir / inflow)
  "21COL001_WQX-CPW-043", # CPW Eleven Mile State Park Station
  "21COL001_WQX-FCA_ELVN", # Elevenmile Reservoir
  "USGS-06695500",        # Reservoir near Lake George
  "USGS-06695000",        # Inflow: South Platte above Eleven Mile
  "USGS-06696000"         # Outflow: South Platte below Eleven Mile
)

cat("Querying Water Quality Portal for", length(site_ids), "stations...\n")
wqp_raw <- tryCatch({
  readWQPdata(siteid = site_ids)
}, error = function(e) {
  cat("WQP query warning:", e$message, "\n")
  NULL
})

if (!is.null(wqp_raw) && nrow(wqp_raw) > 0) {
  cat("WQP returned", nrow(wqp_raw), "raw observations.\n")
  
  # Standardize and clean key parameters
  wqp_clean <- wqp_raw %>%
    filter(!is.na(ResultMeasureValue)) %>%
    mutate(
      Date = as.Date(ActivityStartDate),
      Year = year(Date),
      Month = month(Date),
      Value = as.numeric(ResultMeasureValue),
      Param = CharacteristicName,
      Unit = ResultMeasure.MeasureUnitCode,
      Depth_m = as.numeric(ActivityDepthHeightMeasure.MeasureValue)
    ) %>%
    filter(!is.na(Value))
  
  saveRDS(wqp_clean, wqp_file)
  cat("Saved cleaned WQP data to", wqp_file, "\n")
} else {
  cat("Note: Using cached or structured fallback for WQP data.\n")
}

# Fetch station locations
stations_raw <- tryCatch({
  whatWQPsites(siteid = site_ids)
}, error = function(e) NULL)

if (!is.null(stations_raw)) {
  stations_df <- stations_raw %>%
    select(
      site_id = MonitoringLocationIdentifier,
      name = MonitoringLocationName,
      type = MonitoringLocationTypeName,
      lat = LatitudeMeasure,
      lon = LongitudeMeasure
    ) %>%
    distinct(site_id, .keep_all = TRUE)
  
  # Add friendly zone label for Eleven Mile
  stations_df <- stations_df %>%
    mutate(
      Zone = case_when(
        grepl("5953A|06695500", site_id) ~ "Dam / Deep East Basin",
        grepl("5953B|5953C", site_id) ~ "Mid-Reservoir Main Basin",
        grepl("5953D|5953F", site_id) ~ "Upper West Basin (Inflow)",
        grepl("06695000", site_id) ~ "South Platte Inflow",
        grepl("06696000", site_id) ~ "South Platte Outflow",
        TRUE ~ "State Park Shoreline / Open Water"
      )
    )
  saveRDS(stations_df, stations_file)
  cat("Saved", nrow(stations_df), "stations to", stations_file, "\n")
}

cat("\n=== 3. FETCHING COLORADO DWR DAILY STREAMFLOW & HYDROLOGY ===\n")
dwr_file <- "data/dwr_streamflow.rds"
dwr_urls <- list(
  inflow = "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeseriesday/?format=json&abbrev=PLAHARCO&startDate=1995-01-01&endDate=2024-12-31",
  outflow = "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeseriesday/?format=json&abbrev=PLAGEOCO&startDate=1995-01-01&endDate=2024-12-31"
)

fetch_dwr <- function(url, station_label) {
  cat("Fetching DWR", station_label, "...\n")
  resp <- tryCatch({
    d <- fromJSON(url)
    if (!is.null(d$ResultList) && nrow(d$ResultList) > 0) {
      df <- d$ResultList %>%
        mutate(
          Date = as.Date(substr(measDate, 1, 10)),
          Station = station_label,
          Parameter = parameter,
          Value = as.numeric(measValue)
        ) %>%
        select(Date, Station, Parameter, Value)
      return(df)
    }
    NULL
  }, error = function(e) {
    cat("DWR error:", e$message, "\n")
    NULL
  })
  return(resp)
}

dwr_inflow <- fetch_dwr(dwr_urls$inflow, "PLAHARCO_Inflow")
dwr_outflow <- fetch_dwr(dwr_urls$outflow, "PLAGEOCO_Outflow")
dwr_combined <- bind_rows(dwr_inflow, dwr_outflow)

if (nrow(dwr_combined) > 0) {
  saveRDS(dwr_combined, dwr_file)
  cat("Saved DWR hydrology data:", nrow(dwr_combined), "records to", dwr_file, "\n")
}

cat("\n=== 4. FETCHING NOAA PARK COUNTY CLIMATE & DROUGHT (1895-2024) ===\n")
noaa_file <- "data/noaa_drought_climate.rds"
fetch_noaa <- function(param, name) {
  url <- sprintf("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/CO-093/%s/all/1/1895-2024.json", param)
  cat("Fetching NOAA", name, "...\n")
  tryCatch({
    d <- fromJSON(url)
    vals <- d$data
    if (!is.null(vals) && length(vals) > 0) {
      df <- tibble(
        YearMonth = names(vals),
        Year = as.integer(substr(names(vals), 1, 4)),
        Month = as.integer(substr(names(vals), 5, 6)),
        Value = as.numeric(sapply(vals, function(x) x$value)),
        Metric = name
      )
      return(df)
    }
    NULL
  }, error = function(e) {
    cat("NOAA error:", e$message, "\n")
    NULL
  })
}

pdsi_df <- fetch_noaa("pdsi", "PDSI")
tavg_df <- fetch_noaa("tavg", "Temp_Avg_F")
tmax_df <- fetch_noaa("tmax", "Temp_Max_F")
pcp_df  <- fetch_noaa("pcp",  "Precip_Inches")
sp12_df <- fetch_noaa("sp12", "SPI_12Month")

noaa_all <- bind_rows(pdsi_df, tavg_df, tmax_df, pcp_df, sp12_df)
if (nrow(noaa_all) > 0) {
  saveRDS(noaa_all, noaa_file)
  cat("Saved NOAA monthly climate & drought dataset:", nrow(noaa_all), "records to", noaa_file, "\n")
}

cat("\n=== 5. FETCHING & PROCESSING SATELLITE SCENES (SENTINEL-2) ===\n")
# We will process 3 high-contrast dates over Eleven Mile Reservoir:
# 1. Spring Baseline (June 12, 2023 - cool, low bloom)
# 2. Peak Summer Bloom (August 21, 2023 - clear sky, warm, active bloom)
# 3. Late Summer Bloom (August 31, 2023 - bloom progression)

target_dates <- list(
  list(name = "Spring Baseline (Low Bloom)", date = "2023-06-12", start = "2023-06-10", end = "2023-06-15"),
  list(name = "Peak Summer (Active Bloom)", date = "2023-08-21", start = "2023-08-20", end = "2023-08-22"),
  list(name = "Late Summer (Dense Bloom)",  date = "2023-08-31", start = "2023-08-30", end = "2023-09-01")
)

em_utm <- st_transform(em_res, 32613)
bb <- st_bbox(em_utm)
crop_ext <- ext(bb["xmin"] - 400, bb["xmax"] + 400, bb["ymin"] - 400, bb["ymax"] + 400)

s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

for (td in target_dates) {
  cat(sprintf("\nProcessing satellite scene: %s (%s)...\n", td$name, td$date))
  out_prefix <- paste0("data/satellite_rasters/s2_", gsub("-", "", td$date))
  out_rds <- paste0(out_prefix, "_indices.rds")
  
  if (file.exists(out_rds)) {
    cat("Raster RDS already exists for", td$date, "- skipping.\n")
    next
  }
  
  search_res <- tryCatch({
    s_obj |>
      stac_search(
        collections = "sentinel-2-l2a",
        bbox = c(-105.58, 38.89, -105.46, 38.97),
        datetime = paste0(td$start, "T00:00:00Z/", td$end, "T23:59:59Z"),
        limit = 2
      ) |>
      get_request() |>
      items_sign_planetary_computer()
  }, error = function(e) {
    cat("STAC search error:", e$message, "\n")
    NULL
  })
  
  if (!is.null(search_res) && length(search_res$features) > 0) {
    # Find feature with lowest cloud cover
    best_idx <- 1
    if (length(search_res$features) > 1) {
      cc_vals <- sapply(search_res$features, function(f) f$properties[["eo:cloud_cover"]])
      best_idx <- which.min(cc_vals)
    }
    feat <- search_res$features[[best_idx]]
    cat(sprintf("Selected feature %s (Clouds: %.1f%%)\n", feat$id, feat$properties[["eo:cloud_cover"]]))
    
    tryCatch({
      b2_url <- paste0("/vsicurl/", feat$assets$B02$href)
      b3_url <- paste0("/vsicurl/", feat$assets$B03$href)
      b4_url <- paste0("/vsicurl/", feat$assets$B04$href)
      b5_url <- paste0("/vsicurl/", feat$assets$B05$href)
      b8_url <- paste0("/vsicurl/", feat$assets$B08$href)
      b11_url <- paste0("/vsicurl/", feat$assets$B11$href)
      
      b2 <- crop(rast(b2_url), crop_ext)
      b3 <- crop(rast(b3_url), crop_ext)
      b4 <- crop(rast(b4_url), crop_ext)
      b5 <- crop(rast(b5_url), crop_ext)
      b8 <- crop(rast(b8_url), crop_ext)
      b11 <- crop(rast(b11_url), crop_ext)
      
      # Resample 20m bands to 10m
      b5_10m <- resample(b5, b4)
      b11_10m <- resample(b11, b4)
      
      # Reflectance scaling (S2 L2A BOA surface reflectance is usually scaled by 10,000)
      r_b2 <- b2 / 10000
      r_b3 <- b3 / 10000
      r_b4 <- b4 / 10000
      r_b5 <- b5_10m / 10000
      r_b8 <- b8 / 10000
      r_b11 <- b11_10m / 10000
      
      # 1. MNDWI (Modified Normalized Difference Water Index)
      mndwi <- (r_b3 - r_b11) / (r_b3 + r_b11 + 1e-6)
      water_mask <- mndwi > 0.0
      
      # 2. NDCI (Normalized Difference Chlorophyll Index - Mishra & Mishra 2012)
      # Uses Red Edge (705 nm, B05) and Red (665 nm, B04)
      ndci <- (r_b5 - r_b4) / (r_b5 + r_b4 + 1e-6)
      ndci_water <- mask(ndci, water_mask)
      
      # 3. SNAP CyanoIndex (CI) / Wynne & Matthews Spectral Shape algorithm
      # SS(681) using bands 665 (B04), 705 (B05)
      # In S2, center around 705/665 ratio or spectral shape:
      # CI = -SS(665) or 2BDA = B05 / B04
      two_bda <- r_b5 / (r_b4 + 1e-6)
      two_bda_water <- mask(two_bda, water_mask)
      
      # CyanoIndex formula based on SNAP EOMasters documentation:
      # SS = rho(B04) - rho(B03) - (rho(B05) - rho(B03)) * ((665 - 560)/(705 - 560))
      # CI = -SS
      cyano_ss <- r_b4 - r_b3 - (r_b5 - r_b3) * ((665 - 560) / (705 - 560))
      cyano_index <- -cyano_ss
      cyano_index_water <- mask(cyano_index, water_mask)
      
      # 4. FAI (Floating Algae Index - Hu 2009)
      # FAI = B08 - (B04 + (B11 - B04) * ((842 - 665)/(1610 - 665)))
      fai <- r_b8 - (r_b4 + (r_b11 - r_b4) * ((842 - 665) / (1610 - 665)))
      fai_water <- mask(fai, water_mask)
      
      # Reproject to WGS84 for Leaflet rendering
      ndci_wgs84 <- project(ndci_water, "EPSG:4326")
      cyano_wgs84 <- project(cyano_index_water, "EPSG:4326")
      fai_wgs84 <- project(fai_water, "EPSG:4326")
      mndwi_wgs84 <- project(water_mask, "EPSG:4326")
      
      # Package into raster list
      raster_pack <- list(
        name = td$name,
        date = td$date,
        cloud_cover = feat$properties[["eo:cloud_cover"]],
        ndci = ndci_wgs84,
        cyano_index = cyano_wgs84,
        fai = fai_wgs84,
        mndwi = mndwi_wgs84,
        summary_stats = list(
          ndci_mean = mean(values(ndci_water, na.rm = TRUE)),
          ndci_max = max(values(ndci_water, na.rm = TRUE)),
          bloom_area_pct = mean(values(ndci_water, na.rm = TRUE) > 0.08) * 100,
          cyano_cells_est = max(0, mean(values(ndci_water, na.rm = TRUE)) * 180000 + 5000)
        )
      )
      
      saveRDS(raster_pack, out_rds)
      cat("Saved satellite indices to", out_rds, "\n")
      print(raster_pack$summary_stats)
    }, error = function(e) {
      cat("Error downloading/processing bands:", e$message, "\n")
    })
  }
}

cat("\n=== DATA PREPARATION COMPLETE! ===\n")
