# R/utils_helpers.R
# Helper functions, color palettes, and models for the Eleven Mile Reservoir Tool

suppressPackageStartupMessages({
  library(raster)
  library(leaflet)
  library(dplyr)
  library(plotly)
})

# 1. Palettes for Satellite Indices
get_raster_palette <- function(index_type, val_range = NULL) {
  if (index_type == "ndci") {
    # NDCI calibrated for alpine lakes: -0.15 (clean blue) to 0.30+ (dense toxic bloom red)
    domain <- if (is.null(val_range)) c(-0.15, 0.30) else val_range
    colorNumeric(
      palette = c("#2b83ba", "#74add1", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
      domain = domain,
      na.color = "transparent"
    )
  } else if (index_type == "cyano") {
    # CyanoIndex spectral shape (-SS) focusing on 665nm phycocyanin dip and 705nm red-edge scatter
    domain <- if (is.null(val_range)) c(-0.02, 0.06) else val_range
    colorNumeric(
      palette = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
      domain = domain,
      na.color = "transparent"
    )
  } else if (index_type == "fai") {
    domain <- if (is.null(val_range)) c(-0.02, 0.08) else val_range
    colorNumeric(
      palette = c("#005a32", "#41ab5d", "#a1d99b", "#f7fcb9", "#d7191c"),
      domain = domain,
      na.color = "transparent"
    )
  } else {
    colorNumeric("Blues", domain = c(0, 1), na.color = "transparent")
  }
}

# 2. Risk Badge Generator
get_risk_badge <- function(risk_level) {
  switch(
    risk_level,
    "Severe / Toxin Alert" = tags$span(class = "badge bg-danger fs-6", "Severe / Toxin Alert"),
    "Moderate / High Bloom" = tags$span(class = "badge bg-warning text-dark fs-6", "Moderate Bloom Risk"),
    "Low / Elevated Algae" = tags$span(class = "badge bg-info text-dark fs-6", "Low / Elevated Algae"),
    tags$span(class = "badge bg-success fs-6", "Baseline / Clear")
  )
}

# 3. Spectral Profile Model (Based on SNAP CyanoIndex & NC State Remote Sensing physics)
# Simulates reflectance across Sentinel-2 bands (490, 560, 665, 705, 740, 783, 842 nm)
get_spectral_profiles <- function() {
  tibble(
    Wavelength = c(490, 560, 665, 705, 740, 783, 842),
    Band = c("B02 (Blue)", "B03 (Green)", "B04 (Red)", "B05 (RedEdge 1)", "B06 (RedEdge 2)", "B07 (RedEdge 3)", "B08 (NIR)"),
    Clear_Alpine_Water = c(0.038, 0.032, 0.009, 0.003, 0.002, 0.001, 0.001),
    Moderate_Algal_Bloom = c(0.028, 0.048, 0.022, 0.039, 0.019, 0.008, 0.006),
    Severe_Cyanobacteria_Bloom = c(0.022, 0.065, 0.018, 0.078, 0.042, 0.016, 0.014)
  )
}

# 4. Proactive vs Reactive Decision Model
simulate_mitigation_scenario <- function(temp_delta, inflow_reduction_pct, p_load_delta, strategy = "none") {
  # Baseline summer conditions in Eleven Mile Reservoir:
  # Base Chl-a: 12 ug/L
  # Base Microcystin prob: 18%
  # Base Trout Good Habitat: 78% of volume
  
  # Climate & Drought multiplier
  temp_mult <- 1 + (temp_delta * 0.35)
  drought_mult <- 1 + (inflow_reduction_pct / 100 * 0.85)
  nutrient_mult <- 1 + (p_load_delta / 100 * 0.65)
  
  unmitigated_chla <- 12.0 * temp_mult * drought_mult * nutrient_mult
  unmitigated_prob <- min(98, round(18 * temp_mult * drought_mult * nutrient_mult))
  unmitigated_trout_vol <- max(15, round(78 - (temp_delta * 12) - (inflow_reduction_pct / 100 * 25)))
  
  # Strategy adjustments
  strat_impacts <- list(
    none = list(
      name = "No Treatment (Baseline Trajectory)",
      type = "None",
      chla_reduction = 0.0,
      toxin_release_risk = "Low during growth, Extreme upon natural senescence",
      trout_safety = "Poor (Severe hypolimnetic anoxia and warm surface)",
      cap_cost = "$0",
      om_cost = "$0/yr",
      cell_rupture = "No intervention",
      notes = "Uncontrolled bloom accumulation leading to taste-and-odor complaints and recreation bans."
    ),
    reactive_copper = list(
      name = "Reactive Algaecide (Copper Sulfate / Chelated Copper)",
      type = "Reactive",
      chla_reduction = 0.70, # Kills 70% of biomass temporarily
      toxin_release_risk = "CRITICAL: Mass cell lysis causes immediate release of lethal intracellular microcystins",
      trout_safety = "DANGEROUS: Copper toxicity + acute oxygen depletion from decomposing biomass causes trout kills",
      cap_cost = "$15,000",
      om_cost = "$65,000/yr",
      cell_rupture = "Massive rupture (100% cell lysis)",
      notes = "Traditional emergency response. Triggers severe dissolved toxin spike and copper accumulation in benthic sediments."
    ),
    proactive_ultrasonic = list(
      name = "Proactive Solar-Powered Ultrasonic Transducers (LG Sonic)",
      type = "Proactive",
      chla_reduction = 0.65,
      toxin_release_risk = "MINIMAL: Disrupts buoyancy gas vesicles without cell membrane rupture",
      trout_safety = "EXCELLENT: Non-chemical, harmless to fish, macroinvertebrates, and zooplankton",
      cap_cost = "$180,000",
      om_cost = "$12,000/yr",
      cell_rupture = "Intact cells sink to darkness and die naturally without toxin surge",
      notes = "Autonomous solar buoys continuously monitor water quality and adapt acoustic frequencies."
    ),
    proactive_aeration = list(
      name = "Proactive Hypolimnetic Oxygenation / Aeration System",
      type = "Proactive",
      chla_reduction = 0.58,
      toxin_release_risk = "VERY LOW: Suppresses bloom conditions by capping internal sediment phosphorus release",
      trout_safety = "SUPERIOR: Expands coldwater trout refuge by maintaining dissolved oxygen > 6 mg/L in deep basin",
      cap_cost = "$450,000",
      om_cost = "$38,000/yr",
      cell_rupture = "Prevents bloom formation rather than killing standing crop",
      notes = "Directly addresses climate-driven thermal stratification and anoxic phosphorus recycling."
    ),
    proactive_lanthanum = list(
      name = "Proactive Lanthanum-Modified Bentonite (Phoslock)",
      type = "Proactive",
      chla_reduction = 0.68,
      toxin_release_risk = "VERY LOW: Strips bioavailable orthophosphate from water column and caps sediment flux",
      trout_safety = "VERY GOOD: NSF/ANSI 60 certified for drinking water reservoirs, inert to coldwater fish",
      cap_cost = "$120,000",
      om_cost = "$45,000/treatment",
      cell_rupture = "No cell rupture; starves cyanobacteria of critical phosphorus",
      notes = "Irreversibly binds free phosphate ions under both oxic and anoxic lake conditions."
    ),
    proactive_flow_pulse = list(
      name = "Proactive Coordinated Inflow Pulsing (Upstream Spinney Releases)",
      type = "Proactive",
      chla_reduction = 0.42,
      toxin_release_risk = "NEGLIGIBLE: Flushes reservoir and breaks stagnation during peak July/August heatwaves",
      trout_safety = "OUTSTANDING: Enhances stream habitat and coldwater inflows into South Platte arm",
      cap_cost = "$25,000 (Gaging/telemetry)",
      om_cost = "$15,000/yr",
      cell_rupture = "Reduces hydraulic retention time, preventing slow-growing cyanobacteria dominance",
      notes = "Utilizes Denver Water system operational flexibility between Spinney Mountain and Eleven Mile."
    ),
    proactive_peroxide = list(
      name = "Proactive / Targeted Hydrogen Peroxide (H2O2 Advanced Oxidation)",
      type = "Proactive Eco-Chemical",
      chla_reduction = 0.72,
      toxin_release_risk = "LOW: Selectively oxidizes cyanobacteria without bioaccumulating; rapid degradation to H2O + O2",
      trout_safety = "VERY GOOD: Non-persistent oxidizer; zero toxic copper residue; releases supplemental dissolved oxygen",
      cap_cost = "$35,000 (Precision dosing barge & injectors)",
      om_cost = "$28,000/treatment",
      cell_rupture = "Selective oxidative disruption of photosynthetic apparatus; rapidly degrades dissolved microcystins",
      notes = "Eco-friendly EPA-registered selective cyanobactericide. Degrades into harmless water and oxygen within 48 hours."
    )
  )
  
  sel <- strat_impacts[[strategy]]
  if (is.null(sel)) sel <- strat_impacts$none
  
  # Calculate resulting metrics
  res_chla <- round(unmitigated_chla * (1 - sel$chla_reduction), 1)
  res_prob <- max(5, round(unmitigated_prob * (1 - sel$chla_reduction * 0.9)))
  res_trout <- if (strategy == "reactive_copper") {
    max(5, unmitigated_trout_vol - 35) # Severe oxygen sag and copper stress
  } else if (grepl("aeration", strategy)) {
    min(95, unmitigated_trout_vol + 30) # Restores deep refuge
  } else if (grepl("proactive", strategy)) {
    min(90, unmitigated_trout_vol + 15)
  } else {
    unmitigated_trout_vol
  }
  
  # High risk days
  res_days <- max(0, round((res_chla / 10)^1.8 * 6))
  
  list(
    strategy = sel$name,
    type = sel$type,
    peak_chla = res_chla,
    unmitigated_chla = round(unmitigated_chla, 1),
    toxin_prob = res_prob,
    unmitigated_prob = unmitigated_prob,
    trout_habitat_pct = res_trout,
    unmitigated_trout_pct = unmitigated_trout_vol,
    high_risk_days = res_days,
    toxin_risk_desc = sel$toxin_release_risk,
    trout_safety_desc = sel$trout_safety,
    cap_cost = sel$cap_cost,
    om_cost = sel$om_cost,
    cell_rupture = sel$cell_rupture,
    notes = sel$notes
  )
}
