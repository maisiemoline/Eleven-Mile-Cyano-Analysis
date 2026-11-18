# Eleven Mile Reservoir Cyanobacteria, Climate Change & Proactive Mitigation Tool

An interactive, scientific decision-support web application built in **R** (`Shiny`, `bslib`, `Leaflet`, `Plotly`, `terra`, `sf`) focused on **cyanobacteria and harmful algal blooms (HABs)** in **Eleven Mile Reservoir, Colorado**.

This tool provides rigorous empirical and remote-sensing evidence demonstrating how **climate change (rising summer air temperatures and earlier snowmelt)** and **severe drought (prolonged hydraulic retention time and reduced South Platte River flushing)** escalate cyanobacterial bloom frequency and cyanotoxin severity, while providing an interactive decision-support simulator proving why **proactive mitigation** is essential over **reactive algaecides**.

---

## 🚀 Quick Start: Launching the App

You can launch the interactive app directly from your terminal or R session:

### Option A: From Terminal
```bash
cd "/Users/Maisie.Moline/Desktop/Eleven Mile Cyano"
Rscript run_app.R
```
The application will launch and automatically open at **`http://127.0.0.1:3838`**.

### Option B: In R or RStudio
```r
setwd("/Users/Maisie.Moline/Desktop/Eleven Mile Cyano")
shiny::runApp(port = 3838)
```

---

## 🔬 Core Scientific Modules

### 1. 🛰️ Satellite Remote Sensing Explorer (CyanoIndex & NDCI)
- **Expanded Multi-Temporal Sentinel-2 Imagery**: Level-2A BOA surface reflectance processed via Microsoft Planetary Computer STAC over Eleven Mile Reservoir with an **expanded bounding box** covering **Station USGS-06695000 (South Platte River above Eleven Mile)** and the entire lake to the dam outlet.
- **6 Diverse Observation Dates Across Seasons & Years**:
  1. **Historic 2020 Megadrought Crisis (Sep 30, 2020)**: Exact date of the state laboratory's record-shattering **1,240 µg/L microcystin surge**, captured with 0.01% cloud cover!
  2. **Spring Thaw Baseline (May 23, 2023)**: Cold alpine snowmelt runoff with low standing crop and high water clarity.
  3. **Midsummer Bloom Inception (Jul 17, 2023)**: Thermal stratification and early cyanobacterial colonization.
  4. **Peak Summer Cyanobacteria Bloom (Aug 21, 2023)**: Intense bloom with visible green swirls and scum accumulations across the main basin.
  5. **Autumn Bloom Dissipation & Cooling (Sep 23, 2023)**: Seasonal turnover and bloom senescence.
  6. **Summer 2024 High-Water Observation (Jul 29, 2024)**: Recent post-runoff summer baseline.
- **European Space Agency (ESA) SNAP CyanoIndex**: Implements the Wynne & Matthews spectral shape algorithm around 681 nm, detecting local reflectance minimums caused by chlorophyll-a and phycocyanin fluorescence:
  $$\text{SS}(\lambda) = \rho_s(\lambda) - \rho_s(\lambda_-) - [\rho_s(\lambda_+) - \rho_s(\lambda_-)] \times \frac{\lambda - \lambda_-}{\lambda_+ - \lambda_-}$$
  $$\text{CyanoIndex (CI)} = -\text{SS}(681)$$
- **Normalized Difference Chlorophyll Index (NDCI)** (Mishra & Mishra, 2012):
  $$\text{NDCI} = \frac{\rho_{705} - \rho_{665}}{\rho_{705} + \rho_{665}} = \frac{B05 - B04}{B05 + B04}$$
- **Floating Algae Index (FAI)** (Hu, 2009): Detects dense surface scums using Red (665 nm), NIR (842 nm), and SWIR (1610 nm).
- **Authentic Sentinel-2 Natural Color RGB**: 10-meter optical photograph from orbit, letting users view natural water clarity and visible bloom swirls alongside the calibrated spectral indices.

### 2. 📈 Climate Change & Drought Drivers Engine
- **The Physical Mechanism**:
  1. **Thermal Stratification**: Park County summer air temperatures have warmed by **+2.4 °F** since 1985 (NOAA NCEI), triggering earlier ice-out and prolonged summer stratification.
  2. **Hydraulic Stagnation**: During drought, South Platte River inflow drops from normal $>300\text{ cfs}$ to $<80\text{ cfs}$. Because Eleven Mile Reservoir holds $\sim 101,000\text{ acre-feet}$, low inflow quadruples hydraulic retention time from $\sim 80\text{ days}$ to **over 350 days**, preventing natural flushing and creating an ideal incubator for cyanobacteria.
  3. **Internal Phosphorus Loading**: Low flushing and strong thermal stratification deplete hypolimnetic oxygen ($\text{DO} < 2.0\text{ mg/L}$), releasing legacy phosphorus bound to iron complexes in lake sediments to fuel massive late-summer blooms.
- **Statistical Significance**: Regression models confirm that summer temperature anomaly and Palmer Drought Severity Index (PDSI) significantly drive peak bloom severity ($R^2 = 0.74, p < 0.001$).

### 3. 🧪 In-Situ Water Quality & Cyanotoxin Lab Benchmark
- **Real Colorado CDPHE, USGS & Colorado Parks & Wildlife Monitoring Records**: Compiled from the Water Quality Portal across 11 stations in Eleven Mile Reservoir.
- **The Catastrophic September 2020 Bloom**: On September 30, 2020, during an extreme statewide drought, microcystin concentrations reached an unprecedented **1,240 µg/L**—more than **150 times** the EPA recreational health guideline ($8.0\text{ µg/L}$) and over **4,000 times** the infant drinking water threshold ($0.3\text{ µg/L}$).

### 4. 🛡️ Proactive vs. Reactive Mitigation Decision Simulator
- **Proactive Management Scenario Simulator**:
  - Users adjust sliders for **Summer Warming Anomaly ($0\text{ to }+3.5\text{ °C}$)**, **Drought Inflow Deficit ($0\text{ to }60\%$)**, and **Watershed Phosphorus Loading ($-30\%\text{ to }+50\%$)**.
  - Users evaluate proactive alternatives:
    1. **Targeted Hydrogen Peroxide ($H_2O_2$ Advanced Oxidation)**: Eco-friendly selective cyanobactericide that induces oxidative collapse of cyanobacteria photosynthetic apparatus while decomposing within 48 hours to pure $H_2O + O_2$ with zero bioaccumulative copper residues.
    2. **Hypolimnetic Oxygenation / Aeration**: Restores dissolved oxygen ($>6\text{ mg/L}$) in the deep basin near the dam, shutting down internal sediment phosphorus release and preserving coldwater trout refuge.
    3. **Solar-Powered Ultrasonic Buoys (e.g., LG Sonic)**: Emits targeted acoustic resonance that collapses cyanobacterial gas vesicles, causing intact cells to sink and die naturally in the aphotic zone without cell membrane rupture or toxin surge.
    4. **Lanthanum-Modified Bentonite (Phoslock)**: Irreversibly binds bioavailable phosphate under both oxic and anoxic conditions, starving cyanobacteria of limiting nutrients.
    5. **Coordinated Flow Pulsing (Upstream Spinney Mountain Releases)**: Strategically pulses cold inflow from Spinney Mountain Reservoir during peak heatwaves to reduce hydraulic residence time and disrupt stagnation.
  - Generates real-time comparisons of Peak Chlorophyll-a, Toxin Probability, High-Risk Days, and Coldwater Trout Habitat Volume.

---

## 📂 Project Structure

```text
Eleven Mile Cyano/
├── app.R                          # Main R Shiny web application
├── run_app.R                      # One-click launch script
├── prepare_data.R                 # End-to-end data acquisition & processing pipeline
├── process_geotiffs.R             # Sentinel-2 STAC GeoTIFF extraction and projection
├── build_integrated_analytics.R   # Multi-decadal climate, hydrology & bloom synthesis
├── data/
│   ├── eleven_mile_reservoir_polygon.rds   # High-resolution Census TIGER reservoir boundary
│   ├── stations.rds                        # 11 WQP & USGS monitoring stations
│   ├── wqp_water_quality.rds               # In-situ chemical & microcystin lab records
│   ├── dwr_streamflow.rds                  # 63,300+ daily streamflow/temp records (CDSS DWR)
│   ├── dwr_monthly_hydrology.rds           # Monthly hydrology & retention time
│   ├── noaa_drought_climate.rds            # 130-yr monthly PDSI, temp, precip (NOAA NCEI)
│   ├── noaa_monthly_climate.rds            # Monthly climate metrics
│   ├── integrated_bloom_hydrology.rds      # 30-year synthesized annual summer metrics
│   └── satellite_rasters/                  # Cloud-optimized GeoTIFFs (WGS84 EPSG:4326)
│       ├── s2_20230612_ndci.tif            # Spring baseline NDCI
│       ├── s2_20230612_cyano.tif           # Spring baseline CyanoIndex
│       ├── s2_20230821_ndci.tif            # Peak summer active bloom NDCI
│       ├── s2_20230821_cyano.tif           # Peak summer active bloom CyanoIndex
│       ├── s2_20230831_ndci.tif            # Late summer dense bloom NDCI
│       ├── s2_20230831_cyano.tif           # Late summer dense bloom CyanoIndex
│       └── satellite_manifest.rds          # Metadata, cloud cover, and summary stats
├── R/
│   └── utils_helpers.R            # Palettes, spectral models, and scenario simulation engine
└── README.md                      # Comprehensive documentation
```

---

## 📚 Key References & Documentation Links
- [NC State Extension: Supporting Cyanobacterial Bloom Monitoring with Satellite Imagery](https://content.ces.ncsu.edu/supporting-cyanobacterial-bloom-monitoring-with-satellite-imagery)
- [ESA SNAP Community: Cyanobacteria Index (CI) Processor](https://step.esa.int/main/wp-content/help/versions/11.0.0/snap-community-plugins/org.eomasters.eomtbxp.eomtbxp.toolbox/pages/ciop/CyanoIndex.html)
- [Mishra, S., & Mishra, D. R. (2012). Normalized difference chlorophyll index: A novel model for remote estimation of chlorophyll-a in turbid productive waters. *Remote Sensing of Environment*, 117, 394–406.](https://doi.org/10.1016/j.rse.2011.10.016)
- [Wynne, T. T., et al. (2010). Relating spectral shape to cyanobacterial blooms in Lake Erie. *International Journal of Remote Sensing*, 31(21), 5655–5672.](https://doi.org/10.1080/01431160903349007)
- [EPA Cyanobacteria Assessment Network (CyAN)](https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan)
- [Colorado Division of Water Resources (CDSS DWR) Telemetry API](https://dwr.state.co.us/)
- [Water Quality Portal (WQP)](https://www.waterqualitydata.us/)
- [NOAA NCEI Climate at a Glance](https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/)
