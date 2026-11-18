# run_app.R
# One-click launcher for the Eleven Mile Reservoir Cyanobacteria & Drought Shiny App.
# Run from terminal: Rscript run_app.R
# Or in R / RStudio: source("run_app.R")

message("\n==================================================================")
message("  Starting Eleven Mile Reservoir Cyanobacteria & Climate App")
message("  Open your browser at: http://127.0.0.1:3838")
message("  Press Ctrl+C (or stop in RStudio) to exit")
message("==================================================================\n")

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The 'shiny' package is required. Install it using install.packages('shiny')")
}

shiny::runApp(appDir = ".", port = 3838, host = "127.0.0.1", launch.browser = TRUE)
