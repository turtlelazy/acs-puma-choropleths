# Check if the user provided the required arguments
args <- commandArgs(trailingOnly = TRUE)

print(length(args))
if (length(args) < 1) {
  stop("Please provide a state abbreviation.\nUsage: Rscript demo.R <state_abbreviation>\nExample: Rscript demo.R NY")
}

state <- args[1]

source("api_utils.R")
source("plot_utils.R")
library(gifski)

if(get_state_number(state) == FALSE) {
  stop("Invalid state abbreviation. Please provide a valid state abbreviation.")
}

query_code_1 = "S1903_C02_001E"
query_code_2 = "S1903_C03_001E"

print(paste0("Retrieve shapefile for ", state))
sf <- get_shapefile(state, 2019)

print("Retrieving decade data points by the year")
decade_df <- generate_puma_df_decade(state, query_code_1, query_code_2)
print("Combining decade data with shapefile")
all_years_data <- gather_decade_sf(state, sf, decade_df)
print("Drawing the animation")
plot_puma_map_animated(all_years_data, decade_df, fname = paste0(state, "_animated_map.gif"), data_point = "Median Income", label = paste0(state," Median Income Graph"), title = paste0(state," Median Income Choropleth Map"))