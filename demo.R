source("api_utils.R")
source("plot_utils.R")
library(gifski)

print("Generating data for the decade")
state = "CA"
query = "S1903_C02_001E"

decade_df <- generate_puma_df_decade(state, query)
print("Combining decade data with shapefile")
all_years_data <- gather_decade_sf(state, decade_df)
print("Drawing the animation")
plot_puma_map_animated(all_years_data, decade_df, fname = "data_map.gif")