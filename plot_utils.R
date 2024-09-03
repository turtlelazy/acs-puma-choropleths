
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)

get_shapefile <- function(state, year) {
  # Get the PUMA boundaries for the specified state and year
  return(tigris::pumas(state, cb = TRUE, year = year))
}

plot_puma_map <- function(
  sf,
  df,
  puma_str = "PUMACE10",
  fname = "",
  colors = c("red", "green", "blue"),
  label="Data Graph",
  title="Data Map"
) {
  # Get the PUMA boundaries for the specified state and year
  # Plot the map with ggplot2
  data_columns <- df %>%
    select(starts_with("data_"))
  
  data_values <- unlist(data_columns, use.names = FALSE)
  mean_val <- mean(data_values, na.rm = TRUE)
  sd_val <- sd(data_values, na.rm = TRUE)
  min_value <- min(data_values)
  max_value <- max(data_values)

  df[puma_str] <- rownames(df)
  rownames(df) <- NULL

  # Flatten the data into a single vector

  # Create a ggplot object
  selected_data_column <- paste0("data_", year)

  # Merge the data on the common PUMA column
  merged_data_map <- sf %>%
    left_join(df, by = puma_str)

    # Plot the shapefile with shading intensity
  ggplot(data = merged_data_map) +
    geom_sf(aes_string(fill = selected_data_column), color = "darkblue") +
    scale_fill_gradientn(
      colors = colors, # Gradient from red to green to blue
      values = scales::rescale(c(min(data_values, na.rm = TRUE), mean_val, max(data_values, na.rm = TRUE))),
      na.value = "grey", # Color for NA values
      limits = c(min_value, max_value), # Fix the color scale limits

      name = paste("Median Income (", year, ")", sep = "")
    ) +

    # scale_fill_gradient(low = "lightblue", high = "darkblue",
    #                     na.value = "red", name = paste("Median Income (", year, ")", sep = ""),   limits = c(min_value, max_value)) +
    theme(
      panel.background = element_rect(fill = "lightgray", color = NA), # Set panel background color
      plot.background = element_rect(fill = "lightblue", color = NA) # Set plot background color
    ) +
    labs(
      title = title,
      subtitle = paste(label, sep = "")
    )

  # Save the plot to a file
  if (fname != "") {
    ggsave(fname, plot = last_plot(), create.dir = TRUE)
  }

}
