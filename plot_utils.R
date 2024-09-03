library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(gganimate)
library(magick)
library(gifski)


#' Get PUMA Shapefile
#'
#' Retrieves the Public Use Microdata Area (PUMA) shapefile for a specified state and year.
#'
#' @param state A character string representing the two-letter state abbreviation (e.g., "NY" for New York).
#' @param year An integer specifying the year for which the PUMA boundaries are needed.
#'
#' @return An `sf` object containing the PUMA boundaries for the specified state and year.
#'
#' @examples
#' sf_data <- get_shapefile("NY", 2019)
#' @export
get_shapefile <- function(state, year) {
  # Get the PUMA boundaries for the specified state and year
  return(tigris::pumas(state, cb = TRUE, year = year))
}

#' Plot PUMA Map
#'
#' Generates a map of PUMA regions with color shading based on specified data.
#'
#' @param sf An `sf` object containing the PUMA shapefile.
#' @param df A data frame containing the data to be visualized, with columns for each year.
#' @param puma_str A character string representing the column name that contains PUMA identifiers (default is "PUMACE10").
#' @param fname A character string specifying the filename to save the plot as an image. If empty, the plot is not saved.
#' @param colors A vector of colors for the gradient used in the plot (default is c("red", "green", "blue")).
#' @param label A character string for the plot label (default is "Data Graph").
#' @param title A character string for the plot title (default is "Data Map").
#'
#' @return A ggplot object representing the PUMA map with the specified data.
#'
#' @examples
#' plot_puma_map(sf, df, fname = "puma_map.png")
#' @export
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
  data_columns <- df %>% select(starts_with("data_"))
  
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

  # Plot the shapefile with shading intensity
  ggplot(data = merged_data_map) +
    geom_sf(aes_string(fill = selected_data_column), color = "darkblue") +
    scale_fill_gradientn(
      colors = colors, # Gradient from red to green to blue
      values = scales::rescale(c(min(data_values, na.rm = TRUE), mean_val, max(data_values, na.rm = TRUE))),
      na.value = "grey", # Color for NA values
      limits = c(min_value, max_value), # Fix the color scale limits

      name = paste("Data (", year, ")", sep = "")
    ) +

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

#' Gather PUMA Data for a Decade
#'
#' Combines PUMA data across multiple years into a single list of `sf` objects.
#'
#' @param state A character string representing the two-letter state abbreviation (e.g., "NY" for New York).
#' @param sf An `sf` object containing the PUMA shapefile.
#' @param decade_df A data frame containing data for multiple years, with columns named by year (e.g., "data_2019").
#' @param fname A character string specifying the filename to save the combined data as a CSV file. If empty, the data is not saved.
#' @param puma_str A character string representing the column name that contains PUMA identifiers (default is "PUMACE10").
#'
#' @return A list of `sf` objects, each containing PUMA data for a specific year.
#'
#' @examples
#' all_years_data <- gather_decade_sf("NY", sf, decade_df)
#' @export

gather_decade_sf <- function(state, sf,decade_df, fname="", puma_str = "PUMACE10") {
  all_years_data <- list()
  decade_sf <- sf
  decade_df[puma_str] <- rownames(decade_df)

  for(year in 2012:2021) {
    if (year == 2020) next  # Skip 2020 as per your original code

    rownames(decade_df) <- NULL
    data_column <- paste0("data_", year)
    merged_data <- decade_sf %>% left_join(decade_df, by = puma_str) %>% mutate(Year = year, data = .data[[data_column]]) 
    all_years_data[[as.character(year)]] <- merged_data
  }
  return(all_years_data)
}

#' Plot Animated PUMA Map
#'
#' Generates an animated map showing changes in PUMA data across multiple years.
#'
#' @param all_years_data A list of `sf` objects containing PUMA data for each year.
#' @param decade_df A data frame containing data for multiple years, with columns named by year (e.g., "data_2019").
#' @param puma_str A character string representing the column name that contains PUMA identifiers (default is "PUMACE10").
#' @param fname A character string specifying the filename to save the animation as a GIF. If empty, the animation is not saved.
#' @param colors A vector of colors for the gradient used in the animation (default is c("red", "green", "blue")).
#' @param data_point A character string representing the data label for the animation (default is "Data").
#' @param label A character string for the animation label (default is "Data Graph").
#' @param title A character string for the animation title (default is "Data Map").
#'
#' @return An animation object (from `gganimate`) showing the PUMA map changing over time.
#'
#' @examples
#' anim <- plot_puma_map_animated(all_years_data, decade_df, fname = "puma_map.gif")
#' @export

plot_puma_map_animated <- function(
  all_years_data,
  decade_df,
  puma_str = "PUMACE10",
  fname = "",
  colors = c("red", "green", "blue"),
  data_point = "Data",
  label="Data Graph",
  title="Data Map" 
) 
{
  # Get the PUMA boundaries for the specified state and year
  # Plot the map with ggplot2
  combined_data <- bind_rows(all_years_data)

  data_columns <- decade_df %>% select(starts_with("data_"))

  data_values <- unlist(data_columns, use.names = FALSE)
  mean_val <- mean(data_values, na.rm = TRUE)
  sd_val <- sd(data_values, na.rm = TRUE)
  min_value <- min(data_values)
  max_value <- max(data_values)

  decade_df[puma_str] <- rownames(decade_df)
  rownames(decade_df) <- NULL

  # Plot the shapefile with shading intensity
  # Create the ggplot object
  p <- ggplot(data = combined_data) + 
    geom_sf(aes(fill = data), color = "darkblue") +
    scale_fill_gradientn(
      colors = c("red", "white", "green"),
      values = scales::rescale(c(min_value, mean_val, max_value)),
      na.value = "grey",
      limits = c(min_value, max_value),
      name = data_point
    ) + 
    theme_void() +
    theme(
      panel.background = element_rect(fill = "lightgray", color = NA),
      plot.background = element_rect(fill = "lightblue", color = NA)
    ) +
    labs(title = title,
        subtitle = paste0("Shading Intensity Based on ", data_point," ({closest_state})")) +
    transition_states(Year, transition_length = 2, state_length = 1) +
    ease_aes('linear')

  # Animate and save the plot
  anim <- animate(p, nframes = 100, fps = 10, renderer = gifski_renderer(fname))
  return (anim)
}