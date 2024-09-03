# Load necessary libraries
library(jsonlite)
library(dplyr)

state_data <- read.table("state.txt", sep = "|", header = TRUE)

#' Get State Number
#'
#' This function retrieves the state number corresponding to a two-letter state abbreviation.
#'
#' @param state_abbr A character string representing the two-letter state abbreviation (e.g., "NY" for New York).
#'
#' @return A character string representing the state number, formatted with leading zeros if necessary. 
#' If the state abbreviation is not found, the function returns `FALSE`.
#'
#' @examples
#' get_state_number("NY")
#' get_state_number("CA")
#' #' @export
get_state_number <- function(state_abbr) {
  # Find the row that matches the state abbreviation
  state_row <- state_data[state_data$STUSAB == state_abbr, ]

  # Check if the state abbreviation exists in the data
  if (nrow(state_row) == 0) {
    return(FALSE)
  }

  # Get the state number and format it with leading zero if necessary
  state_number <- sprintf("%02d", as.integer(state_row$STATE))

  return(state_number)
}

#' Default Project Census API URL
#'
#' This function generates the URL to retrieve data from the U.S. Census Bureau API based on state abbreviation, year, and query code.
#'
#' @param state_abbrv A character string representing the two-letter state abbreviation (e.g., "NY" for New York).
#' @param year An integer specifying the year of the data to retrieve (e.g., 2019).
#' @param query_code A character string representing the query code for the data of interest (e.g., "S1903_C03_001E").
#'
#' @return A character string containing the full API URL for the specified state, year, and query code.
#'
#' @examples
#' grab_url("NY", 2019, "S1903_C03_001E")
#' @export

grab_url <- function(state_abbrv, year, query_code) {
  state <- get_state_number(state_abbrv)
  url <- paste0("https://api.census.gov/data/",
    year, "/acs/acs1/subject?get=NAME,",
    query_code, "&for=public%20use%20microdata%20area:*&in=state:"
    , state
  )
  return(url)
}

#' Retrieve PUMA Data for a Given Year
#'
#' This function retrieves Public Use Microdata Area (PUMA) data for a specific state and year, and formats it into a data frame.
#'
#' @param state A character string representing the two-letter state abbreviation (e.g., "NY" for New York).
#' @param year An integer specifying the year of the data to retrieve (e.g., 2019).
#' @param query_code A character string representing the query code for the data of interest (e.g., "S1903_C03_001E").
#' @param fname A character string specifying the filename to save the data frame as a CSV file. If empty, the data frame is not saved.
#'
#' @return A data frame containing the PUMA data for the specified state and year. The column name is updated to reflect the year.
#'
#' @examples
#' grab_puma_df_by_year("NY", 2019, "S1903_C03_001E")
#' #' @export

grab_puma_df_by_year <- function(state, year, query_code, fname = "") {
  # Fetch and format data into a df
  url <- grab_url(state, year, query_code)
  api_response <- fromJSON(url)

  # Convert the API response to a dataframe
  df <- as.data.frame(api_response[-1, ], stringsAsFactors = FALSE)

  colnames(df) <- api_response[1, ] # Set the column names

  # Set "public use microdata area" as the row names
  rownames(df) <- df$`public use microdata area`

  # Extract only the S1903_C03_001E column and rename it with the year
  df_year <- df[, query_code, drop = FALSE]

  df_year[, 1] <- as.integer(df_year[, 1]) # Convert the column to integer

  colnames(df_year) <- paste0("data_", year)

  # Print Year df info of the data

  if (fname != "") {
    write.csv(df_year, fname, row.names = TRUE)
  }
  return(df_year)
}

#' Generate PUMA Data for a Decade
#'
#' This function retrieves and combines PUMA data for a specific state over a decade (2012 to 2021), excluding 2020, and merges the data into a single data frame.
#'
#' @param state A character string representing the two-letter state abbreviation (e.g., "NY" for New York).
#' @param query_code_1 A character string representing the query code for data from years 2012 to 2016.
#' @param query_code_2 A character string representing the query code for data from years 2017 to 2021.
#' @param fname A character string specifying the filename to save the combined data frame as a CSV file. If empty, the data frame is not saved.
#' @param progress A logical value indicating whether to display a progress bar (default is TRUE).
#'
#' @return A data frame containing the combined PUMA data for the specified state over the decade.
#'
#' @examples
#' generate_puma_df_decade("NY", "S1903_C02_001E", "S1903_C03_001E")
#' #' @export

generate_puma_df_decade <- function(state, query_code_1, query_code_2, fname = "", progress = TRUE) {
  final_df <- NULL
  skips <- c(2020)
  curr <- 0
  
  pb <- if (progress) txtProgressBar(min = 2012, max = 2021, style = 3) else NULL
  
  for (year in 2012:2021) {
    if (year %in% skips) next

    df_year <- grab_puma_df_by_year(state, year, if(year>= 2017) query_code_2 else query_code_1)

    if (is.null(final_df)) {
      final_df <- df_year
    } else {
      final_df <- merge(final_df, df_year, by = "row.names", all = TRUE)
      rownames(final_df) <- final_df$Row.names # Reassign row names
      final_df$Row.names <- NULL # Remove the auxiliary column
    }
    if (progress) setTxtProgressBar(pb, year)
  }
  if (progress) close(pb)

  if (fname != "") {
    write.csv(final_df, fname, row.names = TRUE)
  }
  return(final_df)
}