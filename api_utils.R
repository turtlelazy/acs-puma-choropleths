# Load necessary libraries
library(jsonlite)
library(dplyr)

state_data <- read.table("state.txt", sep = "|", header = TRUE)
get_state_number <- function(state_abbr) {
  # Find the row that matches the state abbreviation
  state_row <- state_data[state_data$STUSAB == state_abbr, ]

  # Check if the state abbreviation exists in the data
  if (nrow(state_row) == 0) {
    return(paste("State abbreviation", state_abbr, "not found"))
  }

  # Get the state number and format it with leading zero if necessary
  state_number <- sprintf("%02d", as.integer(state_row$STATE))

  return(state_number)
}


grab_url <- function(state_abbrv, year, query_code) {
  state <- get_state_number(state_abbrv)
  url <- paste0("https://api.census.gov/data/",
    year, "/acs/acs1/subject?get=NAME,",
    query_code, "&for=public%20use%20microdata%20area:*&in=state:"
    , state
  )
  return(url)
}

# Initialize an empty list to store dataframes for each year
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

# Later print a progress bar
generate_puma_df_decade <- function(state, query_code, fname = "") {
  final_df <- NULL
  skips <- c(2020)
  for (year in 2012:2021) {
    if (year %in% skips) next

    df_year <- grab_puma_df_by_year(state, year, query_code)

    if (is.null(final_df)) {
      final_df <- df_year
    } else {
      final_df <- merge(final_df, df_year, by = "row.names", all = TRUE)
      rownames(final_df) <- final_df$Row.names # Reassign row names
      final_df$Row.names <- NULL # Remove the auxiliary column
    }
  }
  if (fname != "") {
    write.csv(final_df, fname, row.names = TRUE)
  }
  return(final_df)
}