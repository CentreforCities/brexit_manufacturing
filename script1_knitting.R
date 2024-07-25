library(labelled)
library(tidyverse)
library(openxlsx)
library(data.table)
library(readxl)
library(dplyr)

# Bring in employment for manufacturing raw data
path <- paste0(dirname("~"), "/Centre for Cities/Centre for Cities POC - Documents/Research/Business and Enterprise/Manufacturing Brexit/Raw Data", collapse = NULL)
setwd(path)

# Read in PUA lookup 
pua_lookup <- read_excel("2024-06_all_LAs_into_LA23_into_CfC_geogs.xlsx" , sheet='LA')

# Subset PUA
pua_lookup <- pua_lookup[, c('LA Code 2023', 'PUA')]
colnames(pua_lookup)[1] <- 'LA_Code'

# Reading in each sheet that has been named by year
dfs <- list()
years <- c(2009:2015)

for (year in years) {
  sheet_year <- paste0(year)
  
  # Read the first two rows separately
  col_names <- read_excel("nomis_ind_raw.xlsx", sheet = sheet_year, range = cell_rows(1), col_names = FALSE)
  col_labels <- read_excel("nomis_ind_raw.xlsx", sheet = sheet_year, range = cell_rows(2), col_names = FALSE)
  
  # Read the rest of the data, skipping the first two rows
  raw_data <- read_excel("nomis_ind_raw.xlsx", sheet = sheet_year, skip = 2)
  
  # Set the column names
  colnames(raw_data) <- col_names[1, ]
  
  # Set the column labels
  labels <- setNames(as.list(col_labels[1, ]), colnames(raw_data))
  raw_data <- set_variable_labels(raw_data, .labels = labels)
  
  # Merge with PUA lookup
  data_with_year <- merge(raw_data, pua_lookup, by = c("LA_Code"))       
  
  # Collapse the data by PUA
  collapsed_data <- data_with_year %>%
    group_by(PUA) %>%
    summarize(across(-c('LA_Code', 'LA_Name'), sum, na.rm = TRUE), .groups = 'drop')
  
  # Rename columns with years
  collapsed_data <- collapsed_data %>%
    rename_with(~paste0(., "_", year), -c('PUA'))
  
  # Add yearly data frame to the list
  dfs[[length(dfs) + 1]] <- collapsed_data
}

# Combine all the yearly data frames into one
joined_data <- reduce(dfs, full_join, by = "PUA")

# Save the combined data to a file
write.csv(joined_data, "joined_data.csv", row.names = FALSE)
