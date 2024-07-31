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
pua_lookup <- read_excel("2024-06_all_LAs_into_LA23_into_CfC_geogs.xlsx", sheet = 'LA')

# Subset PUA
pua_lookup <- pua_lookup[, c('LA Code 2023', 'PUA')]
colnames(pua_lookup)[1] <- 'LA_Code'

# Initialize list for data frames
dfs <- list()

# Read each sheet named by year
years <- c(2009:2022)

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
  
  # Add ind_01 as the sum of all other industries
  collapsed_data <- collapsed_data %>%
    mutate(ind_01 = rowSums(across(starts_with("ind_")), na.rm = TRUE))
  
  # Rename columns with years
  collapsed_data <- collapsed_data %>%
    rename_with(~paste0(., "_", year), -c('PUA'))
  
  # Add yearly data frame to the list
  dfs[[length(dfs) + 1]] <- collapsed_data
}

# Combine all the yearly data frames into one
joined_data <- reduce(dfs, full_join, by = "PUA")

# Transform the dataset from wide to long format
long_data <- joined_data %>%
  pivot_longer(
    cols = starts_with("ind_"),
    names_to = c("industry_code", "year"),
    names_pattern = "ind_(\\d+)_(\\d+)",
    values_to = "employment"
  ) %>%
  mutate(industry_code = paste0("ind_", industry_code))

# Create a new PUA called "England" that sums every industry across all PUAs
england_data <- long_data %>%
  group_by(industry_code, year) %>%
  summarise(employment = sum(employment, na.rm = TRUE)) %>%
  mutate(PUA = "England")

# Combine England data with the original long_data
long_data <- bind_rows(long_data, england_data)

# Import population data for industry change adjustment
population_data <- read_excel("2024-05-30_population.xlsx", sheet = 'population_PUA')

# Transform the population data to long format
long_population_data <- population_data %>%
  pivot_longer(
    cols = starts_with("Population_"),
    names_to = "year",
    names_prefix = "Population_",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(year))

# Create population data for England by summing the population for all PUAs
england_population <- long_population_data %>%
  group_by(year) %>%
  summarise(population = sum(population, na.rm = TRUE)) %>%
  mutate(PUA = "England")

# Combine England population data with the original population data
long_population_data <- bind_rows(long_population_data, england_population)

# Convert the year column in long_data to numeric
long_data <- long_data %>%
  mutate(year = as.numeric(year))

# Merge population data with long_data
long_data <- long_data %>%
  left_join(long_population_data, by = c("PUA", "year"))

# Ensure that employment and population columns are numeric
long_data <- long_data %>%
  mutate(
    employment = as.numeric(employment),
    population = as.numeric(population)
  )

# Adjust employment by population and scale by 10,000
long_data <- long_data %>%
  mutate(employment_per_10000_capita = (employment / population) * 10000)

# Replace NA, Inf, and -Inf values with empty strings
long_data <- long_data %>%
  mutate(
    employment_per_10000_capita = ifelse(is.finite(employment_per_10000_capita), employment_per_10000_capita, "")
  )

# Analysis for pre and post Brexit growth rates
# Filter data into pre-Brexit (up to 2015) and post-Brexit (from 2016 onwards)
pre_brexit_data <- long_data %>%
  filter(year < 2016)

post_brexit_data <- long_data %>%
  filter(year >= 2016)

# Calculate the average annual growth rate for pre-Brexit period
pre_brexit_growth <- pre_brexit_data %>%
  group_by(PUA, industry_code) %>%
  arrange(PUA, industry_code, year) %>%
  mutate(lag_employment_per_10000_capita = lag(employment_per_10000_capita)) %>%
  filter(!is.na(lag_employment_per_10000_capita) & employment_per_10000_capita != "") %>%
  mutate(growth_rate = ((as.numeric(employment_per_10000_capita) - as.numeric(lag_employment_per_10000_capita)) / as.numeric(lag_employment_per_10000_capita)) * 100) %>%
  summarise(avg_growth_rate_pre = mean(growth_rate, na.rm = TRUE))

# Calculate the average annual growth rate for post-Brexit period
post_brexit_growth <- post_brexit_data %>%
  group_by(PUA, industry_code) %>%
  arrange(PUA, industry_code, year) %>%
  mutate(lag_employment_per_10000_capita = lag(employment_per_10000_capita)) %>%
  filter(!is.na(lag_employment_per_10000_capita) & employment_per_10000_capita != "") %>%
  mutate(growth_rate = ((as.numeric(employment_per_10000_capita) - as.numeric(lag_employment_per_10000_capita)) / as.numeric(lag_employment_per_10000_capita)) * 100) %>%
  summarise(avg_growth_rate_post = mean(growth_rate, na.rm = TRUE))

# Combine the pre and post Brexit growth rates
combined_growth <- merge(pre_brexit_growth, post_brexit_growth, by = c("PUA", "industry_code"))

# Calculate the difference between pre and post Brexit growth rates
combined_growth <- combined_growth %>%
  mutate(difference = avg_growth_rate_post - avg_growth_rate_pre)

# Replace NA, Inf, and -Inf values with empty strings in combined_growth
combined_growth <- combined_growth %>%
  mutate(
    avg_growth_rate_pre = ifelse(is.finite(avg_growth_rate_pre), avg_growth_rate_pre, ""),
    avg_growth_rate_post = ifelse(is.finite(avg_growth_rate_post), avg_growth_rate_post, ""),
    difference = ifelse(is.finite(difference), difference, "")
  )

# Calculate the share of each industry's employment for each PUA relative to the total employment of "England" for each year
# Filter the data for "England" only
england_total <- long_data %>%
  filter(PUA == "England") %>%
  select(year, industry_code, employment) %>%
  rename(england_employment = employment)

# Merge England's employment totals with the original data
long_data <- long_data %>%
  left_join(england_total, by = c("year", "industry_code"), suffix = c("", "_england")) %>%
  mutate(share = (employment / england_employment) * 100) # Convert share to percentage

# Calculate average share before and after Brexit for each PUA and industry
pre_brexit_share <- long_data %>%
  filter(year < 2016 & PUA != "England") %>%
  group_by(PUA, industry_code) %>%
  summarise(pre_share = mean(share, na.rm = TRUE))

post_brexit_share <- long_data %>%
  filter(year >= 2016 & PUA != "England") %>%
  group_by(PUA, industry_code) %>%
  summarise(post_share = mean(share, na.rm = TRUE))

# Combine the pre and post Brexit shares and calculate share change
share_change <- merge(pre_brexit_share, post_brexit_share, by = c("PUA", "industry_code")) %>%
  mutate(share_change = post_share - pre_share)

# Merge share change data with combined growth data
final_data <- merge(combined_growth, share_change, by = c("PUA", "industry_code"), all.x = TRUE)

# Read in the industry label file
industry_labels <- read_excel("ind_labels.xlsx")

# Convert the industry_code column in the industry_labels to character
industry_labels <- industry_labels %>%
  mutate(industry_code = as.character(industry_code))

# Ensure the industry_code column in the final data is character
final_data <- final_data %>%
  mutate(industry_code = as.character(industry_code))

# Merge the industry labels with the final dataset
final_data <- final_data %>%
  left_join(industry_labels, by = "industry_code")

path <- paste0(dirname("~"), "/Centre for Cities/Centre for Cities POC - Documents/Research/Business and Enterprise/Manufacturing Brexit/Outputs/", collapse = NULL)
setwd(path)


# Save the final data with labels to a file
write.csv(final_data, "final_data.csv", row.names = FALSE)

# Display the first few rows of the final data with labels
head(final_data)

#loading packages

library(readxl)
library(openxlsx)
library(sf)
library(dplyr)

#setting working directory

dirpath <- paste0(dirname("~"), "Centre for Cities/Centre For Cities POC - Documents/Research/Business and Enterprise/Manufacturing Brexit/Raw Data")
setwd(dirpath)

# reading in GPKGs and renaming for merging -------------------------------------------------------------------

#read in GPKGs

GPKG <- read_sf("C:/Users/t.tu/Centre for Cities/Centre For Cities POC - Documents/Research/Core Data/GIS/PUAs/2019 PUAs/Polygons/Clipped/Dissolved/EW/2023-12-01_PUA_Clip_Dis_EW.gpkg")

# reading in data -----------------------------------------------------------------------------------

data <- read.csv("final_data.csv")

# merging data with GPKGs ---------------------------------------------------------------------------

Geo_data <- merge(data, GPKG, by.x = "PUA", by.y = "PUA", all.x = TRUE)

# writing GPKGs for mapping

Geo_data <- write_sf(Geo_data, "C:/Users/t.tu/Centre for Cities/Centre For Cities POC - Documents/Research/Business and Enterprise/Manufacturing Brexit/Outputs/2024_07_31_Geo_manufacturing.gpkg")


