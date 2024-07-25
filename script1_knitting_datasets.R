library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)

###########################################################################################################################################################################################################################################

dfs <- list()

# Define vector of years
years <- c(2009:2022)
for (year in years) {
  sheet_year <- paste0(year)
  raw_data <- read_excel("C:/Users/t.tu/Centre for Cities/Centre For Cities POC - Documents/Research/Manufacturing Brexit/Raw Data/nomis_ind.xlsx", sheet = sheet_year)
  
  # Extract industry names
  industry_columns <- colnames(raw_data)[-c(1:3)]
  
  # Rename columns with years  
  data_with_year <- raw_data %>%
    rename_with(~paste0(., "_", year), -c(PUA, la_code, la))
  
  collapsed_data <- data_with_year %>%
    group_by(PUA) %>%
    summarize(across(-c(la_code, la), list(total = sum), na.rm = TRUE), .groups = 'drop')

  
  # Add yearly data frame to the list
  dfs[[length(dfs) + 1]] <- collapsed_data
}

# Perform left join on all data frames by PUA
final_df <- Reduce(function(x, y) left_join(x, y, by = "PUA"), dfs)

###########################################################################################################################################################################################################################################

# Create a list to store industry-specific data frames
industry_data_list <- list()

for (i in seq_along(industry_columns)) {
  # Get the industry name
  industry_name <- industry_columns[i]
  
  # Find columns in final_df that match the industry name
  industry_column <- grep(industry_name, names(final_df), value = TRUE)
  
  # Subset final_df using the matched columns
  industry_data <- final_df[, c("PUA", industry_column)]
  
  # Rename the columns to just the year
  colnames(industry_data)[-1] <- sub(".*_(\\d{4})_total", "\\1", colnames(industry_data)[-1])
  
  # Convert columns to numeric
  industry_data[ , -1] <- lapply(industry_data[ , -1], as.numeric)
  
  # Make data into long format
  industry_data_long <- industry_data %>% 
    pivot_longer(
      cols = -PUA, 
      names_to = "year",
      values_to = "employment"
    )
  
  # Ensure the 'year' column is numeric
  industry_data_long <- industry_data_long %>%
    mutate(year = as.numeric(year),
           employment = as.numeric(employment))
  
  # Calculate year-on-year growth rate
  industry_data_long <- industry_data_long %>%
    group_by(PUA) %>%
    arrange(year) %>%
    mutate(growth_rate = (employment - lag(employment)) / lag(employment) * 100) %>%
    ungroup()
  
  # Store the subsetted data frame in the list
  industry_data_list[[industry_name]] <- industry_data_long
}

###########################################################################################################################################################################################################################################

# Calculate summary statistics for growth rates for each industry and PUA
growth_summary_list <- lapply(names(industry_data_list), function(industry_name) {
  industry_data_long <- industry_data_list[[industry_name]]
  
  summary_stats <- industry_data_long %>%
    group_by(PUA) %>%
    summarise(
      avg_growth_pre_2016 = mean(growth_rate[year < 2016], na.rm = TRUE),
      avg_growth_post_2016 = mean(growth_rate[year >= 2016], na.rm = TRUE),
      change_in_growth = avg_growth_post_2016 - avg_growth_pre_2016
    ) %>%
    mutate(industry = industry_name)
  
  return(summary_stats)
})

# Combine all summary statistics into a single data frame
growth_summary <- bind_rows(growth_summary_list)

# Calculate the average change in growth for each industry
industry_change_summary <- growth_summary %>%
  group_by(industry) %>%
  summarise(avg_change_in_growth = mean(change_in_growth, na.rm = TRUE))

###########################################################################################################################################################################################################################################

# Export to Excel

# Create a new workbook
wb <- createWorkbook()

# Loop through the list of industry data frames and add each to a new sheet
for (industry_name in names(industry_data_list)) {
  # Add a new sheet for the industry
  addWorksheet(wb, industry_name)
  
  # Write the industry data frame to the sheet
  writeData(wb, sheet = industry_name, industry_data_list[[industry_name]])
}

# Add a sheet for the growth summary
addWorksheet(wb, "Growth_Summary")
writeData(wb, sheet = "Growth_Summary", growth_summary)

# Save the workbook
saveWorkbook(wb, "industry_growth", overwrite = TRUE)
###########################################################################################################################################################################################################################################

#Import population data












###########################################################################################################################################################################################################################################
## Data Visualisations
# Create a bar chart of the average change in growth for each industry
ggplot(industry_change_summary, aes(x = reorder(industry, avg_change_in_growth), y = avg_change_in_growth)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Change in Growth Rates Before and After Brexit by Industry",
       x = "Industry",
       y = "Average Change in Growth (%)") +
  theme_minimal()

###########################################################################################################################################################################################################################################

