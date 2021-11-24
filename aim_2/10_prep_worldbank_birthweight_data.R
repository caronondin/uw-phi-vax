# Author: Meg Robertson
# Purpose: Load and prep World Bank birth rate data
# Date: November 15, 2021

# Load libraries and data file
library("tidyr")  
library("dplyr")
dt1 <- read_csv(paste0(raw_data_dir, "index_variables/world_bank/Data_Extract_From_World_Development_Indicators/0cbfa095-9dc1-4270-ba69-af94452a33c9_Data.csv"))

# Remove unnecessary columns
dt1 <- dt1[-c(1,2,4)]

# Transform dataframe from wide to long
dt1_long <- gather(dt1, year, crude_birth_rate, "1990 [YR1990]":"2020 [YR2020]", factor_key=TRUE)

# Rename country column
dt1_long <- rename(dt1_long, "location" = "Country Name")

# Remove brackets from year values
dt1_long$year <- substr(dt1_long$year, 1, 4)

# Reformat year and rate columns
dt1_long$year <- as.numeric(dt1_long$year)
dt1_long$crude_birth_rate <- as.numeric(dt1_long$crude_birth_rate)

# Update data name
birth_rate_dataset <- dt1_long

# Save cleaned dataset in Google Drive folder 
saveRDS(birth_rate_dataset, file = "10_prepped_worldbank__birthrate_data.rds")