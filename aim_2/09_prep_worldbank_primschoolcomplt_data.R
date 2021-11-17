# Author: Meg Robertson
# Purpose: Load and prep World Bank primary school completion rate data
# Date: November 12, 2021

# Load libraries and data file
library("tidyr")  
dt1 <- read.csv(file = "G:/My Drive/Merck Vaccine Improvement Index Project/Data/raw_data/index_variables/world_bank/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_3163677/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_3163677.csv", skip=4)

# Remove unnecessary columns
dt1 <- dt1[-c(2:4)]

# Transform dataframe from wide to long
dt1_long <- gather(dt1, year, prim_school_complt, X1960:X2020, factor_key=TRUE)

# Remove unnecessary column
dt1_long <- dt1_long[-c(2)]

# Rename country column
dt1_long <- rename(dt1_long, "location" = "Country.Name")

# Remove leading "X" from year values
dt1_long$year <- sub('X', '', dt1_long$year)

# Reformat year and rate columns
dt1_long$year <- as.numeric(dt1_long$year)
dt1_long$prim_school_complt <- as.numeric(dt1_long$prim_school_complt)

# Update data name
prim_school_complt_dataset <- dt1_long

# Save cleaned dataset in Google Drive folder 
saveRDS(prim_school_complt_dataset, file = "09_prepped_worldbank__primschoolcomplt_data.rds")
