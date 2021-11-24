# Author: Meg Robertson
# Purpose: Load and prep World Bank primary school completion rate data
# Date: November 12, 2021

# Load libraries and data file
library("tidyr")  
dt1 <- read.csv(file = paste0(raw_data_dir,"index_variables/world_bank/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_3163677/API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_3163677.csv"), skip=4)

# Remove unnecessary columns
dt1 <- dt1[-c(3:4)]

# Transform dataframe from wide to long
dt1_long <- gather(dt1, year, prim_school_complt, X1960:X2020, factor_key=TRUE)

# Remove unnecessary column
dt1_long <- dt1_long[-c(3)]

# Rename country column
dt1_long <- rename(dt1_long, "country_name" = "Country.Name",
                   "iso_code" = "Country.Code")

# Remove leading "X" from year values
dt1_long$year <- sub('X', '', dt1_long$year)

# Reformat year and rate columns
dt1_long$year <- as.numeric(dt1_long$year)
dt1_long$prim_school_complt <- as.numeric(dt1_long$prim_school_complt)

# Load location map to merge standard place names
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge to make sure names are standardized
dt1_long <- dt1_long %>% right_join(location_map, by=c("iso_code"))

# Update data name and keep select variables
prim_school_complt_dataset <- dt1_long %>% select(location, year, gbd_location_id, iso_code, iso_num_code, prim_school_complt)

# Save cleaned dataset in Google Drive folder 
saveRDS(prim_school_complt_dataset, file = paste0(prepped_data_dir, "aim_2/10_prepped_worldbank__primschoolcomplt_data.rds"))
