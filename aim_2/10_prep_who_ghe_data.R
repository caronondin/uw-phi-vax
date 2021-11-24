# Author: Francisco Rios Casas
# Purpose: Load and prep vaccine confidence data (vaccines considered safe)
# Date: November 19, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[9], "/", file_list$data_source[9], "/", file_list$file_name[9])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 1)

# Keep columns of interest
dt2 <- dt1 %>% select(country, `country code`, year, gghed_che, hc62_g_gghed, hc62_che)

# Load location map
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge to make sure names are standardized
dt2 <- dt2 %>% left_join(location_map, by=c("country code"="iso_code"))

# Rename columns of interest
dt2 <- rename(dt2, 
              iso_code=`country code`)

# Keep columns of interest
prepped_ghe_dataset <- dt2 %>% select(location, year, gbd_location_id, iso_code, iso_num_code, gghed_che, hc62_g_gghed, hc62_che)

# Save year variable as numeric
prepped_ghe_dataset$year <- as.numeric(prepped_ghe_dataset$year)

# Save final dataset
saveRDS(prepped_ghe_dataset, paste0(prepped_data_dir, "aim_2/09_prepped_who_ghe_data.RDS"))


