# Author: Francisco Rios Casas
# Purpose: Load and prep IHME HAQI data files
# Date: July 12 2022

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables" & data_source=="ihme")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[2], "/", file_list$data_source[2], "/", file_list$containing_folder[2], "/", file_list$file_name[2])

# Load data file
dt1 <- read_csv(file_path)

# subset rows to only include HAQI values
dt1 <- dt1 %>% filter(indicator_name=="Healthcare Access and Quality")

# keep only certain columns
dt1 <- dt1 %>% select(location_id, location_name, year_id, val)

# Load the location name codebook
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS")) 

# Merge to ensure only national-level data is saved
haqi_dataset <- dt1 %>% 
  inner_join(location_map, by=c('location_id'='gbd_location_id'))

# rename the "val" variable
haqi_dataset <- rename(haqi_dataset, 
       haqi = val,
       gbd_location_id = location_id, 
       year = year_id)

# Subset to columns of interest
haqi_dataset <- haqi_dataset %>% select(location, year, gbd_location_id, iso_code, iso_num_code, haqi)

# save the new data set
saveRDS(haqi_dataset, file = paste0(prepped_data_dir, "aim_2/03_prepped_ihme_haqi_data.RDS"))
