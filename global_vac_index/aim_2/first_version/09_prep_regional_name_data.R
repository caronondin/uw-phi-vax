# Author: Francisco Rios 
# Purpose: Prep geographic area variable to help control for regional variation
# Date: Last modified July 12, 2022

# clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

# Load codebooks that will be used to create final codebook
gbd_code <- read_xlsx(paste0(codebook_directory, "IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.xlsx")) # GBD Location Hierarchy

# Subset columns
gbd_code <- gbd_code[,c(2:5)]

# Rename the columns
gbd_code <- rename(gbd_code,
                   parent_id='Parent ID', 
                   level='Level',
                   location=`Location Name`,
                   gbd_location_id=`Location ID`)

# Subset GBD Locations to only countries (level 2) and keep only columns of interest
gbd_code_nations <- gbd_code %>% filter(level %in% c(3))
gbd_code_regions <- gbd_code %>% filter(level %in% c(2)) %>%
  select(gbd_location_id, location)

# rename location variable
gbd_code_regions <- rename(gbd_code_regions,
                           region=location,
                           parent_id=gbd_location_id)

# merge together to create code book
gbd_region_variable <- gbd_code_nations %>% left_join(gbd_code_regions, by="parent_id")

# load location map to add other important variables
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# merge gbd_region_variable with location merge variables
full_data <- gbd_region_variable %>% full_join(location_map, by=c("gbd_location_id", "location"))

# Subset columns of interest
full_data <- full_data %>% select(gbd_location_id, location, iso_code, iso_num_code, region, region)

# save new variable
saveRDS(full_data, file=paste0(prepped_data_dir, "aim_2/08_prepped_regional_name_data.RDS"))
