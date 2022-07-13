# Author: Francisco Rios 
# Purpose: Merge together different variables to create one data set
# Date: Last modified July 12 2022

# clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/second_version/01_set_up_R.R"))

# Load all of the prepped data sets (that will still be used in the final analysis)
prepped_file_01 <- readRDS(paste0(prepped_data_dir, "aim_2/01_prepped_ihme_health_spending_data.RDS"))
prepped_file_02 <- readRDS(paste0(prepped_data_dir, "aim_2/02_prepped_dah_eligibility_data.RDS"))
prepped_file_03 <- readRDS(paste0(prepped_data_dir, "aim_2/03_prepped_ihme_haqi_data.RDS"))
prepped_file_04 <- readRDS(paste0(prepped_data_dir, "aim_2/04_prepped_cpi_data.RDS"))
prepped_file_05 <- readRDS(paste0(prepped_data_dir, "aim_2/05_prepped_un_birth_attendant_data.RDS"))
prepped_file_06 <- readRDS(paste0(prepped_data_dir, "aim_2/06_prepped_un_immigrant_data.RDS"))
prepped_file_07 <- readRDS(paste0(prepped_data_dir, "aim_2/07_prepped_un_perc_urban_data.RDS"))
prepped_file_08 <- readRDS(paste0(prepped_data_dir, "aim_2/08_prepped_regional_name_data.RDS"))
prepped_file_09 <- readRDS(paste0(prepped_data_dir, "aim_2/12_prepped_vaccine_confidence_data.RDS"))

# Making 
mergeVars <- c("location", "year", "gbd_location_id", "iso_code", "iso_num_code")
merged_data <- prepped_file_01 %>% 
  full_join(prepped_file_02, by=mergeVars) %>%
  full_join(prepped_file_03, by=mergeVars) %>%
  full_join(prepped_file_04, by=mergeVars) %>%
  full_join(prepped_file_05, by=mergeVars) %>%
  full_join(prepped_file_06, by=mergeVars) %>%
  full_join(prepped_file_07, by=mergeVars) %>% 
  full_join(prepped_file_08, by=c("location", "gbd_location_id", "iso_code", "iso_num_code")) %>%
  full_join(prepped_file_09, by=mergeVars)

# Join SDI variable
sdi_dat <- readRDS(paste0(prepped_data_dir, "aim_1/02_sdi.RDS"))
sdi_dat <- sdi_dat %>% select(location_id, year_id, sdi)

final_merged_data <- merged_data %>% 
  left_join(sdi_dat, by=c("gbd_location_id"="location_id", "year"="year_id"))

# merge onto a complete country and year map between 1990 and 2020

# Load the location map codebook
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# create a datatable to keep the data frame
years_of_data <- (1990:2020)
years <- matrix(nrow = 204, ncol = 31)
colnames(years) <- years_of_data
years <- data.table(years)

frame <- cbind(location_map, years)
frame <- as.data.table(frame)

frame_long <- melt(frame, id.vars=c('gbd_location_id', 'location', 'iso_code', 'iso_num_code'), variable.factor = FALSE)
frame_long$year <- as.numeric(frame_long$variable)

# Remove blank value from frame
frame_long <- frame_long[,-c(5,6)]

# Use the frame to map existing data
expanded_final_merged_data <- frame_long %>% left_join(final_merged_data, by = mergeVars)

# re arrange variables and save final
expanded_final_merged_data <- expanded_final_merged_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, region, 
                                                                    dah_eligible,
                                                                    sdi, the_per_cap_mean, ghes_per_the_mean, dah_per_cap_ppp_mean, 
                                                                    haqi, cpi, perc_skill_attend, imm_pop_perc, perc_urban,
                                                                    mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective)

# Save Final Prepped Data
saveRDS(expanded_final_merged_data, file = paste0(prepped_data_dir, "aim_2/13_merged_dataset_second_version.RDS"))
