# Author: Francisco Rios 
# Purpose: Merge together different variables to create one data set
# Date: Last modified November 15, 2021

# Load all of the prepped data sets
prepped_file_01 <- readRDS(paste0(prepped_data_dir, "aim_2/01_prepped_ihme_dah_data.RDS"))
prepped_file_02 <- readRDS(paste0(prepped_data_dir, "aim_2/02_prepped_ihme_haqi_data.RDS"))
prepped_file_03 <- readRDS(paste0(prepped_data_dir, "aim_2/03_prepped_cpi_data.RDS"))
prepped_file_04 <- readRDS(paste0(prepped_data_dir, "aim_2/04_prepped_un_birth_attendant_data.RDS"))
prepped_file_05 <- readRDS(paste0(prepped_data_dir, "aim_2/05_prepped_un_immigrant_data.RDS"))
prepped_file_06 <- readRDS(paste0(prepped_data_dir, "aim_2/06_prepped_un_net_migr_rate_data.RDS"))
prepped_file_07 <- readRDS(paste0(prepped_data_dir, "aim_2/07_prepped_un_perc_urban_data.RDS"))
prepped_file_08 <- readRDS(paste0(prepped_data_dir, "aim_2/08_prepped_vaccine_confidence_data.RDS"))
prepped_file_09 <- readRDS(paste0(prepped_data_dir, "aim_2/09_prepped_who_ghe_data.RDS"))
prepped_file_10 <- readRDS(paste0(prepped_data_dir, "aim_2/10_prepped_worldbank__primschoolcomplt_data.RDS"))
prepped_file_11 <- readRDS(paste0(prepped_data_dir, "aim_2/11_prepped_worldbank__birthrate_data.RDS"))

# Making 
mergeVars <- c("location", "year", "gbd_location_id", "iso_code", "iso_num_code")
merged_data <- prepped_file_01 %>% 
  full_join(prepped_file_02, by=mergeVars) %>%
  full_join(prepped_file_03, by=mergeVars) %>%
  full_join(prepped_file_04, by=mergeVars) %>%
  full_join(prepped_file_05, by=mergeVars) %>%
  full_join(prepped_file_06, by=mergeVars) %>%
  full_join(prepped_file_07, by=mergeVars) %>% 
  full_join(prepped_file_08, by=mergeVars) %>%
  full_join(prepped_file_09, by=mergeVars) %>%
  full_join(prepped_file_10, by=mergeVars) %>%
  full_join(prepped_file_11, by=mergeVars)

# Join SDI variable
sdi_dat <- readRDS(paste0(prepped_data_dir, "aim_1/02_sdi.RDS"))
sdi_dat <- sdi_dat %>% select(location_id, year_id, sdi)

final_merged_data <- merged_data %>% 
  left_join(sdi_dat, by=c("gbd_location_id"="location_id", "year"="year_id"))

# Save Final Prepped Data
saveRDS(final_merged_data, file = paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))
