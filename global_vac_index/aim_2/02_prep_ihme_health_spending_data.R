# Author: Francisco Rios Casas
# Purpose: Load and prep IHME DAH data files and health spending files
# Date: December 27, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>%
  filter(data_type=="index_variables" & data_source=="ihme")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[3], "/", file_list$data_source[3], "/", file_list$containing_folder[3], "/", file_list$file_name[3])

# Load data file
dt1 <- read_csv(file_path, show_col_types = FALSE)

# Subset rows
dt1 <- dt1 %>% filter(level=="Country")

# Subset columns
dt1 <- dt1 %>% select(location_id, location_name, iso3, year, the_per_cap_mean, ghes_per_the_mean, dah_per_the_mean, dah_per_cap_ppp_mean)

# # rename variables for merging
dt1 <- rename(dt1,
              gbd_location_id=`location_id`,
              iso_code=`iso3`,
              country=`location_name`)

# Load location map to merge location names and iso numeric code
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge onto file
merged_data <- dt1 %>% left_join(location_map, by=c("gbd_location_id", "iso_code"))

# Select columns to keep
final_data <- merged_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, the_per_cap_mean, ghes_per_the_mean, dah_per_the_mean, dah_per_cap_ppp_mean)

# Save data
saveRDS(final_data, file=paste0(prepped_data_dir, "aim_2/01_prepped_ihme_health_spending_data.RDS"))

# Part II: Create list of non-eligible countries
dt2 <- as.data.table(final_data)

# check to see which countries have never received any funds
zero_funds <- dt2[,.(sum_received=sum(dah_per_the_mean, na.rm=TRUE)),
                          by=c("iso_code", "location", "gbd_location_id",
                               "iso_num_code")] %>% filter(sum_received==0)

# load two different ways of calculating eligibility: Global Fund and World Bank
gf_elibility_list <- readRDS(paste0(codebook_directory, "gf_eligible_locations_map.RDS"))
world_bank_groups <- readRDS(paste0(codebook_directory, "world_bank_income_groups.RDS"))

# Merge additional columns indicating eligibility
mergeVars <- c("iso_code", "location", "gbd_location_id", "iso_num_code")
subset <- zero_funds %>% left_join(gf_elibility_list, by=mergeVars) %>%
  left_join(world_bank_groups, by=mergeVars)

# re-code missing in global fund eligibility
subset[is.na(eligible_per_gf), eligible_per_gf:=0]

# Crete vector of ineligible locations
ineligible <- subset[which(subset$eligible_per_gf==0)]$location

# Save object to be used in later analysis
saveRDS(ineligible, file=paste0(codebook_directory, "locations_ineligible_for_dah.RDS"))
