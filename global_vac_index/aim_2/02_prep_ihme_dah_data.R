# Author: Francisco Rios Casas
# Purpose: Load and prep IHME DAH data files and health spending files
# Date: October 29, 2021

# Section 1: Load and organize the data -----

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables" & data_source=="ihme")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[1], "/", file_list$data_source[1], "/", file_list$containing_folder[1], "/", file_list$file_name[1])

# Load data file
dt1 <- read_csv(file_path, show_col_types = FALSE)

# Drop certain rows to avoid double counting
dt1 <- dt1 %>% filter(elim_ch==0)

# Subset columns
dt1 <- dt1 %>% select(year, source, recipient_isocode, recipient_country, gbd_location_id, nch_cnv_dah_20)

# replace missing values with NA
dt1$nch_cnv_dah_20[dt1$nch_cnv_dah_20=="-"] <- NA

# replace negative values with zero
dt1$nch_cnv_dah_20[dt1$nch_cnv_dah_20<0] <- 0

# Transform column from character to numeric
dt1$nch_cnv_dah_20 <- as.numeric(dt1$nch_cnv_dah_20)

# sum donations across all sources
dt1 <- as.data.table(dt1)
dt1 <- dt1[, .(nch_cnv_dah_20=sum(nch_cnv_dah_20, na.rm = T)), by=c('year', 'recipient_isocode', 'recipient_country', 'gbd_location_id')]

# remove rows without meaningful data
dt1 <- dt1 %>% filter(!is.na(recipient_country)) %>% 
  filter(recipient_country!="Unallocated/Unspecified") %>%
  filter(recipient_country!="Administrative expenses")

# Section 2: Create table frame to identify gaps -----

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
dah_dataset <- frame_long %>% left_join(dt1, by = c("gbd_location_id", "year"))

# Subset final columns
dah_dataset <- dah_dataset %>% select(location, year, gbd_location_id, iso_code, iso_num_code, nch_cnv_dah_20)

# Replace NAs with 0
# dah_dataset[is.na(nch_cnv_dah_20), nch_cnv_dah_20:=0]

# Find interquartile cutoffs
# hist(dah_dataset$nch_cnv_dah_20)

# Part III: Identify countries not eligible to receive donations -----

# check to see which countries have never received any funds
zero_funds <- dah_dataset[,.(sum_received=sum(nch_cnv_dah_20, na.rm=TRUE)),
                          by=c("iso_code", "gbd_location_id", 
                               "location", "iso_num_code")]

# Create table to identify which countries that did not receive funds should have been eligible
zero_funds <- zero_funds %>% filter(sum_received==0) %>% filter(!is.na(location))

# load two different ways of calculating eligibility: Global Fund and World Bank
gf_elibility_list <- readRDS(paste0(codebook_directory, "gf_eligible_locations_map.RDS"))
world_bank_groups <- readRDS(paste0(codebook_directory, "world_bank_income_groups.RDS"))

# Merge additional columns
mergeVars <- c("iso_code", "location", "gbd_location_id", "iso_num_code")
subset <- zero_funds %>% left_join(gf_elibility_list, by=mergeVars) %>%
  left_join(world_bank_groups, by=mergeVars)

# re-code missing in global fund eligibility
subset[is.na(eligible_per_gf), eligible_per_gf:=0]

# Crete vector of ineligible locations
ineligible <- subset[which(subset$eligible_per_gf==0)]$location
saveRDS(ineligible, file=paste0(codebook_directory, "locations_ineligible_for_dah.RDS"))

# Part IV: Read in the IHME health spending data
# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[3], "/", file_list$data_source[3], "/", file_list$containing_folder[3], "/", file_list$file_name[3])

# Load data file
dt2 <- read_csv(file_path, show_col_types = FALSE)

# Subset rows
dt2 <- dt2 %>% filter(level=="Country")

# Subset columns
dt2 <- dt2 %>% select(location_id, location_name, iso3, year, the_total_mean, ghes_total_mean)

# rename variables for merging
dt2 <- rename(dt2,
              gbd_location_id=`location_id`,
              country=`location_name`,
              iso_code=`iso3`)

# merge onto dah_dataset
prepped_dah_dataset <- dah_dataset %>% full_join(dt2, by=c("year", "gbd_location_id", "iso_code"))
# 
# # extrapolate where necessary using GLM (better would be to use multiple imputation)
# numVars <- c("nch_cnv_dah_20", "the_total_mean", "ghes_total_mean")
# i=1
# for(v in numVars) {
#   for(h in unique(prepped_dah_dataset$location)) {
#     i=i+1
#     if (!any(is.na(prepped_dah_dataset[location==h][[v]]))) next
#     if (!any(!is.na(prepped_dah_dataset[location==h][[v]]))) next
#     form = as.formula(paste0(v,'~year'))
#     lmFit = glm(form, prepped_dah_dataset[location==h], family='poisson')
#     prepped_dah_dataset[location==h, tmp:=exp(predict(lmFit, newdata=prepped_dah_dataset[location==h]))]
#     lim = max(prepped_dah_dataset[location==h][[v]], na.rm=T)+sd(prepped_dah_dataset[location==h][[v]], na.rm=T)
#     prepped_dah_dataset[location==h & tmp>lim, tmp:=lim]
#     # ggplot(prepped_dah_dataset[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red')
#     prepped_dah_dataset[location==h & is.na(get(v)), (v):=tmp]
#     pct_complete = floor(i/(length(numVars)*length(unique(prepped_dah_dataset$location)))*100)
#     cat(paste0('\r', pct_complete, '% Complete'))
#     flush.console()
#   }
# }
# prepped_dah_dataset$tmp = NULL



# Part V: Create new categorical variable for analysis -----
# Create new variable
# dah_dataset$nch_cnv_dah_20_cat <- NA
# 
# dah_dataset <- dah_dataset %>% 
#   mutate(
#     nch_cnv_dah_20_cat = case_when(nch_cnv_dah_20==0 & location %in% ineligible ~ "5", # not eligible for funds
#                                    nch_cnv_dah_20 >0  & nch_cnv_dah_20 <= 50 ~ "2", # received low percentage of funds
#                                    nch_cnv_dah_20 >50 & nch_cnv_dah_20 <5243 ~ "3", # received medium percentage of funds
#                                    nch_cnv_dah_20 >= 5243 ~ "4", # received large percentage of overall funds
#                                    nch_cnv_dah_20==0 ~ "1")) # eligible for funds but did not receive
# 
# # save as numeric variable
# dah_dataset$nch_cnv_dah_20_cat <- as.numeric(dah_dataset$nch_cnv_dah_20_cat)

# select only one value variable to keep
# dah_dataset <- dah_dataset %>% select(-c(nch_cnv_dah_20))

# Re-order columns
prepped_dah_dataset <- prepped_dah_dataset %>% select(location, year, gbd_location_id, iso_code, iso_num_code, nch_cnv_dah_20, the_total_mean, ghes_total_mean)

# Save data
saveRDS(prepped_dah_dataset, file=paste0(prepped_data_dir, "aim_2/01_prepped_ihme_dah_data.RDS"))
