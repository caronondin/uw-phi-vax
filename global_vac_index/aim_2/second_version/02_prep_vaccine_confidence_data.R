# Author: Francisco Rios Casas
# Purpose: Load and prep vaccine confidence data (vaccines considered safe)
# Date: July 12, 2022

# Read in list of files to prep
file_list <- read_excel(paste0(file_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[8], "/", file_list$data_source[8], "/", file_list$file_name[8])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 6) # vaccines are safe
dt2 <- read_xlsx(file_path, sheet = 7) # vaccines are important
dt3 <- read_xlsx(file_path, sheet = 8) # vaccines are effective

# subset rows where population strongly agrees
dt1 <- filter(dt1, response=="strongly agree")
dt2 <- filter(dt2, response=="strongly agree")
dt3 <- filter(dt3, response=="strongly agree")

# rename columns to facilitate merge
colnames(dt1)[5] <- "mean_agree_vac_safe"
colnames(dt2)[5] <- "mean_agree_vac_important"
colnames(dt3)[5] <- "mean_agree_vac_effective"

# merge together the three different sheets
mergeVars <- c('country or territory', 'who_region', 'time', 'response')
dataset <- dt1 %>% 
  full_join(dt2, by=mergeVars) %>%
  full_join(dt3, by=mergeVars)
  
# rename column with country name
colnames(dataset)[1] <- "country"

# Load location code book to standardize names
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Double check whether all location names in vaccine confidence match names in the codebook
conf_concat <- paste0(dataset$country)
loca_concat <- paste0(location_map$location)
dataset$keep <- !conf_concat%in%loca_concat
unmapped_locs <- dataset %>% filter(keep==TRUE)

# show which country names do not match the location names in the module map
unique(unmapped_locs$country)

# Fix country names in the database
dataset$country[which(dataset$country=="Ivory Coast")] <- "Côte d'Ivoire"
dataset$country[which(dataset$country=="Republic of Congo")] <- "Congo"
dataset$country[which(dataset$country=="Swaziland")] <- "Eswatini"
dataset$country[which(dataset$country=="Tanzania")] <- "United Republic of Tanzania"
dataset$country[which(dataset$country=="Bolivia")] <- "Bolivia (Plurinational State of)"
dataset$country[which(dataset$country=="USA")] <- "United States of America"
dataset$country[which(dataset$country=="Venezuela")] <- "Venezuela (Bolivarian Republic of)"
dataset$country[which(dataset$country=="Iran")] <- "Venezuela (Bolivarian Republic of)"
dataset$country[which(dataset$country=="Syria")] <- "Syrian Arab Republic"
dataset$country[which(dataset$country=="Czech Republic")] <- "Czechia"
dataset$country[which(dataset$country=="Macedonia")] <- "North Macedonia"
dataset$country[which(dataset$country=="Moldova")] <- "Republic of Moldova"
dataset$country[which(dataset$country=="Russia")] <- "Russian Federation"
dataset$country[which(dataset$country=="UK")] <- "United Kingdom"
dataset$country[which(dataset$country=="Laos")] <- "Lao People's Democratic Republic"
dataset$country[which(dataset$country=="South Korea")] <- "Republic of Korea"
dataset$country[which(dataset$country=="Taiwan")] <- "Taiwan (Province of China)"
dataset$country[which(dataset$country=="Vietnam")] <- "Viet Nam"

# Kosovo, Hong Kong, and Northern Cyprus had no equivalent in the GBD location map

# Extract the year portion of the time variable
# prepped_migr_rate_dataset$year <- 
dataset$year <- sub('.*(\\d{4}).*', '\\1', dataset$time)
dataset$year <- as.numeric(dataset$year)
dataset$time <- as.numeric(dataset$time)
dataset$month_digit <- dataset$time - dataset$year
dataset$month <- round((dataset$month_digit * 12) + 1)

# Merge location map onto the data
prepped_vacc_confid_dataset <- dataset %>%
  inner_join(location_map, by=c("country"="location"))

prepped_vacc_confid_dataset <- prepped_vacc_confid_dataset %>% rename(location=country)

# Keep variables of interest
prepped_vacc_confid_dataset <- prepped_vacc_confid_dataset %>% select(location, year, month, gbd_location_id, iso_code, iso_num_code, 
                                                                      mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective)

# use data table to average across columns
prepped_vacc_confid_dataset <- as.data.table(prepped_vacc_confid_dataset)

test <- prepped_vacc_confid_dataset[,.(mean_agree_vac_safe=mean(mean_agree_vac_safe, na.rm = TRUE), 
                                       mean_agree_vac_important=mean(mean_agree_vac_important, na.rm = TRUE), 
                                       mean_agree_vac_effective=mean(mean_agree_vac_effective, na.rm=TRUE)), 
                by = c("location","year","gbd_location_id",
                       "iso_code","iso_num_code")]

# Save over dataset
prepped_vacc_confid_dataset <- as_tibble(test)

# Divide value by 100
prepped_vacc_confid_dataset$mean_agree_vac_safe <- prepped_vacc_confid_dataset$mean_agree_vac_safe/100
prepped_vacc_confid_dataset$mean_agree_vac_important <- prepped_vacc_confid_dataset$mean_agree_vac_important/100
prepped_vacc_confid_dataset$mean_agree_vac_effective <- prepped_vacc_confid_dataset$mean_agree_vac_effective/100

# Save prepped data source
saveRDS(prepped_vacc_confid_dataset, paste0(prepped_data_dir, "aim_2/12_prepped_vaccine_confidence_data.RDS"))
