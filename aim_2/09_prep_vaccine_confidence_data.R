# Author: Francisco Rios Casas
# Purpose: Load and prep vaccine confidence data (vaccines considered safe)
# Date: November 19, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[8], "/", file_list$data_source[8], "/", file_list$file_name[8])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 6)

# subset rows where population strongly agrees
dt1 <- filter(dt1, response=="strongly agree")

# rename columns
colnames(dt1)[1] <- "country"
colnames(dt1)[5] <- "mean_agree_vac_safe"

# Load location codebook to standardize names
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Double check whether all location names in vaccine confidence match names in the codebook
conf_concat <- paste0(dt1$country)
loca_concat <- paste0(location_map$location)
dt1$keep <- !conf_concat%in%loca_concat
unmapped_locs <- dt1 %>% filter(keep==TRUE)

# show which country names do not match the location names in the module map
unique(unmapped_locs$country)

# Fix country names in the database
dt1$country[which(dt1$country=="Ivory Coast")] <- "Côte d'Ivoire"
dt1$country[which(dt1$country=="Republic of Congo")] <- "Congo"
dt1$country[which(dt1$country=="Swaziland")] <- "Eswatini"
dt1$country[which(dt1$country=="Tanzania")] <- "United Republic of Tanzania"
dt1$country[which(dt1$country=="Bolivia")] <- "Bolivia (Plurinational State of)"
dt1$country[which(dt1$country=="USA")] <- "United States of America"
dt1$country[which(dt1$country=="Venezuela")] <- "Venezuela (Bolivarian Republic of)"
dt1$country[which(dt1$country=="Iran")] <- "Venezuela (Bolivarian Republic of)"
dt1$country[which(dt1$country=="Syria")] <- "Syrian Arab Republic"
dt1$country[which(dt1$country=="Czech Republic")] <- "Czechia"
dt1$country[which(dt1$country=="Macedonia")] <- "North Macedonia"
dt1$country[which(dt1$country=="Moldova")] <- "Republic of Moldova"
dt1$country[which(dt1$country=="Russia")] <- "Russian Federation"
dt1$country[which(dt1$country=="UK")] <- "United Kingdom"
dt1$country[which(dt1$country=="Laos")] <- "Lao People's Democratic Republic"
dt1$country[which(dt1$country=="South Korea")] <- "Republic of Korea"
dt1$country[which(dt1$country=="Taiwan")] <- "Taiwan (Province of China)"
dt1$country[which(dt1$country=="Vietnam")] <- "Viet Nam"

# Kosovo, Hong Kong, and Northern Cyprus had no equivalent in the GBD location map
# dt1$country[which(dt$country=="Kosovo")] <- ""
# dt1$country[which(dt$country=="Northern Cyprus")] <- ""

# Merge location map onto the data
prepped_vacc_confid_dataset <- dt1 %>%
  inner_join(location_map, by=c("country"="location"))

prepped_vacc_confid_dataset <- prepped_vacc_confid_dataset %>% rename(location=country)

# Keep variables of interest
prepped_vacc_confid_dataset <- prepped_vacc_confid_dataset %>% select(location, time, gbd_location_id, iso_code, iso_num_code, mean_agree_vac_safe)

# Save prepped data source
saveRDS(prepped_vacc_confid_dataset, paste0(prepped_data_dir, "aim_2/08_prepped_vaccine_confidence_data.RDS"))
