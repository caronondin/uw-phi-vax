# Author: Francisco Rios Casas
# Purpose: create location eligibility map
# December 07, 2021

# Read in the Global Fund location eligibility list
gf_list <- read_xlsx(paste0(codebook_directory, "gf_eligible_list.xlsx"))

# Select name row
names(gf_list)[1] <- "location"
names(gf_list)[2] <- "income"
names(gf_list)[3] <- "disease_component"
names(gf_list)[4] <- "disease_burden"
names(gf_list)[5] <- "eligibility"

# remove rows with blank location locations
gf_list <- gf_list %>% 
  filter(!is.na(location)) %>% 
  filter(!is.na(income) & !is.na(disease_component) & !is.na(disease_burden)) %>%
  filter(location!="Country")

# remove first row
gf_list <- gf_list[-1,]

# load the location map
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Double check whether all location names in vaccine confidence match names in the codebook
conf_concat <- paste0(gf_list$location)
loca_concat <- paste0(location_map$location)
gf_list$keep <- !conf_concat%in%loca_concat
unmapped_locs <- gf_list %>% filter(keep==TRUE)

# show which location names do not match the location names in the module map
unique(unmapped_locs$location)

# recode location names to make sure they will match the location_map file
gf_list$location[which(gf_list$location=="Bolivia (Plurinational State)")] <- "Bolivia (Plurinational State of)"
gf_list$location[which(gf_list$location=="Iran (Islamic Republic)")] <- "Iran (Islamic Republic of)"
gf_list$location[which(gf_list$location=="Korea (Democratic People's Republic)")] <- "Democratic People's Republic of Korea"
# gf_list$location[which(gf_list$location=="Kosovo")] <- 
gf_list$location[which(gf_list$location=="Lao (People's Democratic Republic)")] <- "Lao People's Democratic Republic"
gf_list$location[which(gf_list$location=="Micronesia (Federated States)")] <- "Micronesia (Federated States of)"
gf_list$location[which(gf_list$location=="Moldova")] <- "Republic of Moldova"
gf_list$location[which(gf_list$location=="Tanzania (United Republic)")] <- "United Republic of Tanzania"
gf_list$location[which(gf_list$location=="Venezuela")] <- "Venezuela (Bolivarian Republic of)"
# gf_list$location[which(gf_list$location=="Zanzibar")] <- 

# Drop zanzibar and kosovo from the list: 
gf_list <- gf_list %>% filter(location!="Kosovo") %>%
  filter(location!="Zanzabar")

# Keep only eligible location disease
gf_list <- gf_list %>% filter(eligibility=="Eligible")

# Select only columns of interest
gf_list <- gf_list %>% select(location, income)

# Remove duplicates of eligible countries
gf_list <- distinct(gf_list)

# Merge location map onto the data
gf_eligible_data <- gf_list %>%
  inner_join(location_map, by="location")

# Add indicator for global_fund eligibility
gf_eligible_data$eligible_per_gf <- 1

# Save GF Eligbility List on Codebook Folder
write.csv(gf_eligible_data, paste0(codebook_directory,"gf_eligible_locations_map.csv"))
saveRDS(gf_eligible_data, paste0(codebook_directory,"gf_eligible_locations_map.RDS"))
                                   