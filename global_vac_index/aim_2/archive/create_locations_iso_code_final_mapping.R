# Author: Francisco Rios 
# Purpose: Create map of country locations to map standardized names onto data
# This file was only run once and then archived
# Date: Last modified November 16, 2021

# Load necessary package
library(openxlsx)

# Load codebooks that will be used to create final codebook
gbd_code <- read_xlsx(paste0(codebook_directory, "IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.xlsx")) # GBD Location Hierarchy
dah_code <- read_delim(paste0(codebook_directory, "IHME_DAH_DATABASE_1990_2020_CODEBOOK_Y2021M09D22.csv")) # DAH GBD codebook
iso_code <- read_xlsx(paste0(codebook_directory, "iso_codebook_final.xlsx")) # ISO Alpha-numeric codebook

# Subset GBD Locations to only countries (level 3)
gbd_code <- gbd_code %>% filter(Level==3)

# Subset columns
gbd_code <- gbd_code[,c(2:3)]

# Rename DAH column headers
name_row <- 1
names <- unlist(dah_code[name_row,])
names(dah_code) <- names

# Remove unnecessary rows and columns data
dah_code <- dah_code[-c(1,2), c(5:7)]

# Change DAH data structure
dah_code$gbd_location_id <- as.numeric(dah_code$gbd_location_id)

# Use GBD Locations to identify any locations missing from the DAH code book
missing_map <- gbd_code %>% 
  left_join(dah_code, by=c("Location ID"="gbd_location_id"))

missing_map <- missing_map %>% filter(is.na(recipient_isocode))

# count how many are missing
paste(nrow(missing_map))

# Use ISO code book to fill in missing countries from DAH
corrected_dah_locations <- missing_map %>% 
  left_join(iso_code, by=c("Location Name"="English short name"))

# Subset to only the columns that are relevant for the DAH code book
corrected_dah_locations <- corrected_dah_locations %>% 
  select(`Location ID`, `Location Name`, `Alpha-3 code`)

# Count how many are still missing
nrow(corrected_dah_locations %>% filter(is.na(`Alpha-3 code`)))

# Manually correct the missing locations
corrected_dah_locations$`Alpha-3 code`[which(corrected_dah_locations$`Location ID`==102)]<-"USA" # United States
corrected_dah_locations$`Alpha-3 code`[which(corrected_dah_locations$`Location ID`==89)]<-"NLD"  # Netherlands
corrected_dah_locations$`Alpha-3 code`[which(corrected_dah_locations$`Location ID`==95)]<-"GBR"  # United Kingdom
corrected_dah_locations$`Alpha-3 code`[which(corrected_dah_locations$`Location ID`==106)]<-"BHS" # Bahamas
corrected_dah_locations$`Alpha-3 code`[which(corrected_dah_locations$`Location ID`==422)]<-"VIR" # US Virgin Islands
corrected_dah_locations$`Alpha-3 code`[which(corrected_dah_locations$`Location ID`==156)]<-"ARE" # United Arab Emirates

# Rename the columns
corrected_dah_locations <- rename(corrected_dah_locations, 
                                  recipient_isocode=`Alpha-3 code`,
                                  recipient_country=`Location Name`,
                                  gbd_location_id=`Location ID`)

# Bind corrected dah locations with former list of dah_code
dah_code <- rbind(dah_code, corrected_dah_locations)

# merge dah codes onto the GBD location hiearchy
location_map <- gbd_code %>%
  left_join(dah_code, by=c("Location ID"="gbd_location_id"))

# merge iso numeric code onto the location mapping
complete_location_map <- location_map %>%
  left_join(iso_code, by=c("recipient_isocode"="Alpha-3 code"))

# Keep only columns of interest
complete_location_map <- complete_location_map[,c(1:3, 6)]

# Rename columns
complete_location_map <- rename(complete_location_map, 
       gbd_location_id=`Location ID`,
       location=`Location Name`,
       iso_code=recipient_isocode,
       iso_num_code=Numeric)

# Save final location map
saveRDS(complete_location_map, file=paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))
write.xlsx(complete_location_map, file=paste0(codebook_directory, "location_iso_codes_final_mapping.xlsx"))