# Author: Francisco Rios 
# Purpose: Prep socio-demographic data for analyses
# Date: Last modified July 13, 2021

# To-dos: -------
# need to find the loction ID for India (subnational estimates) somewhere -----

# Read in relevant SDI and location data files -----

# SDI data downloaded from GBD
sdi_dat <- as.data.table(read_xlsx(path = paste0(raw_data_dir, "sdi/IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.xlsx")))

# read in codebook for locations
loc_codebook <- as.data.table(read_xlsx(path = paste0(codebook_directory, "IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.xlsx")))

# locations that need to be added in manually
add_locs <- as.data.table(read_xlsx(path = paste0(codebook_directory, "gbd_location_corrections.xlsx")))

# Data Prep -----

# re-name variable names of SDI data
names_sdi <- as.character(sdi_dat[1])
names(sdi_dat) <- names_sdi
sdi_dat <- sdi_dat[-c(1),]

# subset codebook to location ID and Name for matching
loc_cleaning <- loc_codebook[,.(`Location ID`, `Location Name`)]

# rename location name variable in all three data files
setnames(loc_cleaning, old=c("Location Name", "Location ID"), new=c("location", "id"))
setnames(sdi_dat, old=c("Location"), new=c("location"))
setnames(add_locs, old=c("Location ID", "Location Name"), new=c("id", "location"))

# bind rows with country names that are not spelled exactly the same in the codebook
loc_cleaning <- rbind(loc_cleaning, add_locs, fill=TRUE)

# create merge label name in both codebook and data
sdi_dat <- strip_chars(sdi_dat)
loc_cleaning <- strip_chars(loc_cleaning)

# check to make sure that all locations in codebook are in the data
code_check <- paste0(loc_cleaning$location)
data_check <- paste0(sdi_dat$location)

unmapped_codes <- sdi_dat[!data_check%in%code_check]

if(nrow(unmapped_codes)>0){
  print(unique(unmapped_codes[, c("location"), with= FALSE]))
  # print(unique(unmapped_codes$file_name)) #For documentation in the comments above. 
  stop("You have locations in the data that aren't in the codebook!")
}

# -----
# remove unmapped locations without a location ID for now
# -----
sdi_dat <- sdi_dat[!location%in%unmapped_codes$location]

# merge the location IDs to the sdi data
sdi_dat <- merge(sdi_dat, loc_cleaning[,.(id, location)], by = "location", all.x = TRUE)

# merge rest of the codebook values to the SDI data
sdi_dat <- merge(sdi_dat, loc_codebook, by.x = "id", by.y = "Location ID", all.x = TRUE)

# check for duplicate rows
print(sdi_dat[duplicated(sdi_dat[,.(location, id)])])

################## manual correction to prepped SDI data due to duplicate names

# manually correct Georgia values (where country and US subnational region are misclassified) 
sdi_georgia <- sdi_dat[location=="georgia"]
sdi_georgia <- sdi_georgia[-c(2,3),] 

# manually correct Mexico values (where country and Mexico subnational region are misclassified)
sdi_mexico <- sdi_dat[location=="mexico"]
sdi_mexico <- sdi_mexico[-c(2,3),] 

# manually correct South Asia values (which is Super-Region and region)
sdi_southasia <- sdi_dat[location=="southasia"]
sdi_southasia <- sdi_southasia[-c(2,3),]

# manually correct North Africa values (which is Super-Region and region)
sdi_northafrica <- sdi_dat[location=="northafricaandmiddleeast"]
sdi_northafrica <- sdi_northafrica[-c(2,3),]

# remove rows that have been cleaned elsewhere
sdi_dat_prepped <- sdi_dat[location != c('georgia') & 
                             location != c('southasia') & 
                             location != c('mexico') & 
                             location !=c('northafricaandmiddleeast')]

# add in rows for newly cleaned data
sdi_dat_prepped <- rbind(sdi_dat_prepped, sdi_georgia, sdi_mexico, sdi_southasia, sdi_northafrica)

# check again for duplicates
print(sdi_dat_prepped[duplicated(sdi_dat_prepped[,.(location, id)])])

# rename columns in data frame
setnames(sdi_dat_prepped, 
         old = c("Location Set Version ID", "Location Name", "Parent ID", "Level", "Sort Order", "id"), 
         new = c("location_set_version_id", "location_name", "parent_id", "level", "sort_order", "location_id"))

# clean numerical values of SDIs
cols = names(sdi_dat_prepped)[3:32]   # define which columns to work with
sdi_dat_prepped[ , (cols) := lapply(.SD, function(x) { as.numeric(gsub("·", ".", x)) }), .SDcols = cols] # replace symbol

# remove unnecessary columns
sdi_dat_prepped[,c("orig_location", "location"):=NULL]

# calculate tertiles from 2019 country-level data
sdi.ter <- sdi_dat_prepped[level==3]$`2019`
# quantile(sdi.ter, c(0:3/3))

# classify countries into groups based on SDI in 2019
sdi_dat_prepped$sdi_group_present[sdi_dat_prepped$`2019` <= 0.5790 ] <- "low"
sdi_dat_prepped$sdi_group_present[sdi_dat_prepped$`2019` > 0.5790 & sdi_dat_prepped$`2019` <= 0.7423 ] <- "medium"
sdi_dat_prepped$sdi_group_present[sdi_dat_prepped$`2019` > 0.7423 ] <- "high"

# reshape data
sdi_dat_prepped_long = melt(sdi_dat_prepped, id.vars = c("location_name", "location_id", "location_set_version_id", 
                                                         "sdi_group_present", "parent_id", "level", "sort_order"),
                            variable.name = "year_id", value.name = "sdi")

# convert variable structures
sdi_dat_prepped_long$year_id <- as.numeric(levels(sdi_dat_prepped_long$year_id))[sdi_dat_prepped_long$year_id]

# subset columns necessary
sdi_dat_prepped_long <- sdi_dat_prepped_long %>% select(location_id, location_name, level, year_id, sdi, sdi_group_present)

# save in prepped data folder
saveRDS(sdi_dat_prepped_long, outputFile03)

# print final statement
print("Step 03: Prepping SDI data now complete.")