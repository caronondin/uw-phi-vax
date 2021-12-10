# Author: Francisco Rios 
# Purpose: Merges all data to be used in analyses (such as vaccination trends, SDI, disease trends)
# Date: Last modified July 13, 2021

# Read in the previously saved files for vaccination trends
vax_dt <- readRDS(outputFile02)

# Read in the previously saved files for SDI trends
sdi_dt <- readRDS(outputFile03)

# Read in the previously saved files for disease trends
dx_dt <- readRDS(outputFile08)

# Read in the previously 
# # make sure that the merge variables align in both data tables
# sdi_concat <- paste0(sdi_dt$location_name)
# vax_concat <- paste0(vax_dt$location_name)
# unmapped_loc <- vax_dt[!vax_concat%in%sdi_concat]
# 
# if(nrow(unmapped_loc)>0){
#   print(unique(unmapped_loc[, c("location_name"), with= FALSE]))
#   print(unique(unmapped_loc$file_name)) #For documentation in the comments above.
#   stop("You have location names in the vaccine data that aren't in the SDI data!")
# }
# 
# # merge the first two data sets together
# dt <- merge(vax_dt, sdi_dt, by=c("location_name", "location_id", "year_id"), all.x = TRUE)
# 
# 
# 
# 
# # subset from year 1990 to present (as this is extent of SDI data)
# dt <- dt[year_id>=1990]


 # run error checks or missing location codes--




saveRDS(dt, outputFile09)

# Print final statement
print("Step 06: Merging complete; data set saved in prepped data folder")


