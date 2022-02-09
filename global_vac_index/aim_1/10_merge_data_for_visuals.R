# Author: Francisco Rios 
# Purpose: Merges all data to be used in analyses (such as vaccination trends, SDI, disease trends)
# Date: Last modified July 13, 2021

# Read in the previously saved files for vaccination trends
vax_dt <- readRDS(outputFile02)

# Read in the previously saved files for SDI trends
sdi_dt <- readRDS(outputFile03)

# Read in the previously saved files for disease trends
dx_dt <- readRDS(outputFile08)

# reformat vaccine name column
vax_dt$vaccine_name <- tolower(vax_dt$vaccine_name)

# Reshape the vaccine data to be wider
vax_dt_rsh <- vax_dt %>% pivot_wider(id_cols = c(location_id, location_name, year_id),
                          names_from = vaccine_name,
                          values_from = c(prop_val, prop_upper, prop_lower, minyear),
                          values_fill = NA)

# Reshape the disease data to be wider
dx_dt <- dx_dt %>% select(location_id, location_name, year_id, cause_name,
                          deaths_number_val, ylds_number_val, 
                          deaths_percent_val, ylds_percent_val,
                          deaths_rate_val, ylds_rate_val)

# # variables that should uniquely identify the values
# id_vals <- dx_dt %>% select(location_id, location_name, year_id, cause_name)
# 
# # create a dataframe of duplicated rows
# test2 <- id_vals[duplicated(id_vals),]
# 
# # View the duplicates
# View(test2)
# # Aceh, Tuberculosis is duplicated in 2017
# 
# # in original data identify why the duplicate exists
# dx_dt %>% filter(location_name=="Aceh" & year_id=="2017" & cause_name=="Tuberculosis")


# Rename the variable of cause_name
dx_dt$cause_name <- tolower(dx_dt$cause_name)
dx_dt <- dx_dt %>% mutate(cause_name=recode(cause_name,
                                   'diarrheal diseases'='diarrheal_diseases',
                                   'total burden related to hepatitis b'='hepb',
                                   'drug-susceptible tuberculosis'='drug-susceptible_tb',
                                   'multidrug-resistant tuberculosis without extensive drug resistance'='mdr_tb',
                                   'tuberculosis'='tb',
                                   'extensively drug-resistant tuberculosis'='xdrtb',
                                   'latent tuberculosis infection'='latent_tb',
                                   'whooping cough'='whooping_cough'))

dx_dt_rsh <- dx_dt %>% pivot_wider(id_cols=c(location_id, location_name, year_id),
                               names_from = cause_name,
                               values_from = c(deaths_number_val, ylds_number_val, 
                                               deaths_percent_val, ylds_percent_val,
                                               deaths_rate_val, ylds_rate_val))

# Set the merge variables
mergeVars <- c('location_id', 'year_id')

# Only keep location name in one of the data sets (disease trend data)
sdi_dt <- sdi_dt %>% select(-c(location_name))
vax_dt_rsh <- vax_dt_rsh %>% select(-c(location_name))

# Merge data together
full_data <- sdi_dt %>% 
  full_join(vax_dt_rsh, by=mergeVars) %>%
  full_join(dx_dt_rsh, by=mergeVars)

# keep only data that applies to level 3 (country)
full_data <- full_data %>% filter(level=="3")

# Re arrange columns
full_data <- relocate(full_data, location_name, .after=location_id)

# Save full data
saveRDS(full_data, file=outputFile09)

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

# saveRDS(dt, outputFile09)

# Print final statement
print("Step 06: Merging complete; data set saved in prepped data folder")


