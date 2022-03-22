# Author: Francisco Rios 
# Purpose: Prep geographic area variable to help control for regional variation
# Date: Last modified March 7, 2022

# Clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

# Load the data file
full_data <- readRDS(paste0(raw_data_dir, "index_variables/world_values_survey/WVS_TimeSeries_1981_2020_R_v2_0.RDS"))

# keep variables of interest
data_subset <- full_data %>% 
  select(
    
    # iso3 alpha, original respondent code
    COUNTRY_ALPHA, S006,
                 
    # survey weights
    S017, 
                        
    # year of survey
    S020,
                        
    # confidence in government
    E069_11) 

# rename variables to keep consistent and more intuitive
data_subset <- rename(data_subset,
                      # new = old
                      main_weight = S017,
                      iso_code = COUNTRY_ALPHA,
                      year = S020 )

# re-code missing variables
data_subset <- data_subset %>% 
  mutate(E069_11 =  na_if(E069_11, -1)) %>%
  mutate(E069_11 =  na_if(E069_11, -2)) %>%
  mutate(E069_11 =  na_if(E069_11, -4)) %>%
  mutate(E069_11 =  na_if(E069_11, -5))

           # old = new
           # `-4` = NA)
           # recode(measure_name, Deaths='deaths', `YLDs (Years Lived with Disability)`='ylds'),

# re-code variable of confidence in government
data_subset <- data_subset %>% mutate(gov_confidence = case_when(
  
  # create group of those with high confidence in government
  E069_11==4 ~ 0,
  E069_11==3 ~ 0,
  E069_11==2 ~ 1,
  E069_11==1 ~ 1))

##### 
# apply the weights
# library(survey)

# Here we use "svydesign" to assign the weights. We will use this new design
# # variable "nhanesDesign" when running our analyses.
# weighted_data <- svydesign(id      = ~S006,
#                            strata   = ~iso_code,
#                            weights  = ~main_weight,
#                            nest     = TRUE,
#                            data     = data_subset)

# svymean(~gov_confidence, weighted_data, na.rm = TRUE)

# dataDesign <- svydesign(id = ~)
# nhanesDesign <- svydesign(id      = ~psu,
#                           strata  = ~strata,
#                           weights = ~persWeight,
#                           nest    = TRUE,
#                           data    = nhanesAnalysis)

#####
# calculate mean value of government confidence per year of data availability
data_subset <- as.data.table(data_subset)

data_subset <- data_subset[,.(mean_gov_confidence=mean(gov_confidence, na.rm=TRUE)),
    by=c("iso_code", "year")]

# load and merge location map
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))


merged_data <- location_map %>% inner_join(data_subset, by=c("iso_code"))

# save variables of interest
merged_data <- merged_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, mean_gov_confidence)

# save data source
saveRDS(merged_data, file = paste0(prepped_data_dir, "aim_2/09_prepped_gov_confidence_data.RDS"))
