# # Author: Francisco Rios Casas
# Date: February 18 2022
# Purpose: Prep county-level data for each state for analysis

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/county_level_analyses/01_set_up_R.r")

# load data
file_list <- read_xlsx(paste0(team_drive, "Data/Documentation/list_of_data_used.xlsx")) %>%
  filter(data_type=="county_demographics")

for (i in 1:nrow(file_list)) {
  
  file_dir <- paste0(raw_data_dir, "county_demographics/", file_list$containing_folder[i], "/")
  state <- file_list$location_name[i]
  tmpData <- read.csv(file=paste0(file_dir, file_list$file_name[i]))
  
  # subset census demographic data
  if (state=="Washington"){
    years_of_interest <- c(8, 10)
  } else {
      years_of_interest <- c(8, 12)
    }

  tmpData <- tmpData %>% filter(YEAR %in% years_of_interest) %>%
    filter(AGEGRP %in% c(0, 1)) %>%
    select(STATE, COUNTY, STNAME, CTYNAME, YEAR, AGEGRP, TOT_POP, 
           NHWA_MALE, NHWA_FEMALE, BAC_MALE, BAC_FEMALE, IAC_MALE, IAC_FEMALE, 
           AAC_MALE, AAC_FEMALE, H_MALE, H_FEMALE)
  
  # sum across sexes
  tmpData <- tmpData %>% mutate(NHWA = NHWA_MALE + NHWA_FEMALE,
                            BAC = BAC_MALE + BAC_FEMALE,
                            IAC = IAC_MALE + IAC_FEMALE,
                            AAC = AAC_MALE + AAC_FEMALE,
                            H   = H_MALE + H_FEMALE) %>% 
    # calculate additional variables
    mutate(PCT_NHWA = (NHWA/TOT_POP)*100,
           PCT_BAC = (BAC/TOT_POP)*100,
           PCT_IAC = (IAC/TOT_POP)*100,
           PCT_AAC = (AAC/TOT_POP)*100,
           PCT_H = (H/TOT_POP)*100)
  
  # select columns
  tmpData <- tmpData %>% select(STATE, COUNTY, STNAME, CTYNAME, YEAR, AGEGRP, 
                            TOT_POP, PCT_NHWA, PCT_BAC, PCT_IAC, PCT_AAC, PCT_H)
  
  
  tmpData <- tmpData %>% mutate(YEAR = case_when(YEAR==8 ~ 2015,
                                             YEAR==10 ~ 2017,
                                    YEAR==12 ~ 2019),
                            AGEGRP = case_when(AGEGRP==0 ~ "OVERALL",
                                               AGEGRP==1 ~ "UNDER_FOUR"))
  
  #Bind data together 
  if(i==1){
    extracted_demo_data <- tmpData
  } else {
    extracted_demo_data <- plyr::rbind.fill(extracted_demo_data, tmpData)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$location_name[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}
  
# save dataset of interest
saveRDS(extracted_demo_data, file=paste0(prepped_data_dir, "06_prepped_county_level_demographic_data.RDS"))

# write for loop to save the results individually
for (s in unique(extracted_demo_data$STNAME)){
  extracted_demo_data <- as_tibble(extracted_demo_data)
  saveRDS(extracted_demo_data %>% filter(STNAME==s), file=paste0(prepped_data_dir, "/prepped_state_level_data/02_census_data_individual_states/", s, " census_data.RDS"))
  write.csv(extracted_demo_data %>% filter(STNAME==s), file=paste0(prepped_data_dir, "/prepped_state_level_data/02_census_data_individual_states/", s, " census_data.csv"))
}
