# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Prep county-level data for each state

# clear everything
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/county_level_analyses/01_set_up_R.r")

# load file list and files ready to be prepped
file_list <- read_excel(paste0(team_drive, "Data/documentation/list_of_data_used.xlsx")) %>%
  filter(data_type=="county_level_vaccination_rates") %>% 
  filter(location_name%in%c("washington", "massachusetts", "arizona", "north_carolina"))

# loop through data prep files
for(i in 1:nrow(file_list)){
  
  # Set up file path 
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$location_name[i], '/')
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  prep_function <- file_list$location_name[i]
  tmpData = do.call(prep_function, args)
  
  #Add indexing data
  append_cols <- file_list %>% filter(row_number()==i) %>% select(location_name, year, age_name)
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    extracted_vax_data <- tmpData
  } else {
    extracted_vax_data <- plyr::rbind.fill(extracted_vax_data, tmpData)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$location_name[i], " ", file_list$year[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}

# save extracted vaccination coverage
# extracted_vax_data
