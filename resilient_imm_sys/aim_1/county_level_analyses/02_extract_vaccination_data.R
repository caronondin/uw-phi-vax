# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Extract county-level data for each state

# clear everything
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/county_level_analyses/01_set_up_R.r")

# load file list and files ready to be prepped
file_list <- read_excel(paste0(team_drive, "Data/documentation/list_of_data_used.xlsx")) %>%
  filter(data_type=="county_level_vaccination_rates") %>% 
  filter(location_name%in%c("Washington", "Massachusetts", "Arizona", "North Carolina", "Virginia"))

# loop through data prep files
for(i in 1:nrow(file_list)){
  
  # Set up file path 
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$containing_folder[i], '/')
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  prep_function <- file_list$r_input_script[i]
  tmpData = do.call(prep_function, args)
  
  #Add indexing data
  append_cols <- file_list %>% filter(row_number()==i) %>% select(location_name, age_name, data_collection_method)
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

# rename columns
extracted_vax_data <- rename(extracted_vax_data,
       state = location_name)

# standardize vaccine names
extracted_vax_data$old_vaccine_name <- extracted_vax_data$vaccine_name
extracted_vax_data <- extracted_vax_data %>% mutate(vaccine_name = case_when(
  vaccine_name=="4 DTaP" ~ "dtap4",
  vaccine_name=="3 Polio" ~ "pol3",
  vaccine_name=="1 MMR" ~ "mmr1", 
  vaccine_name=="1 Varicella" ~ "var1",
  vaccine_name=="3 Hib" ~ "hib3",
  vaccine_name=="3 Hep B" ~ "hepb3",
  vaccine_name=="Series Complete" ~ "full_series",
  vaccine_name=="series" ~ "full_series",
  vaccine_name=="full series" ~ "full_series",
  vaccine_name=="var" ~ "var1",
  TRUE ~ old_vaccine_name ))

# reorder and subset columns
extracted_vax_data <- extracted_vax_data %>% 
  select(state, county, year, population, age_name, data_collection_method, vaccine_name, proportion)

# save extracted vaccination coverage
saveRDS(extracted_vax_data, file = paste0(prepped_data_dir, "04_extracted_county_level_data.RDS"))
write.csv(extracted_vax_data, file = paste0(prepped_data_dir, "04_extracted_county_level_data.csv"))
