# # Author: Francisco Rios Casas
# Date: February 18 2022
# Purpose: Prep county-level data for each state for analysis

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_3/01_set_up_R.r")

# load data
file_list <- read_xlsx(paste0(team_drive, "Data/Documentation/list_of_data_used.xlsx")) %>%
  filter(data_type=="covid_county_vaccinations")

for (i in 1:nrow(file_list)) {
  
  file_dir <- paste0(raw_data_dir, "covid_county_vaccination_rates/", file_list$containing_folder[i], "/")
  state <- file_list$location_name[i]
  tmpData <- read.csv(file=paste0(file_dir, file_list$file_name[i]), skip = 2)
  
  # rename columns of interest
  names(tmpData)[1]  <- "county"
  names(tmpData)[5]  <- "total_population"
  names(tmpData)[9]  <- "5+"
  names(tmpData)[13] <- "12+"
  names(tmpData)[17] <- "18+"
  names(tmpData)[21] <- "65+"
  
  # keep certain columns of interest
  tmpData <- tmpData %>% select(c(1,5,9, 13, 17, 21))
  
  # reshape data to be long
  tmpData <- pivot_longer(tmpData,!county , names_to = "age_group", values_to="proportion")
  
  #Add indexing data
  append_cols <- file_list %>% filter(row_number()==i) %>% select(location_name, year)
  stopifnot(nrow(append_cols)==1) 
  
  tmpData = cbind(tmpData, append_cols)

  #Bind data together 
  if(i==1){
    extracted_covid_county_data <- tmpData
  } else {
    extracted_covid_county_data <- plyr::rbind.fill(extracted_covid_county_data, tmpData)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$location_name[i], " ",  file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}  

# fix date variable
extracted_covid_county_data$year <- as.numeric(extracted_covid_county_data$year)
extracted_covid_county_data$year <- as.Date(extracted_covid_county_data$year, origin = "1899-12-30")

# fix proportion variable
extracted_covid_county_data <- extracted_covid_county_data %>%
  mutate(proportion = na_if(proportion, "N/A"))

extracted_covid_county_data$proportion <- as.numeric(extracted_covid_county_data$proportion)

# drop columns without county values (individuals vaccinated that didn't report county of residence)
extracted_covid_county_data <- extracted_covid_county_data %>% filter(county!="Unknown")
  
# fix column names
names(extracted_covid_county_data)[4] <- "state"

# save the extracted data in prepped data folder
saveRDS(extracted_covid_county_data, file=paste0(prepped_data_dir, "08_extracted_covid_county_vaccination_data.RDS"))
