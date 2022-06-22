# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Function to prep washington data at county level

arizona = function(dir, inFile) {
  
  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
  # Load/prep data
  county_dataset <- read_xlsx(paste0(dir, inFile))
  
  # create a vector of names
  if (ncol(county_dataset)==11){
    names <- c("county", "population", "dtap4", "pol3", "mmr1", "hib3", "hepa2", "hepb3", "var", "religious_exempt", "medical_exempt")
  } else if(ncol(county_dataset)==12){
    names <- c("county", "population", "dtap4", "pol3", "mmr1", "hib3", "hepa2", "hepb3", "var", "religious_exempt", "medical_exempt", "exemptfromall")
  }
  
  names(county_dataset) <- names
  
  # drop empty rows
  county_dataset <- county_dataset %>% filter(!is.na(county)) %>% 
    filter(!is.na(population)) %>% 
    filter(!county=="Total") %>% 
    filter(!county=="County") %>%
    filter(!population=="Exempt")
  
  # subset columns
  county_dataset <- county_dataset %>% select(county, population, dtap4, pol3, 
                                              mmr1, hib3, hepa2, hepb3, var)
  
  # reshape data
  county_dataset <- county_dataset %>% pivot_longer(
    cols = c(3:9),
    names_to = c("vaccine_name"),
    values_to = "proportion")

  # change data type
  county_dataset$proportion <- as.numeric(county_dataset$proportion)
  county_dataset$population <- as.numeric(county_dataset$population)
  
  # add in additional variables from file_list
  return(county_dataset)
}
