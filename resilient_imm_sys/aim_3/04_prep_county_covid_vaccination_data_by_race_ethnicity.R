# Create additional county level data sets on covid vaccination according to race and ethnicity
# Date:

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_3/01_set_up_R.R")

# load file list and files ready to be prepped
file_list <- read_excel(paste0(team_drive, "Data/documentation/list_of_data_used.xlsx")) %>%
  filter(data_type=="covid_county_vaccinations_disaggregated") %>% 
  filter(location_name%in%c("North Carolina", "Arizona", "Washington"))

# create vector of state names that need to be prepped
states <- unique(file_list$location_name)

i <- 1

# loop through data prep files
for(i in 1:nrow(file_list)){
  
  # Set up file path 
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$containing_folder[i], '/', file_list$file_name[i])
  
  # read excel file
  tmpData <- read_xlsx(path=file_dir)
  
  # prep one of two North Carolina excel sheets here
  if (file_list$file_name[i]=="Vaccination_Ethnicity-NC-Cnty.xlsx"){
    tmpData <- tmpData %>% select(County, `Week of`, Hispanic) %>% 
      filter(County %in% c("Lenoir", "Edgecombe", "Hertford")) %>%
      filter(`Week of`=="3/21/2022")
    
    # get rid of negative values
    tmpData$Hispanic <- abs(tmpData$Hispanic)
    
    # reshape data
    tmpData <- pivot_longer(tmpData, cols=c("Hispanic"), names_to = "race_ethnicity", values_to="proportion")
    
    # add in variable name
    tmpData$variable <- "Percent of population vaccinated with two doses or one dose of J&J"
    
    # add in state
    tmpData$state <- "North Carolina"
    
    # rename columns
    tmpData <- rename(tmpData, "county"="County")
    
    # select variables to keep
    tmpData <- tmpData %>% select(state, county, race_ethnicity, variable, proportion)
  
    } else if (file_list$file_name[i]=="Vaccination_Race-NC-Cnty.xlsx") {
    
    tmpData <- tmpData %>% select(County, `Week of`, `American Indian or Alaskan Native`, `Asian or Pacific Islander`, `Black or African American`, `White`) %>% 
      filter(County %in% c("Lenoir", "Edgecombe", "Hertford")) %>%
      filter(`Week of`=="3/21/2022") 
    
    # reshape data
    tmpData <- pivot_longer(tmpData, cols = c("American Indian or Alaskan Native", "Asian or Pacific Islander", "Black or African American", "White"), names_to = "race_ethnicity",
                            values_to =  "proportion")
    
    # get rid of negative values
    tmpData$proportion <- abs(tmpData$proportion)
    
    # add in variable name
    tmpData$variable <- "Percent of population vaccinated with two doses or one dose of J&J"
    
    # add in state
    tmpData$state <- "North Carolina"
    
    # rename columns
    tmpData <- rename(tmpData, "county"="County")
    
    # select variables to keep
    tmpData <- tmpData %>% select(state, county, race_ethnicity, variable, proportion)
    
    } else if (file_list$file_name[i]=="azdhs_covid19_vaccine_data.xlsx") {
      
      # rename columns
      tmpData <- rename(tmpData,
                        proportion = value)
      
      # recode raceethnicity values
      tmpData <- tmpData %>% mutate(race_ethnicity = case_when(
        race == "white" ~ "White",
        race =="american indian" ~ "American Indian",
        race == "hispanic" ~ "Hispanic",
        race == "black" ~ "Black or African-American",
        race == "unknown" ~ "Unknown",
        race == "other" ~ "Other",
        race == "asian or pacific islander" ~ "Asian or Pacific Islander"
      ))
      
      # select columns of interest
      tmpData <- tmpData %>% select(state, county, race_ethnicity, variable, proportion)
      
    } else if (file_list$file_name[i]=="dohwa_dashboard_data.xlsx"){
      
      # rename columns
      tmpData <- rename(tmpData, "proportion"="value")
      
      # recode race/ethnicity values
      tmpData <- tmpData %>% mutate(race_ethnicity = case_when(
        race_ethnicity=="hispanic" ~ "Hispanic",
        race_ethnicity=="white" ~ "White",
        race_ethnicity=="asian" ~ "Asian",
        race_ethnicity=="black" ~ "Black or African-American",
        race_ethnicity=="american indian" ~ "American Indian",
        race_ethnicity=="native hawaiian/pacific islander" ~ "Native Hawaiian/Pacific Islander"
      ))
      
      # select columns of interest
      tmpData <- tmpData %>% select(state, county, race_ethnicity, variable, proportion)
    }
  
    # Bind data together 
  if(i==1){
    extracted_vax_data <- tmpData
  } else {
    extracted_vax_data <- plyr::rbind.fill(extracted_vax_data, tmpData)
  }
}

extracted_vax_data <- extracted_vax_data %>% filter(!is.na(proportion))

# save in one combined file in prepped data folder
saveRDS(extracted_vax_data, file=paste0(prepped_data_dir, "10_prepped_county_covid_vaccination_data_disaggregated.RDS"))
