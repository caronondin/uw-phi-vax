# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Function to prep Massachusetts data at county level

prep_virginia = function(dir, inFile) {
  
  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]

  # Load/prep data
  if (inFile=="Virginia KG Data Request _2015_2018.xls"){
    county_dataset <- as.data.table(read_xls(paste0(dir, inFile)))
  } else {
    county_dataset <- as.data.table(read_xlsx(paste0(dir, inFile)))
  }

  
  # rename columns
  names(county_dataset)[1] <- "period"
  names(county_dataset)[4] <- "county"
  names(county_dataset)[8] <- "adequately_immunized"
  names(county_dataset)[12] <- "total"
  names(county_dataset)[13] <- "proportion"
  
  # replace with NA if values are asterisks
  county_dataset <- county_dataset %>% mutate(adequately_immunized = na_if(adequately_immunized, "*"))
   
  # drop values with insufficient data
  county_dataset <- county_dataset %>% filter(!is.na(adequately_immunized))
  
  # extract year variable
  county_dataset$year <- gsub("Fall ", "", county_dataset$period)
  
  # convert numeric variables to correct factor
  county_dataset$adequately_immunized <- as.numeric(county_dataset$adequately_immunized)
  county_dataset$total <- as.numeric(county_dataset$total)
  county_dataset$year <- as.numeric(county_dataset$year)
  
  # sum to county level
  county_dataset <- county_dataset[,.(sum_adequately_immunized=sum(adequately_immunized, na.rm = TRUE), sum_total=sum(total, na.rm=TRUE)),
                    by=c("county", "year")]
    
  # county_dataset <- pivot_longer(county_dataset, !c(county), names_to="year", values_to="proportion")
  
  county_dataset$vaccine_name <- "full series"
  
  # calculate proportion of children vaccinated in each county/city entity
  county_dataset$proportion <- county_dataset$sum_adequately_immunized/county_dataset$sum_total
  
  # only keep columns of interest
  county_dataset <- county_dataset %>% select(county, year, vaccine_name, proportion)
  
  return(county_dataset)
  
}
