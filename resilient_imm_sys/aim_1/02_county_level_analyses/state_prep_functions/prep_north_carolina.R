# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Function to prep Massachusetts data at county level

prep_north_carolina = function(dir, inFile) {
  
  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
  # Load/prep data
  county_dataset <- as.data.table(read_xlsx(paste0(dir, inFile)))
  
  # rename columns
  names(county_dataset)[1] <- "county"
  # names(county_dataset)[2] <- "number_of_programs"
  # names(county_dataset)[3] <- "population"
  
  # remove rows without any county data
  county_dataset <- county_dataset %>% filter(!county=="Statewide County average")
  
  # remove rows without any county data
  # drop_rows <- county_dataset %>% filter(is.na(county))
  # drop_rows <- as.data.table(drop_rows)
  # drop_rows[, proportion:=as.numeric(` Complete`)]
  # na_proportion = drop_rows[, sum(proportion, na.rm = TRUE)]
  
  # if (na_proportion!=0){
  #   stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  # } else {
  #   county_dataset <- county_dataset %>% filter(!is.na(population))
  # }
  # 
  
  # drop total rows
  county_dataset <- county_dataset %>% filter(county!="State Total")
  
  # reshape the data
  # county_dataset <- 
  
  # make sure all variables are the same structure
  county_dataset$`2019` <- as.numeric(county_dataset$`2019`)
  county_dataset$`2020` <- as.numeric(county_dataset$`2020`)
    
  county_dataset <- pivot_longer(county_dataset, !c(county), names_to="year", values_to="proportion")
  
  # if (na_proportion!=0){
  #   stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  # } else {
  #   county_dataset <- county_dataset %>% filter(!is.na(county))
  # }
  
  county_dataset$vaccine_name <- "full series"
  
  # only keep columns of interest
  county_dataset <- county_dataset %>% select(county, year, vaccine_name, proportion)
  
  return(county_dataset)
  
}