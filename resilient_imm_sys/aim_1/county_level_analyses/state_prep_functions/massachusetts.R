# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Function to prep Massachusetts data at county level

massachusetts = function(dir, inFile) {
  
  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
  # Load/prep data
  county_dataset <- as.data.table(read_xlsx(paste0(dir, inFile)))
  
  # rename columns
  names(county_dataset)[1] <- "county"
  names(county_dataset)[2] <- "number_of_programs"
  names(county_dataset)[3] <- "population"
  
  # remove rows without any population data
  drop_rows <- county_dataset %>% filter(is.na(population))
  drop_rows <- as.data.table(drop_rows)
  drop_rows[, proportion:=as.numeric(`Series Complete`)]
  na_proportion = drop_rows[, sum(proportion, na.rm = TRUE)]
  
  if (na_proportion!=0){
    stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  } else {
    county_dataset <- county_dataset %>% filter(!is.na(population))
  }
  
  # drop total rows
  county_dataset <- county_dataset %>% filter(county!="State Total")
  
  # reshape the data
  county_dataset <- pivot_longer(county_dataset, !c(county, number_of_programs, population), names_to="vaccine_name", values_to="proportion")
  
  # if (na_proportion!=0){
  #   stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  # } else {
  #   county_dataset <- county_dataset %>% filter(!is.na(county))
  # }
  
  # only keep columns of interest
  county_dataset <- county_dataset %>% select(county, population, vaccine_name, proportion)
  return(county_dataset)
  
}