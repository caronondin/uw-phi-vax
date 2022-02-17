# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Function to prep new mexico data at county level

new_mexico = function(dir, inFile) {
  
  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
  # Load/prep data
  county_dataset <- read_xlsx(paste0(dir, inFile))
  
  # rename columns
  names(county_dataset)[1] <- "county"
  names(county_dataset)[3] <- "total_population"
  names(county_dataset)[4] <- "up_to_date"
  names(county_dataset)[5] <- "proportion"
  
  # drop extra row
  drop_rows <- county_dataset %>% filter(is.na(county))
  drop_rows <- as.data.table(drop_rows)
  drop_rows[, proportion:=as.numeric(proportion)]
  na_proportion = drop_rows[, sum(proportion, na.rm = TRUE)]
  
  if (na_proportion!=0){
    stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  } else {
    county_dataset <- county_dataset %>% filter(!is.na(county))
  }
  
  # only keep columns of interest
  county_dataset <- county_dataset %>% select(county, total_population, up_to_date, proportion)
  
  # change data type
  county_dataset$total_population <- as.numeric(county_dataset$total_population)
  county_dataset$up_to_date <- as.numeric(county_dataset$up_to_date)
  county_dataset$proportion <- as.numeric(county_dataset$proportion)
  
  # add in additional variables from file_list
  
  return(county_dataset)
  
}
# # loop through data prep files
# for(i in 1:nrow(file_list)){
#   
# 
#   # i <- 1
#   # 
#   # # Set up file path 
#   # file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$location_name[i], '/', file_list$file_name[i])
#   # 
#   # set up arguments
#   # args <- list(file_dir, file_list$file_name[i], file_list$data_type[i])
#   
#   # ### PREP EACH FILE HERE HERE ###
#   # county_dataset <- read_xlsx(file_dir)
#   
#   # starting total check to make sure no values are dropped
#   
#   
#   
#   #Add indexing data
#   # append_cols = file_list[i, .(location_name, year)]
#   append_cols <- file_list %>% filter(row_number()==i) %>% select(location_name, year, age_name, vaccine_name)
#   stopifnot(nrow(append_cols)==1)
#   
#   county_dataset = cbind(county_dataset, append_cols)
#   
#   #Bind data together 
#   if(i==1){
#     extracted_vax_data = county_dataset
#   } else {
#     extracted_vax_data = rbind(extracted_vax_data, county_dataset, use.names=TRUE, fill = TRUE)
#   }
#   
#   print(paste0(i, " ", file_list$data_type[i], " ", file_list$location_name[i], " ", file_list$year[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
# }
# 
# # save extracted vaccination coverage
# extracted_vax_data
