# Author: Francisco Rios Casas
# Date: February 11 2022
# Purpose: Function to prep washington data at county level

washington = function(dir, inFile) {
  
  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
  # Load/prep data
  county_dataset <- read_xlsx(paste0(dir, inFile))
  
  # rename id columns
  names(county_dataset)[1] <- "county"
  names(county_dataset)[2] <- "year"
  names(county_dataset)[3] <- "sex_name"
  names(county_dataset)[4] <- "supressed"
  
  # split up into separate vaccination data
  idVars = c("county", "year", "sex_name", "supressed")
  
  # create vector of repeating variable names to facilitate naming
  frame <- data.frame(vaccine=rep(c("series", "mmr1", "var1", "hepb3", "hib3","pol3", "dtap4", "pcv4"), each=5),
                      variable=rep(c("count", "population", "proportion", "UB", "LB")))
  
  frame$names <- paste0(frame$vaccine, "_", frame$variable)
  
  names <- c(idVars, frame$names)
  names(county_dataset) <- names
  
  # drop un-necessary rows
  county_dataset <- county_dataset %>% 
    filter(!is.na(county)) %>% 
    filter(!county=="Geography")
  
  # pivot data long 
  county_dataset <- county_dataset %>% pivot_longer(
    cols = c(5:44),
    names_to = c("vaccine_name", "variable_name"),
    names_sep = "_",
    values_to = "value"
  )
  
  # only keep columns of interest
  county_dataset <- county_dataset %>% select(county, vaccine_name, variable_name, value)
  
  # subset rows
  county_dataset <- county_dataset %>% filter(variable_name %in% c("population", "proportion"))
  
  # reshape data once more
  county_dataset <- county_dataset %>% pivot_wider(id_cols = c("county", "vaccine_name"), 
                                 names_from = variable_name, 
                                 values_from= value)
  
  # change data type
  county_dataset$proportion <- as.numeric(county_dataset$proportion)
  
  # add in additional variables from file_list
  return(county_dataset)
  
}
