# ----------------------------------------------
# AUTHOR: Francisco Rios
# PURPOSE: Prep gbd covariate data
# DATE: Last updated June 2020.

prep_dx_trend_data <- function(dir, inFile, data_type){
  
  ### TROUBLESHOOTING HELP 
  # Uncomment lines below to run tests
  
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # data_type = file_list$data_type[i]

  # Load data
  if (data_type == 'disease_trends'){
    dx_data = data.table(read.csv(paste0(dir,inFile), encoding = "UTF-8"))
  } else {
    stop('Not a valid file iteration value.')
  }
  
  #
  return(dx_data)
  
}