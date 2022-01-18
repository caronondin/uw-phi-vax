# Author: Francisco Rios Casas
# PURPOSE:  Run each of the R scripts which creates a permanent
# R Dataset
# Date: Jan 18 2022

# Load file list indicating NIS data to prep
file_list <- read_xlsx(paste0(team_drive, "Data/documentation/list_of_data_used.xlsx"))

# select files to prep
file_list <- file_list %>% filter(data_type=="nis_survey")

# run loop which preps each script in turn
for(i in 1:nrow(file_list)){
  
  # set up file path where extracted R dataset is to be saved
  PUF <- paste0(raw_data_dir, "r_files/")
  
  # set up location to ascii file that will be prepped
  flatfile <- paste0(raw_data_dir, "cdc/ascii_files/", file_list$file_name[i])
  
  # set up name of prep script that needs to be called
  prep_script <- paste0(code_dir, "aim_1/r_input_scripts/", file_list$r_input_script[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  source(prep_script)
  
  # Print message
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  
}
# for(i in 1:nrow(file_list)){
#   # Set up file path 
#   file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/' )
#   
#   # set up arguments
#   args <- list(file_dir, file_list$file_name[i], file_list$data_type[i])
#   
#   ### RUN THE PREP FUNCTION HERE ###
#   tmpData = do.call(prep_vax_trend_data, args)
#   
#   #Add indexing data
#   append_cols = file_list[i, .(file_name, data_type, data_source)]
#   
#   stopifnot(nrow(append_cols)==1)
#   
#   tmpData = cbind(tmpData, append_cols)
#   
#   #Bind data together 
#   if(i==1){
#     prepped_vax_data = tmpData
#   } else {
#     prepped_vax_data = rbind(prepped_vax_data, tmpData, use.names=TRUE, fill = TRUE)
#   }
#   
#   print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
# }

# source each script which creates a final r data set