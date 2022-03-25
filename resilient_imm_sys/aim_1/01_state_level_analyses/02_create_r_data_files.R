# Author: Francisco Rios Casas
# PURPOSE:  Run each of the R scripts which creates a permanent
# R Dataset
# Date: Jan 18 2022


# run loop which preps each script in turn
for(i in 1:14){
  
  # source set up script
  source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))
  
  # create file list indicating files to use
  nis_file_list <- read_xlsx(paste0(team_drive, "Data/documentation/list_of_data_used.xlsx")) %>% filter(data_type=="nis_survey")

  # set up file path where extracted R dataset is to be saved
  PUF <- paste0(raw_data_dir, "cdc/r_files")
  
  # set up location to ascii file that will be prepped
  flatfile <- paste0(raw_data_dir, "cdc/ascii_files/", nis_file_list$file_name[i])
  
  # set up name of prep script that needs to be called
  prep_script <- paste0(code_dir, "aim_1/r_input_scripts/", nis_file_list$r_input_script[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  source(prep_script)
  
  # Print message
  print(paste0(i, " ", nis_file_list$data_type[i], " ", nis_file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  
  # clear local environment
  rm(list=ls())
  
}
