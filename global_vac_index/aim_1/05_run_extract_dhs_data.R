# AUTHOR: Francisco Rios
# PURPOSE: Extract all necessary DHS data
# DATE: Last updated September 2 2021

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(g_drive, "Data/list_of_data_used.xlsx")))

# subset files to dhs data (and only Nigeria data for now)
file_list <- file_list[data_source%in%c("dhs6", "dhs7") & location_name=="Nigeria",.(file_name, data_type, data_source, year, containing_folder, location_name)]

for(i in 1:nrow(file_list)){
  # Set up file path 
  file_dir <- paste0(raw_data_dir, file_list$data_type[i], '/dhs/', file_list$containing_folder[i], '/' )
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i], file_list$containing_folder[i], file_list$data_source[i], file_list$location_name[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(extract_dhs_data, args)
  
  #Add indexing data
  append_cols = file_list[i, .(file_name)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    extracted_dhs_data = tmpData
  } else {
    extracted_dhs_data = rbind.fill(extracted_dhs_data, tmpData)
  }
  print("Now prepping:")
  print(paste0(i, " ", file_list$location_name[i], " ", file_list$data_source[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}

# save raw extracted data 
saveRDS(extracted_dhs_data, outputFile05)

# print final statement
print("Step 05: Extracting dhs vaccination data completed.")
