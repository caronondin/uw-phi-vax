# Author: Francisco Rios 
# Purpose: Prep Vaccination coverage data for analyses
# Date: Last modified July 13, 2021

# Read in vaccine trend data

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")))

# subset files to latest vaccine trends data
file_list <- file_list[data_type=="vaccination_trends" & disease=="all" & year=="2020"]

for(i in 1:nrow(file_list)){
  # Set up file path 
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/' )
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i], file_list$data_type[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(prep_vax_trend_data, args)
  
  #Add indexing data
  append_cols = file_list[i, .(file_name, data_type, data_source)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    prepped_vax_data = tmpData
  } else {
    prepped_vax_data = rbind(prepped_vax_data, tmpData, use.names=TRUE, fill = TRUE)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  }

# formatting of data -----

# load newly prepped data
dt <- prepped_vax_data

# subset columns
dt <- dt %>% select(location_id, location_name, vaccine_name, year_id, measure_name, val, upper, lower)

# recode measure_name value
dt <- dt %>% 
  mutate(measure_name=recode(measure_name, Proportion="prop"))

# pivot data wider
tidy_data <- dt %>%
  pivot_wider(
    names_from = c(measure_name),
    names_glue = "{measure_name}_{.value}",
    values_from = c(val, upper, lower)
  )

# convert from tibble to datatable
tidy_data <- as.data.table(tidy_data)

# save prepped data 
saveRDS(tidy_data, outputFile02)

# print final statement
print("Step 02: Reading and prepping vaccination trend data completed.")
