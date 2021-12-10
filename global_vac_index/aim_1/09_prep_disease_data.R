# Author: Francisco Rios 
# Purpose: read and prep vaccine-preventable disease trend data
# Date: Last modified October 28, 2021

####### read in vaccine-preventable disease trend data

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")))

# subset files to latest disease trends data
file_list <- file_list[data_type=="disease_trends" & year=="2019"]

print("Now prepping:")

for(i in 1:nrow(file_list)){
  # Set up file path 
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/' )
  
  # set up arguments
  args <- list(file_dir, file_list$file_name[i], file_list$data_type[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(prep_dx_trend_data, args)
  
  #Add indexing data
  append_cols = file_list[i, .(file_name, data_type, data_source)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  #Bind data together 
  if(i==1){
    prepped_dx_data = tmpData
  } else {
    prepped_dx_data = rbind(prepped_dx_data, tmpData, use.names=TRUE, fill = TRUE)
  }

  print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}

# formatting of data ----

# load recently extracted data
dx_dt <- prepped_dx_data

# subset columns of interest
dx_dt <- dx_dt %>% 
  select(measure_name, location_id, location_name, 
         cause_id, cause_name, metric_name,
         year, val, upper, lower, file_name)

# rename columns for consistency
setnames(dx_dt, old = c("year"), new = c("year_id"))

# recode YDL values
dx_dt <- dx_dt %>%
  mutate(measure_name = recode(measure_name, Deaths='deaths', `YLDs (Years Lived with Disability)`='ylds'),
         metric_name  = recode(metric_name, Number='number', Percent='percent', Rate='rate'))

# split into three datasets (number, percent, rate)
dx_numb <- dx_dt %>% filter(metric_name=="number")
dx_rate <- dx_dt %>% filter(metric_name=="percent")
dx_perc <- dx_dt %>% filter(metric_name=="rate")

# pivot each data set wider
dx_numb <- dx_numb %>% 
  pivot_wider(
              names_from = c(measure_name, metric_name),
              names_glue = "{measure_name}_{metric_name}_{.value}",
              values_from = c(val, upper, lower)
              )

dx_rate <- dx_rate %>% 
  pivot_wider(
    names_from = c(measure_name, metric_name),
    names_glue = "{measure_name}_{metric_name}_{.value}",
    values_from = c(val, upper, lower)
  )

dx_perc <- dx_perc %>%
  pivot_wider(
    names_from = c(measure_name, metric_name),
    names_glue = "{measure_name}_{metric_name}_{.value}",
    values_from = c(val, upper, lower)
  )

# bind datasets back together
mergeCols <- c("location_id", "location_name", "cause_id", "cause_name", "year_id", "file_name")

tidy_data <- dx_numb %>% 
  full_join(dx_rate, by = mergeCols) %>%
  full_join(dx_perc, by = mergeCols)

# save prepped data 
saveRDS(tidy_data, outputFile08)

# print final statement
print("Step 08: Reading in disease trend data completed.")
