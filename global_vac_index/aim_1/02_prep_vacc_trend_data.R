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

# Prep data on vaccine introductions -----
# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="vaccine_introduction")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type, "/", file_list$containing_folder, "/", file_list$file_name)

# Read data sheet
dt2 <- read_xlsx(file_path, sheet = 1)
dt2 <- dt2 %>% mutate(
  vaccine_name = case_when(
    # DESCRIPTION=="aP (acellular pertussis) vaccine" ~ "DTP3",
    DESCRIPTION=="Hepatitis B vaccine" ~ "HepB3",
    DESCRIPTION=="Hib (Haemophilus influenzae type B) vaccine" ~ "Hib3",
    DESCRIPTION=="Measles-containing vaccine 2nd dose" ~ "MCV2",
    DESCRIPTION=="PCV (Pneumococcal conjugate vaccine)" ~ "PCV3",
    # DESCRIPTION=="IPV (Inactivated polio vaccine)" ~ "Pol3",
    DESCRIPTION=="Rubella vaccine" ~ "RCV1",
    DESCRIPTION=="Rotavirus vaccine" ~ "RotaC"))

# standardize location names using GBD Location Info
location_map <- readRDS(paste0(codebook_directory, "aim_2/location_iso_codes_final_mapping.RDS"))
dt2 <- dt2 %>% full_join(location_map, by=c('ISO_3_CODE'='iso_code'))

# subset columns of interest
dt2 <- dt2 %>% select(gbd_location_id, location, iso_num_code, ISO_3_CODE, YEAR, vaccine_name, DESCRIPTION, INTRO) %>%
  filter(!is.na(vaccine_name)) %>% filter(!is.na(INTRO))

# find the earliest value a vaccine was introduced for each location
intro_year <- dt2 %>%
  group_by(gbd_location_id, location, vaccine_name) %>%
  mutate(minyear = min(YEAR, na.rm = T)) %>% 
  arrange(location)

# reshape data
prepped_intro_year <- unique(intro_year %>% select(gbd_location_id, location, vaccine_name, minyear))

# rename variables in the prepped_intro_year file
prepped_intro_year <- rename(prepped_intro_year, location_id=gbd_location_id, location_name=location)

# merge onto original data
prepped_data <- tidy_data %>% full_join(prepped_intro_year, by=c("location_id", "location_name", "vaccine_name"))

# set any value that is zero to missing if it occurred before the vaccine was officially introduced in each country
final_prepped_data <- prepped_data %>% mutate(prop_val= case_when(prop_val==0 & year_id<minyear ~ as.numeric(NA),
                                          TRUE~prop_val),
                        prop_upper=case_when(prop_upper==0 & year_id<minyear ~ as.numeric(NA),
                                             TRUE~prop_upper),
                        prop_lower=case_when(prop_lower==0 & year_id<minyear ~ as.numeric(NA)))
                        
check_na <- prepped_data %>% filter(year_id<minyear)
na_proportion = check_na[, sum(prop_val, na.rm = TRUE)]
# recode 0s that occurred before the vaccine was introduced


# save prepped data 
saveRDS(tidy_data, outputFile02)

# print final statement
print("Step 02: Reading and prepping vaccination trend data completed.")
