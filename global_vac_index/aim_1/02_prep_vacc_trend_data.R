# Author: Francisco Rios 
# Purpose: Prep Vaccination coverage data for analyses and prep UN data on when 
# vaccines were first introduced
# Date: Last modified February 7, 2021

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
    extracted_vax_data = tmpData
  } else {
    extracted_vax_data = rbind(extracted_vax_data, tmpData, use.names=TRUE, fill = TRUE)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  }

# formatting of data -----

# load newly prepped data
dt <- extracted_vax_data

# subset columns
dt <- dt %>% select(location_id, location_name, vaccine_name, year_id, measure_name, val, upper, lower)

# recode measure_name value
dt <- dt %>% 
  mutate(measure_name=recode(measure_name, Proportion="prop"))

# pivot data wider
dt <- dt %>%
  pivot_wider(
    names_from = c(measure_name),
    names_glue = "{measure_name}_{.value}",
    values_from = c(val, upper, lower)
  )

# convert from tibble to datatable
dt <- as.data.table(dt)

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
  filter(!is.na(vaccine_name)) %>% 
  filter(!is.na(INTRO)) %>% 
  filter(INTRO%in%c("Yes", "Yes (R)", "Yes (P)", "Yes (A)"))

# find the earliest value a vaccine was introduced for each location
dt2 <- dt2 %>%
  group_by(gbd_location_id, location, vaccine_name) %>%
  mutate(minyear = min(YEAR, na.rm = T)) %>% 
  arrange(location)

# reshape data
dt2 <- unique(dt2 %>% select(gbd_location_id, location, vaccine_name, minyear))

# rename variables in the prepped_dt2 file
dt2 <- rename(dt2, location_id=gbd_location_id, location_name=location)

# merge onto original data
prepped_data <- dt %>% full_join(dt2, by=c("location_id", "location_name", "vaccine_name"))

# save prepepd_data as a tibble
prepped_data <- as_tibble(prepped_data)

# some vaccines do not have an introduction year

# drop any value that occurs before the official date of introduction per UNICEF
final_data <- prepped_data %>% mutate(too_early = 
                                        case_when(prop_val==0 & !is.na(minyear) & year_id<=minyear ~ 1,
                                                  !is.na(minyear) & year_id>minyear ~0,
                                                  is.na(minyear) ~ 0),
                                      )

# keep only data that is not too early to be counted
final_data <- final_data %>% filter(too_early==0)

# drop columns not needed
final_data <- final_data %>% select(-c(too_early, minyear))

# save prepped data as datatable
final_data <- as.data.table(final_data)
saveRDS(final_data, outputFile02)

# print final statement
print("Step 02: Reading and prepping vaccination trend data completed.")

