# Author: Francisco Rios Casas
# Date: February 22 2022
# Purpose: Merge vaccination and the demographic data for each state/county

# clear workspace
rm(list=ls())

# load set up script
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/county_level_analyses/01_set_up_R.r")

# load each data set
data  <- as.data.table(readRDS(paste0(prepped_data_dir, "05_prepped_county_level_vaccination_data.RDS")))
demos <- as.data.table(readRDS(paste0(prepped_data_dir, "06_prepped_county_level_demographic_data.RDS")))

# last minute data cleaning to make sure the data merge

# demos$CTYNAME <- str_to_title(demos$CTYNAME)

# recode the data for McDowell county in North Carolina
data <- data %>% 
  mutate(county = case_when(
    # capitalize the letter D to match with census name
    county=="Mcdowell County" & state=="North Carolina" ~ "McDowell County",
  
    # add the word "County" to two particular locations in Virginia which are counties
    county=="Charles City" & state=="Virginia"     ~ "Charles City County",
    county=="James City"   & state=="Virginia"     ~ "James City County",
    TRUE ~ county ))

demos <- demos %>% 
  mutate(
    # capitalize the word city in the demographic data to match the state data
    CTYNAME = case_when(
      STNAME=="Virginia" ~ str_to_title(CTYNAME),
      TRUE ~ CTYNAME))

# check to make sure that all locations in codebook are in the data
data_check <- paste0(data$county)
demo_check <- paste0(demos$CTYNAME)

unmapped_locs <- data[!data_check%in%demo_check]

if(nrow(unmapped_locs)>0){
  print(unique(unmapped_locs[, c("county", "state"), with= FALSE]))
  # print(unique(unmapped_codes$file_name)) #For documentation in the comments above. 
  stop("You have locations in the data that aren't in the census demographics data!")
}

# reshape county-level data
data <- data %>% pivot_longer(cols=c(`2015`, `2017` , `2019`),
                              names_to = "year",
                              values_to = "proportion") %>% 
  # DROP VALUES THAT ARE MISSING (NOT ALL STATES HAVE ALL YEARS OF DATA)
  filter(!is.na(proportion))

# change the format of the year variable
data$year <- as.numeric(data$year)

# re-shape the demographic data
demos <- demos %>% pivot_wider(id_cols = c("STATE", "COUNTY", "STNAME", "CTYNAME", "YEAR"),
                               values_from = c("TOT_POP", "PCT_NHWA", "PCT_BAC", "PCT_IAC", "PCT_AAC", "PCT_H"),
                               names_from = "AGEGRP")

# merge data together
full_data <- data %>% left_join(demos, by=c("county"="CTYNAME", "state"="STNAME", "year"="YEAR"))

# save the full output
saveRDS(full_data, file=paste0(prepped_data_dir, "07_all_merged_county_vaccination_demographic_data.RDS"))
write.csv(full_data, file=paste0(prepped_data_dir, "07_all_merged_county_vaccination_demographic_data.csv"))

# write for loop to save the results individually
for (s in unique(full_data$state)){
  saveRDS(full_data %>% filter(state==s), file=paste0(prepped_data_dir, "/prepped_state_level_data/03_merged_data_individual_states/", s, " merged_vaccination_demographic_data.RDS"))
  write.csv(full_data %>% filter(state==s), file=paste0(prepped_data_dir, "/prepped_state_level_data/03_merged_data_individual_states/", s, " merged_vaccination_demographic_data.csv"))
}
