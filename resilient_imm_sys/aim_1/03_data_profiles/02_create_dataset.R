# Author: Francisco Rios 
# Purpose: Create data set with all values that will be included on data profile
# Date: Last modified March 21, 2022

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/03_data_profiles/01_set_up_R.r")

# want to create a dataset that includes:
# vaccinations, demographics, covid data, covid hesitation, fips values

# load state-level analyses from NIS
state_vacc_data <- readRDS(paste0(prepped_data_dir, "03_estimates_vaccine_coverage_2007-2019.RDS"))

# recode state names
state_vacc_data <- state_vacc_data %>% mutate(state = case_when(
  ESTIAP=="29" ~ "North Carolina",
  ESTIAP=="66" ~ "Arizona",
  ESTIAP=="77" ~ "Washington")) %>% 
  mutate(race = case_when(RACEETHK_R==1 ~ "white",
                                       RACEETHK_R==2 ~ "hispa",
                                       RACEETHK_R==3 ~ "black",
                                       RACEETHK_R==4 ~ "other"),
                income = case_when(INCPOV1==1 ~ "high",
                                    INCPOV1==2 ~ "med", 
                                    INCPOV1==3 ~ "low",
                                    INCPOV1==4 ~ "miss")) %>% 
  filter(state%in%c("Arizona", "North Carolina", "Washington")) %>%
  select(state, race, income, VACCINE, YEAR, PredictedProb)

# reshape new variables
state_rates <- state_vacc_data %>% 
  pivot_wider(id_cols = c(state, YEAR, income, VACCINE), names_from = c(race), values_from = c(PredictedProb))

# calculate new variables
state_rates <- state_rates %>% mutate(hispa_diff = white-hispa,
                                      black_diff = white-black,
                                      other_diff = white-other)

# save final dataset
saveRDS(state_rates, file=paste0(prepped_data_dir, "11_merged_data_for_state_profile_docs.RDS"))

# load demo_vacc_data
demo_vacc_data <- readRDS(paste0(prepped_data_dir, "07_all_merged_county_vaccination_demographic_data.RDS")) %>%
  # keep only states of interest
  filter(state %in% c("Arizona", "North Carolina", "Washington")) %>%
  # subset data to keep only variables of interest
  select(state, county, change, year, category, proportion, TOT_POP_OVERALL, TOT_POP_UNDER_FOUR, 
         PCT_NHWA_OVERALL, PCT_NHWA_UNDER_FOUR, PCT_BAC_OVERALL, 
         PCT_BAC_UNDER_FOUR,  PCT_IAC_OVERALL,  PCT_IAC_UNDER_FOUR, 
         PCT_AAC_OVERALL,  PCT_AAC_UNDER_FOUR,  PCT_H_OVERALL,   
         PCT_H_UNDER_FOUR)

# reshape the data into long format for plotting
## dt1 contains percent change between years
dt1 <- demo_vacc_data %>% select(state, county, change) 
dt1 <- unique(dt1)
dt1 <- dt1 %>% 
  pivot_longer(!c(state, county), names_to = "variable", values_to="value")

# dt2 contains data on proportion of children vaccinated
dt2 <- demo_vacc_data %>% select(state, county, year, proportion)

dt2 <- dt2 %>% 
  pivot_wider(id_cols=c(state, county), 
              names_from = year, 
              values_from=c(proportion))

dt2 <- dt2 %>% pivot_longer(!c(state, county), names_to = "variable", values_to = "value")

# drop values that are missing
dt2 <- dt2 %>% filter(!is.na(value))

## dt3 contains information on demographic data
dt3 <- demo_vacc_data %>% 
  select(state, county, year, TOT_POP_OVERALL, TOT_POP_UNDER_FOUR, 
   PCT_NHWA_OVERALL, PCT_NHWA_UNDER_FOUR, PCT_BAC_OVERALL,
   PCT_BAC_UNDER_FOUR,  PCT_IAC_OVERALL,  PCT_IAC_UNDER_FOUR, 
   PCT_AAC_OVERALL,  PCT_AAC_UNDER_FOUR,  PCT_H_OVERALL,   
   PCT_H_UNDER_FOUR) %>% filter(year!=2015) %>%
  select(-year)

dt3 <- dt3 %>% pivot_longer(!c(state, county), names_to = "variable", values_to = "value")

## dt4 contains information on county rankings/categories
dt4 <- demo_vacc_data %>% select(state, county, category)
dt4 <- unique(dt4)
dt4 <- dt4 %>% 
  pivot_longer(!c(state, county), names_to = "variable", values_to = "value")

## dt5 contains information on Washington data for the years 2018 and 2019
dt5 <- read_rds(paste0(prepped_data_dir, "04_extracted_county_level_data.RDS")) %>% 
  filter(state=="Washington" & 
           year%in%c(2018, 2019) & 
           county %in% c("Chelan", "Skagit", "Yakima") &
           vaccine_name=="full_series")

# rename the variables
dt5 <- rename(dt5, "value"="proportion", "variable"="year")

# reshape the data
dt5 <- dt5 %>% 
  select(state, county, variable, value)

# recode county names to make sure they match with other locations
dt5 <- dt5 %>% mutate(county = case_when(
  county=="Chelan" ~ "Chelan County",
  county=="Skagit" ~ "Skagit County",
  county=="Yakima" ~ "Yakima County",
  TRUE ~ county
))

# bind all component dataframes together
demo_vacc_data <- rbind(dt1, dt2, dt3, dt4, dt5)

# load fips data
full_map <- usmap::us_map(regions = "counties") %>% select(fips, full, county)
fips <- unique(full_map) %>% rename(state=full) %>% filter(state%in%c("Arizona", "Washington", "North Carolina"))

# load covid vaccination coverage
covid_data <- readRDS(paste0(prepped_data_dir, "09_prepped_county_level_covid_vaccination_data.RDS")) %>%
  select(state, county, proportion, category) %>%
  rename(covid_proportion_18over=proportion,
         covid_category=category)

# pivot longer
dt5 <- covid_data %>% select(state, county, covid_category)
dt5 <- dt5 %>% pivot_longer(!c(state, county), names_to = "variable", values_to = "value")

dt6 <- covid_data %>% select(state, county, covid_proportion_18over)
dt6 <- dt6 %>% pivot_longer(!c(state, county), names_to = "variable", values_to = "value")

covid_data <- rbind(dt5, dt6)

# make sure merge variables are all in the appropriate dataset
fips_check <- paste0(fips$county)
data_check <- paste0(demo_vacc_data$county)

unmapped_codes <- demo_vacc_data[!data_check%in%fips_check,]

if(nrow(unmapped_codes)>0){
  print(unique(unmapped_codes[, c("county", "state"), with= FALSE]))
  # print(unique(unmapped_codes$file_name)) #For documentation in the comments above. 
  stop("You have locations in the data that aren't in the codebook!")
}

# merge fips data and bind other data files
# bind different data files together
demo_vacc_data <- demo_vacc_data %>% inner_join(fips, by=c("state", "county"))
covid_data     <- covid_data %>% inner_join(fips, by=c("state", "county"))
  
# bind all data together
full_data <- rbind(demo_vacc_data, covid_data)

# select rows of interest
unique(full_data$variable)

# rename rows
full_data <- full_data %>% mutate(variable= case_when( 
                    # old name = new name
                    variable=="change" ~ "Change between time points", 
                    variable=="2015" ~ "Fully vaccinated children in 2015",
                    variable=="2017" ~ "Fully vaccinated children in 2017",
                    variable=="2018" ~ "Fully vaccinated children in 2018",
                    variable=="2019" ~ "Fully vaccinated children in 2019",
                    variable=="TOT_POP_OVERALL" ~ "total_population",
                    variable=="TOT_POP_UNDER_FOUR" ~ "under4_population",
                    # variable==PCT_NHWA_OVERALL ~ "under4_population",
                    # variable==PCT_NHWA_UNDER_FOUR ~ "under4_population",
                    # variable==PCT_BAC_OVERALL ~ "under4_population",
                    # variable==PCT_BAC_UNDER_FOUR ~ "under4_population",
                    # variable==PCT_IAC_OVERALL ~ "under4_population",
                    # variable==PCT_IAC_UNDER_FOUR ~ "under4_population",
                    # variable==PCT_AAC_OVERALL ~ "under4_population",
                    # variable==PCT_AAC_UNDER_FOUR ~ "under4_population",
                    # variable==PCT_H_OVERALL ~ "under4_population",
                    # variable==PCT_H_UNDER_FOUR ~ "under4_population",
                    variable=="PCT_NHWA_UNDER_FOUR" ~ "Non-Hispanic White",
                    variable=="PCT_BAC_UNDER_FOUR" ~ "African-American/Black",
                    variable=="PCT_IAC_UNDER_FOUR" ~ "American Indian",
                    variable=="PCT_AAC_UNDER_FOUR" ~ "Asian-American",
                    variable=="PCT_H_UNDER_FOUR" ~ "Hispanic",
                    variable=="category" ~ "County level of childhood vaccination",
                    variable=="covid_category" ~ "County level of Covid-19 vaccination",
                    variable=="covid_proportion_18over" ~ "Fully vaccinated adults, Covid-19 vaccine",
                    TRUE ~ variable ))

# reorder columns
full_data <- full_data %>% select(state, county, fips, variable, value)

# save file
saveRDS(full_data, paste0(prepped_data_dir, "12_merged_data_for_county_profile_docs.RDS"))
