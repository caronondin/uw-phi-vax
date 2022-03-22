# Author: Francisco Rios Casas
# Date: February 18 2022
# Purpose: Prep county-level data for each state for analysis

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_3/01_set_up_R.r")

# load data
data <- readRDS(paste0(prepped_data_dir, "08_extracted_covid_county_vaccination_data.RDS"))

# identify age-group of interest
data <- data %>% filter(age_group=="18+")

# create vector of state names that need to be prepped
states <- unique(data$state)

i <- 1

# group counties according to vaccination coverage seen in each state
for (s in states){
  # uncomment to troubleshoot
  # s <- states[i]
  
  # subset data
  tmpData <- data %>% filter(state==s)
  
  # calculate quantile cutoffs for each state
  quants <- c(.25, .75)
  quantiles <- as_tibble(quantile(tmpData$proportion, probs = quants, na.rm = TRUE))
  quantiles$quant <- NA 
  quantiles$quant[1] <- "first_q"
  quantiles$quant[2] <- "third_q"
  
  quantiles <- pivot_wider(quantiles, names_from = quant, values_from = value)
  quantiles$state <- s
  
  tmpData <- tmpData %>% full_join(quantiles, by="state")
  
  # # drop outliers from each state
  # # create function that deletes outliers
  # is_outlier <- function(x){
  #   lower_bound <- quantile(x, 0.05)
  #   upper_bound <- quantile(x, 0.95)
  #   ifelse((x < lower_bound | x > upper_bound), NA, x)
  # }
  # 
  # numVars <- "proportion"
  # for (v in numVars) {
  #   tmpData <- as.data.table(tmpData)
  #   tmpData[, (v):=is_outlier(get(v))]
  #   tmpData <- tmpData %>% filter(!is.na(proportion))
  # }
  
  # group counties according to the proportion seen in each state
  tmpData <- tmpData %>% 
    mutate(category = case_when(proportion >= first_q & proportion <= third_q ~ "medium",
                                proportion >= third_q ~ "high",
                                proportion <= first_q ~ "low"))
  
  # save prepped data at the county level
  if (s=="North Carolina"){
    file_name_state <- "north_carolina"
  } else if (s=="Washington"){
    file_name_state <- "washington"
  } else if (s=="Arizona"){
    file_name_state <- "arizona"
  } else if (s=="Massachusetts"){
    file_name_state <- "massachusetts"
  } else if (s=="Virginia"){
    file_name_state <- "virginia"
  }
  
  saveRDS(tmpData,   file=paste0(prepped_data_dir, "prepped_state_level_data/04_covid_vaccination_data_individual_states/", file_name_state, "_covid_vaccination_rates.RDS"))
  write.csv(tmpData, file=paste0(prepped_data_dir, "prepped_state_level_data/04_covid_vaccination_data_individual_states/", file_name_state, "_covid_vaccination_rates.csv"))
  
  #Bind data together 
  if(i==1){
    prepped_county_data <- tmpData
  } else {
    prepped_county_data <- plyr::rbind.fill(prepped_county_data, tmpData)
  }
  
  # save bound data
  saveRDS(prepped_county_data, file=paste0(prepped_data_dir, "09_prepped_county_level_covid_vaccination_data.RDS"))
  
  print(paste0(i, " ", s, " is prepped")) # if the code breaks, you know which file it broke on
  
  i <- i + 1
  
}
