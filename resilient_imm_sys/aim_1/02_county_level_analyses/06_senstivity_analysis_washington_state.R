# Author: Francisco Rios Casas
# Date: February 18 2022
# Purpose: Prep county-level data for each state for analysis--
# slightly modify the script to prep different years for washington state

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/02_county_level_analyses/01_set_up_R.r")

# load data
full_data <- readRDS(file = paste0(prepped_data_dir, "04_extracted_county_level_data.RDS")) %>% filter(state=="Washington")

# create vector of state names that need to be prepped
states <- unique(full_data$state)

i <- 1

for (s in states) {

  # subset data-reshape data
  if (s=="Washington"){
    data <- full_data %>% filter(state==s) %>% 
     filter(year %in% c(2015, 2019))}  else {
    data <- full_data %>% filter(state==s) %>%
    filter(year %in% c(2015, 2019))
    }
  
  tmpData <- pivot_wider(data, id_cols = c(state, county, 
                              age_name, data_collection_method, vaccine_name),
            names_from = year,
            values_from = proportion)


  # caculate percent change between min and max year

  if (s=="Washington"){
    tmpData$change <- (tmpData$`2019` - tmpData$`2015`)
  } else {
    tmpData$change <- (tmpData$`2019` - tmpData$`2015`)
    }


  # some states have multiple vaccinations available, but we are just going to use full series data

  if (s %in% c("Massachusetts", "Washington")){
     tmpData <- tmpData %>% filter(vaccine_name=="full_series")
  } else if(s=="Arizona"){
       tmpData <- tmpData %>% filter(vaccine_name=="dtap4")
  }
  
  # for Virginia or Washington, small counties do not have data reported for some years, so will drop rows where change cannot be calculate
  if (s=="Virginia"){
    tmpData <- tmpData %>% filter(!is.na(change))
  } else if (s=="Washington"){
    tmpData <- tmpData %>% filter(!is.na(change))
  }

  # calculate quantile cutoffs for each state

  quants <- c(.25, .75)
  quantiles <- as_tibble(quantile(tmpData$change, probs = quants, na.rm = TRUE))
  quantiles$quant <- NA 
  quantiles$quant[1] <- "first_q"
  quantiles$quant[2] <- "third_q"

  quantiles <- pivot_wider(quantiles, names_from = quant, values_from = value)
  quantiles$state <- s

  tmpData <- tmpData %>% full_join(quantiles, by="state")

  # drop outliers from each state
  # create function that deletes outliers
  is_outlier <- function(x){
    lower_bound <- quantile(x, 0.05)
    upper_bound <- quantile(x, 0.95)
    ifelse((x < lower_bound | x > upper_bound), NA, x)
    }

  numVars <- "change"
  for (v in numVars) {
    tmpData <- as.data.table(tmpData)
    tmpData[, (v):=is_outlier(get(v))]
    tmpData <- tmpData %>% filter(!is.na(change))
  }

  # group counties according to the change seen in each state
  tmpData <- tmpData %>% 
    mutate(category = case_when(change >= first_q & change <= third_q ~ "medium",
                              change >= third_q ~ "high",
                              change <= first_q ~ "low"))

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
  
  # Some states require the word "county" in the names of the county
  if (s %in% c("Washington", "Arizona", "Massachusetts")){
    tmpData$county <- paste0(tmpData$county," County")
  }

saveRDS(tmpData,   file=paste0(prepped_data_dir, "_archive/washington_sensitivity_analysis/01_vaccination_data_", file_name_state, ".RDS"))
write.csv(tmpData, file=paste0(prepped_data_dir, "_archive/washington_sensitivity_analysis/01_vaccination_data_", file_name_state, ".RDS"))

  #Bind data together 
  # if(i==1){
  #   prepped_county_data <- tmpData
  # } else {
  #   prepped_county_data <- plyr::rbind.fill(prepped_county_data, tmpData)
  # }
  # 
  # save bound data
  # saveRDS(prepped_county_data, file=paste0(prepped_data_dir, "_archive05_prepped_county_level_vaccination_data.RDS"))
  
  print(paste0(i, " ", s, " is prepped")) # if the code breaks, you know which file it broke on
  
  i <- i + 1
}

# load the two data frames and compare their county rankings and county groupings???
old <- readRDS(paste0(prepped_data_dir, "prepped_state_level_data/01_vaccination_data_individual_states/washington.RDS")) %>% 
  select(state, county, change, category) %>%
  rename(old_change=change, old_category=category)
  
new <- tmpData %>% 
  select(state, county, change, category) %>%
  rename(new_change=change, new_category=category)

# merge the two datasets together
data_check <- inner_join(old, new, by=c("state", "county"))

# compare level of change and groupings--especially for Chelan, Yakima, Skagit
data_check <- data_check %>% filter(county %in% c("Skagit County", "Chelan County", "Yakima County"))
