# Author: Francisco Rios Casas
# Purpose: Load and prep World Bank Income Data to determine DAH eligibility
# Date: July 12 2022

# clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

#### Part I: load data and prep 
world_bank_groups <- read_xlsx(paste0(raw_data_dir, "index_variables/world_bank/OGHIST.xlsx"), sheet = "Country Analytical History")

# grab column names from row 5
col_names <- unlist(world_bank_groups[5,])
col_names[1] <- "iso_code"
col_names[2] <- "location"

# rename columns
names(world_bank_groups) <- col_names

# keep rows 11-228
world_bank_groups <- world_bank_groups[c(11:228),]

# pivot longer
world_bank_groups <- world_bank_groups %>% pivot_longer(cols=c(3:36), names_to = "year", values_to = "income_classification")

# subset years
world_bank_groups <- world_bank_groups %>% filter(year>=1990)

# recode ".." to missing
world_bank_groups <- world_bank_groups %>% mutate(income_classification = na_if(income_classification, ".."))
world_bank_groups$year <- as.numeric(world_bank_groups$year)

# save income classifications in codebook directory
write.csv(world_bank_groups, file=paste0(codebook_directory, "locations_world_bank_income_classifications.csv"))
saveRDS(world_bank_groups, file=paste0(codebook_directory, "locations_world_bank_income_classifications.RDS"))

### Part II: Merge onto location map to determine which nations are missing
world_bank_groups <- world_bank_groups %>% select(-c(location))

# Load the location map codebook
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# create a datatable to keep the data frame
years_of_data <- (1990:2020)
years <- matrix(nrow = 204, ncol = 31)
colnames(years) <- years_of_data
years <- data.table(years)

frame <- cbind(location_map, years)
frame <- as.data.table(frame)

frame_long <- melt(frame, id.vars=c('gbd_location_id', 'location', 'iso_code', 'iso_num_code'), variable.factor = FALSE)
frame_long$year <- as.numeric(frame_long$variable)

# Remove blank value from frame
frame_long <- frame_long[,-c(5,6)]

# Use the frame to map existing data
mapped_income_data <- frame_long %>% left_join(world_bank_groups, by = c('iso_code', 'year'))

#### Part III: Merge onto previously prepped financial data to determine eligibility
dah_data <- readRDS(file=paste0(prepped_data_dir, "aim_2/01_prepped_ihme_health_spending_data.RDS")) %>%
  select(location, year, gbd_location_id, iso_code, iso_num_code, dah_per_cap_ppp_mean)

all_data <- dah_data %>% full_join(mapped_income_data, by=c("location", "iso_num_code", "iso_code", "year", "gbd_location_id"))

# # check to see which countries have never received any funds
dt2 <- as.data.table(all_data)
zero_funds <- dt2[,.(sum_received=sum(dah_per_cap_ppp_mean, na.rm=TRUE)),
                  by=c("iso_code", "location", "gbd_location_id",
                       "iso_num_code")] %>% filter(sum_received==0)

zero_funds$never_received_funds <- TRUE

zero_funds <- zero_funds %>% select(-(sum_received))

all_data <- all_data %>% left_join(zero_funds, by=c("iso_code", "location", "gbd_location_id", "iso_num_code"))
all_data <- all_data %>% mutate(never_received_funds = replace_na(never_received_funds, FALSE))

# attempt to fill in whether locations are eligible for DAH based on income, and whether they ever received funds
all_data <- all_data %>% mutate(dah_eligible = case_when(
  
  # countries that are upper middle income and but never received funds will be marked as ineligible
  income_classification=="UM" & never_received_funds==TRUE ~ FALSE,
  income_classification=="UM" & never_received_funds==FALSE ~ TRUE,
  
  # there are some locations without income classifications but that still received DAH funds, mark as eligible
  is.na(income_classification) & never_received_funds==FALSE ~ TRUE, 
  is.na(income_classification) & never_received_funds==TRUE ~ TRUE,
  
  # lower middle income, low income countries are always eligible
  income_classification=="LM"  ~ TRUE,
  income_classification=="LM*" ~ TRUE,
  income_classification=="L"   ~ TRUE, 
  
  # high income countries are not eligible for funds
  income_classification=="H" ~ FALSE,
  income_classification=="H" ~ FALSE,
  
  TRUE ~ NA))

# keep columns of interest
all_data <- all_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code,
                                    dah_eligible, income_classification)

# Save data
saveRDS(all_data, file=paste0(prepped_data_dir, "aim_2/02_prepped_dah_eligibility_data.RDS"))