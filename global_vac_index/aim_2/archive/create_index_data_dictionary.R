# Author: Francisco Rios Casas
# Purpose: Create codebook of all variables used and calculated to place on website
# Date: December 07, 2021

# clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

# load prepped data
data <-  readRDS(paste0(prepped_data_dir, "aim_2/11_index_results.RDS"))

# calculate unique values--for the id variables these should all be the same and match up
id_cols <- data %>% select(location, gbd_location_id, iso_code, iso_num_code)
id_cols <- unique(id_cols)

# create a frame with necessary column names and descriptions column with descriptions
frame <- data.frame(Variable=c("Label:"), 
                    location=c("Country name"),
                    year=c("Year"),
                    gbd_location_id=c("Country's IHME Location ID"),
                    iso_code=c("ISO 3-digit code for country"),
                    iso_num_code=c("ISO 3-digit numeric code for country"),
                    region=c("Country's Global Burden of Disease region"),
                    dah_eligible=c("Country was eligible for development assistance"),
                    sdi = c("Socio-demographic Index"),
                    the_per_cap_mean = c("Total Health Spending per person"),
                    ghes_per_the_mean = c("Government Health Spending per person"),
                    dah_per_cap_ppp_mean = c("Development Assistance for Health per person"),
                    haqi = c("Healthcare Access and Quality Index"),
                    cpi = c("Corruption Perception Index"),
                    perc_skill_attend = c("Births attended by skilled health personnel (%)"),
                    imm_pop_perc = c("International migrants as a percentage of the total population"),
                    perc_urban = c("Percentage of population residing in urban areas"),
                    result = c("Vaccine Improvement Index"))

# find unique values for the other columns
reg_col <- data %>% select(region) %>% unique()
year_col <- data %>% select(year) %>% unique()
dah_col <- data %>% select(dah_eligible) %>% unique()

# add in labels
# cdbook <- rbind.fill(id_cols, year_col, reg_col, dah_col)
all_col <-  qpcR:::cbind.na(id_cols, year_col, reg_col, dah_col)

all_col$sdi <- NA
all_col$the_per_cap_mean <- NA
all_col$ghes_per_the_mean <- NA
all_col$dah_per_cap_ppp_mean <- NA
all_col$haqi <- NA
all_col$cpi <- NA
all_col$perc_skill_attend <- NA
all_col$imm_pop_perc <- NA
all_col$perc_urban <- NA
all_col$result <- NA
all_col$Variable <- NA

# merge files together??somehow??
full_codebook <- rbind(frame, all_col)

# Add in Value Coding Label
full_codebook$Variable[2] <- "Value Coding:"

# re-arrange the order of the values
full_codebook <- full_codebook %>% select(Variable, location, year, gbd_location_id, iso_code, iso_num_code, region, dah_eligible, sdi, the_per_cap_mean, ghes_per_the_mean, 
                        dah_per_cap_ppp_mean, haqi, cpi, perc_skill_attend, imm_pop_perc, perc_urban, result)

# save as csv file
write.csv(full_codebook, file= paste0(g_drive, "Data/documentation/codebooks/aim_2/vaccine_index_variable_codebook_for_website.csv"), 
          row.names = FALSE, na="")