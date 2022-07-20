# Purpose: Final transformations of data before calculating index
# Author: 
# Date: July 12, 2022

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/first_version/01_set_up_R.R"))

# Load final data for analysis
final_data <- readRDS(paste0(prepped_data_dir, "aim_2/10_prepped_data_for_analysis.RDS"))

# Normalize to ensure all values are between 0 and 1
normVars = c('the_per_cap_mean', 'dah_per_cap_ppp_mean')

norm_cut <- read_xlsx(paste0(codebook_directory, "vaccine_index_normalizations_cutoffs.xlsx"))

i <- 1
for (i in 1:length(normVars)) {
  min = norm_cut$min[i]
  max = norm_cut$max[i]
  v = norm_cut$variable[i]
  final_data[, (v):=(get(v)-min)/(max-min)]
}

# Calculate the geometric mean--some locations only utilize 8 variables and some utilize 9
final_data <- final_data %>% 
  mutate(n_for_geo_mean = case_when(
    dah_eligible==TRUE  ~ 9,
    dah_eligible==FALSE ~ 8))

final_data$product <- NA

for (i in 1:nrow(final_data)){
  if (final_data$dah_eligible[i]==TRUE){
    final_data$product[i] <- apply(final_data[,8:16][i], 1, prod)
  } else {
    final_data$product[i] <- apply(final_data[,c(8:10,12:16)][i], 1, prod)}
}

# calculate index by taking the nth root
final_data$result <- final_data$product^(1/final_data$n_for_geo_mean)

# drop values for 2020 since those are based almost entirely on imputed data
final_data <- final_data %>% filter(year<=2019)

hist(final_data$result)

# select columns of interest
final_data <- final_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code,
                                    region, dah_eligible, sdi, the_per_cap_mean, ghes_per_the_mean,
                                    dah_per_cap_ppp_mean, haqi, cpi, perc_skill_attend, imm_pop_perc,
                                    perc_urban, result)

# Save final results
saveRDS(final_data, file=paste0(prepped_data_dir, "aim_2/11_index_results.RDS"))
