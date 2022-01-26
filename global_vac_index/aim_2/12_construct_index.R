# Purpose: Final transformations of data before calculating index
# Author: 
# Date: Dec 13, 2021

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

# Load final data for analysis
final_data <- readRDS(paste0(prepped_data_dir, "aim_2/09_prepped_data_for_analysis.RDS"))

# subset from 1995 to 2020
# final_data <- final_data %>% filter(year>1995)

# Normalize to ensure all values are between 0 and 1
normVars = c('the_per_cap_mean', 'dah_per_the_mean_cat')

norm_cut <- read_xlsx(paste0(codebook_directory, "vaccine_index_normalizations_cutoffs.xlsx"))

i <- 1
for (i in 1:length(normVars)) {
  min = norm_cut$min[i]
  max = norm_cut$max[i]
  v = norm_cut$variable[i]
  final_data[, (v):=(get(v)-min)/(max-min)]
}

# calculate geometric mean of all index variables
# Calculate the geometric mean
new.col <- apply(final_data[,6:17], 1, prod)
result <- as.data.frame(new.col)
final_data$result <- result

n <- ncol(final_data[,6:17])
final_data$result <- final_data$result^(1/n)

hist(final_data$result)

# Save final results
saveRDS(final_data, file=paste0(prepped_data_dir, "aim_2/10_index_results.RDS"))
