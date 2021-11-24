# Author: Francisco Rios 
# Purpose: Crete visualizations to explore data
# Date: Last modified November 15, 2021

# Load prepped and merged data
data <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))

# Create histograms of data to 
hist(data$perc_skil_attend)

# correlations of variables

# Make time series graphs

# Explore missingness

# Explore random set of locations