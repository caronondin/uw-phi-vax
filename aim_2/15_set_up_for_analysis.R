# set up for analyses
# might be necessary to do this first

# Load prepped and merged data
dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))

# subset data to specific time frame
dt1 <- dt %>% filter(year < 2020 & year > 1990)

# impute missing values
library(mice)

# ensure all variables have a complete time series


# extrapolate where necessary using GLM

# Data tranformations

## including: logit transformation, log-tranformation