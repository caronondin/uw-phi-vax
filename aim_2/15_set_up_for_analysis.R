# set up for analyses
# might be necessary to do this first

# Load prepped and merged data
dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))

# subset data to specific time frame
dt1 <- dt %>% filter(year < 2020 & year > 1990)

# impute missing values
library(mice)
library(VIM)
library(car)

# investigate missingness
mice_plot <- aggr(dt1, col=c('navyblue', 'yellow'),
                  numbers=TRUE, sortVars=TRUE)


# ensure all variables have a complete time series
imputed_Data <- mice(dt1, m=5, maxit = 50, method = 'pmm', seed = 500)

# get first completed dataset to observe trends
completedData <- complete(imputed_Data, 1)

# save copy of untransformed data
untransformed <- copy(imputed_Data)

completedData <- as.data.table(completedData)

# refers to variables that are proportions
complVars = c('perc_skil_attend', 
              'imm_pop_perc', 'perc_urban', 'prim_school_complt')

# transform completeness variables using approximation of logit that allows 1's and 0's
# (Smithson et al 2006 Psychological methods "A better lemon squeezer")
# smithsonTransform = function(x) { 
#   N=length( x[!is.na(x)] )
#   logit(((x*(N-1))+0.5)/N)
# }
# 
# for(v in complVars) { 
#   completedData[get(v)>1, (v):=1]
#   completedData[, (v):=smithsonTransform(get(v))]
# }


# extrapolate where necessary using GLM

# Data transformations

## including: logit transformation, log-tranformation