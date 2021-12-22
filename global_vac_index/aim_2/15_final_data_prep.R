# Purpose: Final transformations of data before calculating index
# Author: 
# Date: Dec 13, 2021

# library packages
library(mice)
library(GGally)

# Load the imputed data set
imputed_Data <- readRDS(file=paste0(prepped_data_dir, "aim_2/imputed_data_list.RDS"))

# get first completed dataset to observe trends
completeDT <- complete(imputed_Data, 1)

# save copy of untransformed first completed data set 
untransformed <- copy(completeDT)
untransformed <- as.data.table(untransformed)

# save file as data
data <- as.data.table(completeDT)

# Drop columns that won't be used
data <- data[,-c(12,19,20)]

# Variable transformations

# Change direction of the variables
invVars = c('cpi', 'mig_rate', 'imm_pop_perc')

# transform inverse variables to reciprocal
inverseTransform = function(x) {
  1/x
}

for (v in invVars) {
  data[, (v):=inverseTransform(get(v))]
}

# Normalize to ensure all values are between 0 and 1
allVars = names(data)[6:17]

norm_cut <- read_xlsx(paste0(codebook_directory, "vaccine_index_normalizations_cutoffs.xlsx"))

i <- 1
for (i in 1:length(allVars)) {
  min = norm_cut$min[i]
  max = norm_cut$max[i]
  v = norm_cut$variable[i]
  data[, (v):=(get(v)-min)/(max-min)]
}

# check multicolinearity in the data
X<-data[,6:21]

ggpairs(X)


# calculate the geometric mean
new.col <- apply(data[,6:18], 1, prod)
result <- as.data.frame(new.col)
data$result <- result

n <- 13
data$result <- data$result^(1/n)

hist(data$result)

# Final data checks

# colinearity
X<-data[,6:18]
library(GGally)
ggpairs(X)


# Check geometric mean assumptions

# plot new data explorations based on these normalizations (such as correlation and histograms and data summaries)
# archive the old data explorations


# double check if percent urban variable is off! why is China showing as 0 percent urban...
# 
# 
# # Add a constant to variable requiring it
# data$mig_rate <- data$mig_rate+71.787
# data$perc_urban <- data$perc_urban+1
# 
# # ln transformation (multiply by constant to avoid negative numbers)
# lnVars = names(data)[grepl('cpi|hc62_g_gghed|imm_pop_perc', names(data))]
# 
# for (v in lnVars) {
#   data[, (v):= log(1000*get(v))]
# }
# 
# # sqrt transformation 
# sqrtVars = names(data)[grepl('mig_rate|hc62_che', names(data))]
# 
# for (v in sqrtVars) {
#   data[, (v):=sqrt(get(v))]
# }
# 
# # cubed transformation
# cubeVars = names(data)[grepl('perc_skil_attend', names(data))]
# 
# for (v in cubeVars) {
#   data[, (v):=get(v)^3]
# }
# # # remove negative numbers
# # negVars <- names(data)[grepl('mig_rate', names(data))]
# # 
# # for (v in negVars) {
# #   data[, (v):=log(1000*get(v))]
# # }
# 
# # calculate the geometric mean
# # df <- data[,6:21]
# new.col <- apply(data[,6:21], 1, prod)
# result <- as.data.frame(new.col)
# data$result <- result
# 
# n <- 16
# data$result <- data$result^(1/n)
# 
# # dummy data
# samples <- c('A','B','C', 'D')
# var1 <- c(3, 5, 2, 5)
# var2 <- c(4, 4, 2, 2)
# var3 <- c(5, 12, 12, 8)
# df <- data.frame(var1,var2,var3,row.names=samples)
# 
# new.col <- apply(df, 1, prod)
# 
# result <- as.data.frame(new.col)
# 
# df$result <- result
# 
# # take the nth rooth of the new result
# n <- 3 # in our index this will be 3
# df$result <- df$result^(1/n)
# 
# 
# # data[,.perc_skil_attend_trans:=6.6-
# #        perc_skil_attend]
# # 
# # data$perc_skil_attend_trans <- 101-data$perc_skil_attend
# # hist(1/data$perc_skil_attend_trans)
# # hist(log(data$perc_skil_attend_trans))
# # hist(sqrt(data$perc_skil_attend_trans))
# 
# # # transform inverse variables to reciprocal
# # inverseTransform = function(x) {
# #   1/x
# # }
# # 
# # for (v in invVars) {
# #   dt2[, (v):=inverseTransform(get(v))]
# # }
# # 
# # numVars <- c('haqi')
# # 
# # logTransform = function(x) {
# #   log(x)
# # }
# # 
# # for (v in numVars) {
# #   dt2[get(v), (v):=1000*v]
# #   dt2[, (v):=logTransform(get(v))]
# # }
# 
# # Variables requiring inverse transformation
# # dt1[,c(8, 10, 11, 18)] <- 1/dt1[, c(8, 10, 11, 18)]
# 
# # refers to variables that are proportions
# # complVars = c('perc_skil_attend', 
# # 'imm_pop_perc', 'perc_urban', 'prim_school_complt')
# 
# # transform completeness variables using approximation of logit that allows 1's and 0's
# # (Smithson et al 2006 Psychological methods "A better lemon squeezer")
# # smithsonTransform = function(x) { 
# #   N=length( x[!is.na(x)] )
# #   logit(((x*(N-1))+0.5)/N)
# # }
# # 
# # for(v in complVars) { 
# #   completedData[get(v)>1, (v):=1]
# #   completedData[, (v):=smithsonTransform(get(v))]
# # }
# 
# 
# # calculate the vaccine index using a geometric mean
# # dt2$result <- dt2[, 6:7]
# # 
# # dt2$Result <- apply(dt2, 1, function(x) (prod(x[x!=0]))^(1/sum(x!=0)))
# # dt2[, v1 := Reduce(`+`, lapply(.SD, function(x) x!=0)), .SDcols = 6:18]
# # dt2[, result2 := round((Reduce(`*`, lapply(.SD, function(x) 
# #   replace(x, x==0, 1))))^(1/v1), 2), .SDcols = 6:18][, v1 := NULL][]