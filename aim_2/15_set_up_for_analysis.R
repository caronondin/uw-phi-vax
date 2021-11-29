# set up for analyses
# might be necessary to do this first

# Load useful packages
library(mice)
library(VIM)
library(car)
library(psych)

# Load prepped and merged data
dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))

# subset data to specific time frame
dt <- dt %>% filter(year < 2020 & year > 1990)


# investigate missingness
# mice_plot <- aggr(dt1, col=c('navyblue', 'yellow'),
#                   numbers=TRUE, sortVars=TRUE)


# ensure all variables have a complete time series
imputed_Data <- mice(dt, m=5, maxit = 50, method = 'pmm', seed = 500)

# get first completed dataset to observe trends
completeDT <- complete(imputed_Data, 1)

# save copy of untransformed data
untransformed <- copy(completeDT)

dt2 <- as.data.table(completeDT)

# Variables requiring inverse transformation
dt2[,c(8, 10, 11, 18)] <- 1/dt2[, c(8, 10, 11, 18)]

# Variables that require log transformation to achieve normality
dt2[,c(7:19)] <- log(1000*dt2[, c(7:19)])

# Load "codeTable" for easy labeling
codeTable <- as.data.table(read_xlsx(path=paste0(codebook_directory, "vaccine_index_variable_codebook.xlsx")))
labelTable <- unique(codeTable[,.(Variable, Label)])

# Make histogram of all the variables
idVars <- unique(codeTable$Variable)[1:5]
df2 <- melt(dt2, id.vars = idVars, variable.name = 'variable')
indexVars <- unique(df2$variable)

histograms = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(df2[variable==v], aes(value)) + 
    geom_histogram() + 
    # facet_wrap(~vaccine_name) + 
    labs(title = paste('Histograms of', l), y = 'Value', x = l) + 
    theme_minimal()
})

# print(paste('Saving:', outputFile4c)C)
outputFile15 <- paste0(visDir, "aim_2/transformed_data_exploration.pdf")
pdf(outputFile15, height=5.5, width=9)

for(i in seq(length(histograms))) { 
  print(histograms[[i]])
  # print(histograms_untr[[i]])
}
dev.off()

# Use DT2 to drop columsn we don't need:
dt2 <- unlist(dt2[,-c(6,11)])

# Calcualte geometric mean
dt2$result <- dt2[, 6:7]

dt2$Result <- apply(dt2, 1, function(x) (prod(x[x!=0]))^(1/sum(x!=0)))
dt2[, v1 := Reduce(`+`, lapply(.SD, function(x) x!=0)), .SDcols = 6:18]
dt2[, result2 := round((Reduce(`*`, lapply(.SD, function(x) 
  replace(x, x==0, 1))))^(1/v1), 2), .SDcols = 6:18][, v1 := NULL][]



# refers to variables that are proportions
# complVars = c('perc_skil_attend', 
              # 'imm_pop_perc', 'perc_urban', 'prim_school_complt')

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

# # transform inverse variables to reciprocal
# inverseTransform = function(x) {
#   1/x
# }
# 
# for (v in invVars) {
#   dt2[, (v):=inverseTransform(get(v))]
# }
# 
# numVars <- c('haqi')
# 
# logTransform = function(x) {
#   log(x)
# }
# 
# for (v in numVars) {
#   dt2[get(v), (v):=1000*v]
#   dt2[, (v):=logTransform(get(v))]
# }

# extrapolate where necessary using GLM

# Data transformations

## including: logit transformation, log-tranformation