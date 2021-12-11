# set up for analyses
# might be necessary to do this first

# Load useful packages
library(mice)
library(VIM)
library(car)
library(psych)
library(moments)

# Make sure all variables have a complete time series -----

# impute data once more or read in previously imputed data
impute_new <- FALSE

if (impute_new==TRUE){
  
  # Load prepped and merged data
  dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))
  
  # subset data to specific time frame
  dt <- dt %>% filter(year < 2020 & year > 1990)
  
  # ensure all variables have a complete time series
  imputed_Data <- mice(dt, m=5, maxit = 50, method = 'pmm', seed = 500)
  
  # save imputed data set in prepped data folder
  saveRDS(imputed_Data, file=paste0(prepped_data_dir, "aim_2/imputed_data_list.RDS"))
  
  # get first completed dataset to observe trends
  completeDT <- complete(imputed_Data, 1)
  
  # save copy of untransformed first completed data set 
  untransformed <- copy(completeDT)
  untransformed <- as.data.table(untransformed)
  
} else {
  imputed_Data <- read_rds(paste0(prepped_data_dir, "aim_2/imputed_data_list.RDS"))
  
  # get first completed dataset to observe trends
  completeDT <- complete(imputed_Data, 1)
  
  # save copy of untransformed first completed data set 
  untransformed <- copy(completeDT)
  untransformed <- as.data.table(untransformed)
}

# Test transformation 1: natural log transformation -----
dt1 <- as.data.table(completeDT)
dt1[,c(6:19)] <- log(dt1[, c(6:19)])

# Test transformation 2: multiply by 1000 then log transformation -----
dt2 <- as.data.table(completeDT)
dt2[,c(6:19)] <- log(1000*dt2[, c(6:19)])

# Test transformation 3: sqrt transformation -----
dt3 <- as.data.table(completeDT)
dt3[,c(6:19)] <- sqrt(dt3[, c(6:19)])

# Test transformation 4: squared transformation -----
dt4 <- as.data.table(completeDT)
dt4[,c(6:19)] <- (dt3[,c(6:19)]^2)

# Fourth transformation: cumulative, log, logit and lag. ----
# pending

# Load "codeTable" for easy labeling
codeTable <- as.data.table(read_xlsx(path=paste0(codebook_directory, "vaccine_index_variable_codebook.xlsx")))
labelTable <- unique(codeTable[,.(Variable, Label)])

# Reshape each transformed data for plotting
idVars <- unique(codeTable$Variable)[1:5]
plotdt1 <- melt(dt1, id.vars = idVars, variable.name = 'variable')
plotdt2 <- melt(dt2, id.vars = idVars, variable.name = 'variable')
plotdt3 <- melt(dt3, id.vars = idVars, variable.name = 'variable')
plotdt4 <- melt(dt4, id.vars = idVars, variable.name = 'variable')

indexVars <- unique(plotdt1$variable)

# Transformed data used to create index
histograms1 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt1[variable==v], aes(value)) + 
    geom_histogram() +
    labs(title = paste('Histogram of', l), y = 'Value', x = l,
         caption='Variables are post-transformation. Transformation: 
			natural log.') + 
    theme_minimal()
})

histograms2 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt2[variable==v], aes(value)) + 
    geom_histogram() +
    labs(title = paste('Histogram of', l), y = 'Value', x = l,
         caption='Variables are post-transformation. Transformation: 
			values multiplied by 1000, and natural log.') + 
    theme_minimal()
})

histograms3 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt3[variable==v], aes(value)) + 
    geom_histogram() +
    labs(title = paste('Histogram of', l), y = 'Value', x = l,
         caption='Variables are post-transformation. Transformation: 
			square root.') + 
    theme_minimal()
})

histograms4 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt4[variable==v], aes(value)) + 
    geom_histogram() +
    labs(title = paste('Histogram of', l), y = 'Value', x = l,
         caption='Variables are post-transformation. Transformation: 
			squared.') + 
    theme_minimal()
})

# Reshape untransformed data for plotting
plot_untr_dt <- melt(untransformed, id.vars = idVars, variable.name = 'variable')

# Untransformed data
histograms_untr = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plot_untr_dt[variable==v], aes(value)) +
    geom_histogram() +
    labs(title = paste('Histograms of untransformed', l), y = 'value', x =l,
         caption='Variables are pre-transformation.')+
    theme_minimal()
})

# QQ Plot each of the transformations -----

# Plot the untransformed variables
for (id in unique(plot_untr_dt$variable)){
  sub_Data <- plot_untr_dt[which(plot_untr_dt$variable == id), ]$value
  
  pdf(paste0(visDir, "aim_2/qqplots_untransformed_data/untr", id, ".pdf"))
  qqnorm(sub_Data, main=paste("Variable =", id))
  qqline(sub_Data, col="red", lty =2, lwd = 3)
  dev.off()
}

# Log transformations
for (id in unique(plotdt1$variable)){
  sub_Data <- plotdt1[which(plotdt1$variable == id), ]$value
  
  pdf(paste0(visDir, "aim_2/qqplots_transformed_data/1_natural_log/ln_log_", id, ".pdf"))
  qqnorm(sub_Data, main=paste("Variable =", id))
  qqline(sub_Data, col="red", lty =2, lwd = 3)
  dev.off()
}

# Multiplied by 1000 then natural Log transformations
for (id in unique(plotdt2$variable)){
  sub_Data <- plotdt2[which(plotdt2$variable == id), ]$value
  
  pdf(paste0(visDir, "aim_2/qqplots_transformed_data/2_1000_natural_log/1000ln_log_", id, ".pdf"))
  qqnorm(sub_Data, main=paste("Variable =", id))
  qqline(sub_Data, col="red", lty =2, lwd = 3)
  dev.off()
}

# Square root transformations
for (id in unique(plotdt3$variable)){
  sub_Data <- plotdt3[which(plotdt3$variable == id), ]$value
  
  pdf(paste0(visDir, "aim_2/qqplots_transformed_data/3_square_root/sqrt_", id, ".pdf"))
  qqnorm(sub_Data, main=paste("Variable =", id))
  qqline(sub_Data, col="red", lty =2, lwd = 3)
  dev.off()
}

# Squared transformations
for (id in unique(plotdt4$variable)){
  sub_Data <- plotdt4[which(plotdt4$variable == id), ]$value
  
  pdf(paste0(visDir, "aim_2/qqplots_transformed_data/4_squared/sqrd_", id, ".pdf"))
  qqnorm(sub_Data, main=paste("Variable =", id))
  qqline(sub_Data, col="red", lty =2, lwd = 3)
  dev.off()
}

# this could be rewritten into a loop perhaps that will populate a table?
skewness_table <- data.table(
  variable = indexVars,
  skewness_untr = c(NA),
  skewness_ln = c(NA),
  skewness_1000ln = c(NA), 
  skewness_sqrt = c(NA),
  skewness_sqrd = c(NA)
  )

# Use a loop to calculate the skewness of each variable
i <- 1
for (i in i:length(indexVars)){
  l <- indexVars[i]
  skewness_table$skewness_untr[i] <- skewness(plot_untr_dt[variable==l]$value)
  skewness_table$skewness_ln[i] <- skewness(plotdt1[variable==l]$value)
  skewness_table$skewness_1000ln[i] <- skewness(plotdt2[variable==l]$value)
  skewness_table$skewness_sqrt[i] <- skewness(plotdt3[variable==l]$value)
  skewness_table$skewness_sqrd[i] <- skewness(plotdt4[variable==l]$value)
}

# Save the skewness table values 
write.csv(skewness_table, file = paste0(visDir, "aim_2/skewness_table.csv"))

# print(paste('Saving:', outputFile4c)C)
outputFile15 <- paste0(visDir, "aim_2/transformed_data_exploration.pdf")
pdf(outputFile15, height=5.5, width=9)

for(i in seq(length(histograms_untr))) { 
  print(histograms_untr[[i]])
  print(histograms1[[i]])
  print(histograms2[[i]])
  print(histograms3[[i]])
  print(histograms4[[i]])
}
dev.off()

# Drop columns that won't be used

# Use DT2 to drop columsn we don't need:
# dt2 <- unlist(dt2[,-c(6,11)])

# Calcualte geometric mean
# dt2$result <- dt2[, 6:7]
# 
# dt2$Result <- apply(dt2, 1, function(x) (prod(x[x!=0]))^(1/sum(x!=0)))
# dt2[, v1 := Reduce(`+`, lapply(.SD, function(x) x!=0)), .SDcols = 6:18]
# dt2[, result2 := round((Reduce(`*`, lapply(.SD, function(x) 
#   replace(x, x==0, 1))))^(1/v1), 2), .SDcols = 6:18][, v1 := NULL][]
# 
# # calculate skewness in r
# # install.packages("moments")
# 
# # Variables requiring inverse transformation
# dt1[,c(8, 10, 11, 18)] <- 1/dt1[, c(8, 10, 11, 18)] 


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
# investigate missingness
# mice_plot <- aggr(dt1, col=c('navyblue', 'yellow'),
#                   numbers=TRUE, sortVars=TRUE)





# extrapolate where necessary using GLM

# Data transformations

## including: logit transformation, log-tranformation