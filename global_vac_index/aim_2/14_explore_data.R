# Name: Explore data before final analysis
# Author: Francisco Rios
# Date: Dec 22, 2021

# Load necessary packages
library(mice)
library(VIM)
library(moments)
library(ggcorrplot)
library(vtable)

# Load prepped data
dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))

# subset data to specific time frame
data <- dt %>% filter(between(year, 1990, 2020))

# drop the variables that won't be used in analysis
data <- data %>% select(-c("mig_rate", "perc_urban", "hc62_g_gghed", "gghed_che",
                           "hc62_che", "prim_school_complt", "crude_birth_rate")) # perc_urban, prim_school_complt, crude_birth_rate


# extrapolate where necessary using GLM
numVars <- names(data)[6:17]
i=1
for(v in numVars) {
  for(h in unique(data$location)) {
    i=i+1
    if (!any(is.na(data[location==h][[v]]))) next
    if (!any(!is.na(data[location==h][[v]]))) next
    form = as.formula(paste0(v,'~year'))
    lmFit = glm(form, data[location==h], family='poisson')
    data[location==h, tmp:=exp(predict(lmFit, newdata=data[location==h]))]
    lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
    data[location==h & tmp>lim, tmp:=lim]
    # ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red')
    data[location==h & is.na(get(v)), (v):=tmp]
    pct_complete = floor(i/(length(numVars)*length(unique(data$location)))*100)
    cat(paste0('\r', pct_complete, '% Complete'))
    flush.console()
  }
}
data$tmp = NULL

# Drop variables
prepped_data <- na.omit(data)

# Categorize DAH variable for analysis
# Create new variable
prepped_data$nch_cnv_dah_20_cat <- NA

ineligible <- readRDS(file=paste0(codebook_directory, "locations_ineligible_for_dah.RDS"))
prepped_data <- prepped_data %>%
  mutate(
    nch_cnv_dah_20_cat = case_when(nch_cnv_dah_20==0 & location %in% ineligible ~ "5", # not eligible for funds
                                   nch_cnv_dah_20 >0  & nch_cnv_dah_20 <= 50 ~ "2", # received low percentage of funds
                                   nch_cnv_dah_20 >50 & nch_cnv_dah_20 <5243 ~ "3", # received medium percentage of funds
                                   nch_cnv_dah_20 >= 5243 ~ "4", # received large percentage of overall funds
                                   nch_cnv_dah_20==0 ~ "1")) # eligible for funds but did not receive

# save as numeric variable
prepped_data$nch_cnv_dah_20_cat <- as.numeric(prepped_data$nch_cnv_dah_20_cat)



# Plot variables

# 

# # impute data once more or read in previously imputed data
# impute_new <- FALSE
# 
# if (impute_new==TRUE){
#   
#   # Load prepped and merged data
#   dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))
#   
#   # subset data to specific time frame
#   dt <- dt %>% filter(between(year, 2000, 2019))
#   
#   # ensure all variables have a complete time series
#   imputed_Data <- mice(dt, m=5, maxit = 50, method = 'pmm', seed = 500)
#   
#   # save imputed data set in prepped data folder
#   saveRDS(imputed_Data, file=paste0(prepped_data_dir, "aim_2/imputed_data_list.RDS"))
#   
#   # get first completed dataset to observe trends
#   completeDT <- complete(imputed_Data, 1)
#   
#   # save copy of untransformed first completed data set 
#   untransformed <- copy(completeDT)
#   untransformed <- as.data.table(untransformed)
#   
# } else {
#   # Load prepped and merged data
#   dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))
#   
#   # subset data to specific time frame
#   dt <- dt %>% filter(between(year, 2000, 2019))
#   
#   imputed_Data <- read_rds(paste0(prepped_data_dir, "aim_2/imputed_data_list.RDS"))
#   
#   # get first completed dataset to observe trends
#   completeDT <- complete(imputed_Data, 1)
#   
#   # save copy of untransformed first completed data set 
#   untransformed <- copy(completeDT)
#   untransformed <- as.data.table(untransformed)
# }

# Find missigness pattern of original data set (dt) ----
dt %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))

# drop variable no longer needed
final_data <- prepped_data %>% select(-c("nch_cnv_dah_20"))

# Data transformations -----
# data <- as.data.table(completeDT)

# add constant to migration rate to avoid having 0 
# data$mig_rate <- data$mig_rate+71.787

# Change direction of the variables
invVars = c('cpi', 'imm_pop_perc')

# transform inverse variables to reciprocal
inverseTransform = function(x) {
  1/x
}

for (v in invVars) {
  final_data[, (v):=inverseTransform(get(v))]
}

# Normalize to ensure all values are between 0 and 1
allVars = names(final_data)[c(6:14, 16)]

norm_cut <- read_xlsx(paste0(codebook_directory, "vaccine_index_normalizations_cutoffs.xlsx"))

i <- 1
for (i in 1:length(allVars)) {
  min = norm_cut$min[i]
  max = norm_cut$max[i]
  v = norm_cut$variable[i]
  final_data[, (v):=(get(v)-min)/(max-min)]
}

# Visualize the raw data, imputed data, and transformed data

# Load "codeTable" for easy labeling
codeTable <- as.data.table(read_xlsx(path=paste0(codebook_directory, "vaccine_index_variable_codebook.xlsx")))
labelTable <- unique(codeTable[,.(Variable, Label)])

# Reshape each transformed data for plotting
idVars <- unique(codeTable$Variable)[1:5]
plotdt1 <- melt(data, id.vars = idVars, variable.name = 'variable') # raw data
plotdt2 <- melt(prepped_data, id.vars = idVars, variable.name = 'variable') # imputed data
plotdt3 <- melt(final_data, id.vars = idVars, variable.name = 'variable') # transformed data

indexVars <- unique(plotdt1$variable)

# Plot histograms of the raw data
histograms1 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt1[variable==v], aes(value)) +
    geom_histogram() +
    labs(title = paste('Histogram of Pre-transformation', l), y = 'Value', x = l,
         caption='Variables are pre-imputation.') +
    theme_minimal()
})

# Plot histograms of the imputed data
histograms2 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt2[variable==v], aes(value)) +
    geom_histogram() +
    labs(title = paste('Histogram of Pre-transformation', l), y = 'Value', x = l,
         caption='Variables are post-imputation.') +
    theme_minimal()
})

# Plot histograms of the transformed data
histograms3 = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(plotdt3[variable==v], aes(value)) +
    geom_histogram() +
    labs(title = paste('Histogram of', l), y = 'Value', x = l,
         caption='Variables are post-transformation. Transformation: Variables are normalized between 0 and 1.') +
    theme_minimal()
})

# Create PDF File with relevant visuals
outputFile14 <- paste0(visDir, "/aim_2/histograms_of_data.PDF")
print(paste('Saving:', outputFile14))


pdf(outputFile14, height=5.5, width=9)

for(i in seq(length(histograms1))) {
  print(histograms1[[i]])
  print(histograms2[[i]])
  print(histograms3[[i]])
  
}
dev.off()

# Compute correlation matrix -----
corr <- round(cor(prepped_data[,6:17]), 1)
corrplot1 <- ggcorrplot(corr,
           ggtheme = ggplot2::theme_gray,
           outline.color = "white",
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE,
           title = "Correlation of variables after normalization.")

corr2 <- round(cor(final_data[,6:21]), 1)
corrplot2 <-ggcorrplot(corr,
           ggtheme = ggplot2::theme_gray,
           outline.color = "white",
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE,
           title = "Correlation of variables before normalization.")

# Save correlations on a PDF
outputFile14b <- paste0(visDir, "aim_2/correlation_matrix.PDF")
pdf(outputFile14b, height=11, width=8.5)
corrplot1
corrplot2
dev.off()

# view summary statistics for all variables
# pdf(file = paste0(visDir, "aim_1/missed_opportunities/summary_statistics_prepped_dhs_data.pdf"))
st(data, file=paste0(visDir, "aim_2/descriptive_stas_transformed_imputed_data.PDF"))
st(untransformed, file=paste0(visDir, "aim_2/descriptive_stas_untransformed_imputed_data.PDF"))

# Calculate index -----
# Move this to seperate function afterwards

# Drop variables that won't be used
data <- data %>% select(-c("imm_pop_perc", "mig_rate", "perc_urban", "hc62_g_gghed",
                           "hc62_che", "prim_school_complt", "crude_birth_rate")) # perc_urban, prim_school_complt, crude_birth_rate

# Calculate the geometric mean
new.col <- apply(final_data[,6:16], 1, prod)
result <- as.data.frame(new.col)
final_data$result <- result

n <- ncol(data[,6:16])
final_data$result <- final_data$result^(1/n)

hist(final_data$result)
# Save output -----
# saveRDS(dt, outputFile06)

# ggcorrplot(corr,
#            hc.order = TRUE,
#            type = "lower",
#            outline.color = "white",
#            ggtheme = ggplot2::theme_gray,
#            colors = c("#6D9EC1", "white", "#E46726"))

# Transformed data used to create index
# histograms1 = lapply(indexVars, function(v) {
#   l = labelTable[Variable==v]$Label
#   ggplot(plotdt1[variable==v], aes(value)) + 
#     geom_histogram() +
#     labs(title = paste('Histogram of', l), y = 'Value', x = l,
#          caption='Variables are post-transformation. Transformation: 
# 			natural log.') + 
#     theme_minimal()
# })
# 
# histograms2 = lapply(indexVars, function(v) {
#   l = labelTable[Variable==v]$Label
#   ggplot(plotdt2[variable==v], aes(value)) + 
#     geom_histogram() +
#     labs(title = paste('Histogram of', l), y = 'Value', x = l,
#          caption='Variables are post-transformation. Transformation: 
# 			values multiplied by 1000, and natural log.') + 
#     theme_minimal()
# })
# 
# histograms3 = lapply(indexVars, function(v) {
#   l = labelTable[Variable==v]$Label
#   ggplot(plotdt3[variable==v], aes(value)) + 
#     geom_histogram() +
#     labs(title = paste('Histogram of', l), y = 'Value', x = l,
#          caption='Variables are post-transformation. Transformation: 
# 			square root.') + 
#     theme_minimal()
# })
# print(paste('Saving:', outputFile4c)C)

# outputFile15 <- paste0(visDir, "aim_2/transformed_data_exploration.pdf")
# pdf(outputFile15, height=5.5, width=9)
# 
# for(i in seq(length(histograms_untr))) { 
#   print(histograms_untr[[i]])
#   print(histograms1[[i]])
#   print(histograms2[[i]])
#   print(histograms3[[i]])
#   print(histograms4[[i]])
# }
# dev.off()



# dt %>% 
#   gather(-c(location, year, gbd_location_id, iso_code, iso_num_code))
# 
# df %>%
#   gather(col, value, -line) %>% 
#   group_by(col)
# 
# %>%
#   group_by(col) %>%
#   summarize(missing_share = mean(is.na(value)))
# 
# dt_aggr = aggr(dt, 
#                    col=mdc(1:2),
#                    numbers=TRUE, sortVars=TRUE, labels=names(dt), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
