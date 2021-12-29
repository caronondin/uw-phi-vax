# Purpose: Create viuals to inspect data
# Author: Francisco Rios 
# Date: Dec 13, 2021

# Set up
library(ggcorrplot)
library(vtable)

# load data sets
dt1 <- as.data.table(readRDS(file=paste0(prepped_data_dir, "aim_2/08_merged_dataset.RDS"))) # merged data set with missing values
dt2 <- as.data.table(readRDS(file=paste0(prepped_data_dir, "aim_2/09_prepped_data_for_analysis.RDS"))) # prepped data set with full series and dropped rows

# Load "codeTable" for easy labeling
codeTable <- as.data.table(read_xlsx(path=paste0(codebook_directory, "vaccine_index_variable_codebook.xlsx")))
labelTable <- unique(codeTable[,.(Variable, Label)])

# Reshape each transformed data for plotting
idVars <- unique(codeTable$Variable)[1:5]
plotdt1 <- melt(dt1, id.vars = idVars, variable.name = 'variable') # raw data
plotdt2 <- melt(dt2, id.vars = idVars, variable.name = 'variable') # imputed data

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

# Create PDF File with relevant visuals
outputFile11 <- paste0(visDir, "/aim_2/histograms_of_data.PDF")
print(paste('Saving:', outputFile11))

pdf(outputFile11, height=5.5, width=9)

for(i in seq(length(histograms1))) {
  print(histograms1[[i]])
  print(histograms2[[i]])
}
dev.off()

# Compute correlation matrix for full dataset-----
corr <- round(cor(dt2[,6:17]), 1)
corrplot <-ggcorrplot(corr,
                       ggtheme = ggplot2::theme_gray,
                       outline.color = "white",
                       colors = c("#6D9EC1", "white", "#E46726"),
                       lab = TRUE,
                       title = "Correlation of variables ")

# Save correlations on a PDF
outputFile11b <- paste0(visDir, "aim_2/correlation_matrix.PDF")
pdf(outputFile11b, height=11, width=8.5)
corrplot
dev.off()

# view summary statistics for all variables
st(dt1, file=paste0(visDir, "aim_2/descriptive_stats_merged_data.PDF"))
st(dt2, file=paste0(visDir, "aim_2/descriptive_stats_imputed_data.PDF"))

# Find missigness pattern of original data set (dt) ----
missingness <- dt1 %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
write.csv(missingness, file=paste0(visDir, "aim_2/missingness_in_merged_data.csv"))
