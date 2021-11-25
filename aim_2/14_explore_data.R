# Author: Francisco Rios 
# Purpose: Crete visualizations to explore data
# Date: Last modified November 15, 2021

library(GGally)
library(PerformanceAnalytics)
library(naniar)

# Load prepped and merged data
dt <- read_rds(paste0(prepped_data_dir, "aim_2/12_merged_dataset.RDS"))

# Load "codeTable" for easy labeling
codeTable <- as.data.table(read_xlsx(path=paste0(codebook_directory, "vaccine_index_variable_codebook.xlsx")))

# Set up to graph
n = 6
set.seed(1)
hzs = sample(dt$location, n)
sample = dt[location %in% hzs]


# re-shape sample data to facilitate plotting
idVars <- unique(codeTable$Variable)[1:5]
df <- melt(sample, id.vars = idVars, variable.name = 'variable')

indexVars <- unique(df$variable)

# Set up time series graphs
timePlots = lapply(indexVars, function(v) {
  l = codeTable[Variable==v]$Label
  ggplot(df[variable==v], aes(x=year, y = value)) + 
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~location, scales='free') +
    labs(title=paste('Timeseries of', l), y='Frequency', x=l, 
         subtitle=paste('Random Sample of', n, 'Locations'),
         caption='Variables are pre-transformation.') + 
    theme_bw()
})

labelTable <- unique(codeTable[,.(Variable, Label)])

# Make histogram of all the variables
df2 <- melt(dt, id.vars = idVars, variable.name = 'variable')

histograms = lapply(indexVars, function(v) {
  l = labelTable[Variable==v]$Label
  ggplot(df2[variable==v], aes(value)) + 
    geom_histogram() + 
    # facet_wrap(~vaccine_name) + 
    labs(title = paste('Histograms of', l), y = 'Value', x = l) + 
    theme_minimal()
})

# # correlations of variables

# cor_data <- dt[, c(6:18)]
# cor(cor_data, use="complete.obs")
# 
# chart.Correlation(cor_data, histogram=TRUE, pch=19)
# 
# vis_miss(cor_data)

# see which years are most prominent overall
# completedt <- df2
completedt <- df2 %>% filter(!is.na(value) & year<2020)
hist(completedt$year)
boxplot(completedt$year)

# Save graphics in appropriate folders
# --------------------------------
# Save file
# print(paste('Saving:', outputFile4c)C)
outputFile14a <- paste0(visDir, "aim_2/data_exploration.pdf")
pdf(outputFile14a, height=5.5, width=9)
for(i in seq(length(timePlots))) { 
  print(timePlots[[i]])
}


for(i in seq(length(histograms))) { 
  print(histograms[[i]])
  # print(histograms_untr[[i]])
}
dev.off()

# indexVars <- pull(codeTable, Variable)
# indexVars <- indexVars[6:18]
# indexVars <-

# seq(length(lctns))unique(codeTable$Variable)[1:5]


# ggplot(df, aes(year, value)) + geom_line(aes(colour='location')) + facet_grid(variable ~.)
# ggplot(df, aes(year, value)) + geom_line() + facet_grid(variable ~ .)





# corPlots = lapply(seq(length(varGroups)), function(g) {
#   l = nodeTable[variable==lhsVars[g]]$label
#   vars = nodeTable[variable %in% varGroups[[g]]]
#   leftout = varGroups[[g]][!varGroups[[g]] %in% vars$variable]
#   vars = rbind(vars, data.table(variable=leftout, label=leftout), fill=TRUE)
#   ggpairs(sample[, vars$variable, with=FALSE], 
#           title=paste('Correlations between variables related to', l),
#           columnLabels=vars$label, lower=list(continuous='smooth'))
# })

# # Make time series graphs
# make vector of all the locations
# lctns <- unique(dt4$location_name)
# labelTable <- unique(codeTable[,.(Variable, Label)])
# 
# tsPlots = lapply(seq(length(indexVars)), function(g) {
#   l = unique(labelTable[Variable==g]$Label)
#   # l = unique(labelTable[location_name == lctns[[g]]]$location_name)
#   ggplot(dt, aes(y = indexVars[[g]], x = year)) + 
#     geom_line(size = 1, alpha = .8) + 
#     # facet_wrap(~vaccine_name) + 
#     labs(title = paste('Time series of vaccine coverage for', l), y = 'Percent', x = 'Year', 
#          subtitle = paste('vaccines with most improvement in low-SDI locations')) + 
#     theme_minimal()
# })
# # Explore missingness