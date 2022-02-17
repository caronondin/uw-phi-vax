# Purpose: Create visuals to inspect data
# Author: Francisco Rios 
# Date: Dec 13, 2021

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

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
outputFile11 <- paste0(visDir, "/aim_2/03_histograms_of_data.PDF")
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
outputFile11b <- paste0(visDir, "aim_2/04_correlation_matrix.PDF")
pdf(outputFile11b, height=11, width=8.5)
corrplot
dev.off()

# view summary statistics for all variables
st(dt1, file=paste0(visDir, "aim_2/05_descriptive_stats_merged_data.PDF"))
st(dt2, file=paste0(visDir, "aim_2/06_descriptive_stats_extrapolated_data.PDF"))

# Find missigness pattern of original data set (dt) ----
missingness1 <- dt1 %>% filter(between(year, 1995, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.))) 
missingness2 <- dt1 %>% filter(between(year, 2000, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
missingness3 <- dt1 %>% filter(between(year, 2010, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
missingness4 <- dt1 %>% filter(between(year, 2015, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
missingness5 <- dt1 %>% filter(between(year, 2015, 2019)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))

# add new column indicating year range
missingness1$range <- "1995-2020"
missingness2$range <- "2000-2020"
missingness3$range <- "2010-2020"
missingness4$range <- "2015-2020"
missingness5$range <- "2015-2019"

# bind into one table
miss_table <- rbind(missingness1, missingness2, missingness3, missingness4, missingness5)

miss_table <- miss_table %>% pivot_longer(cols = !range, names_to = "variable", values_to = "missingness")

# reshape miss_table to be long
miss_table_reformat <- miss_table %>% pivot_wider(names_from = range, values_from=missingness)
write.csv(miss_table_reformat, file=paste0(visDir, "aim_2/07_missingness_in_merged_data.csv"))

# Compare which locations have been dropped
all_locs <- unique(dt1$location)
sub_locs <- unique(dt2$location)
dropped_locs <- setdiff(all_locs, sub_locs)

# Save vector of locations that were dropped due to missing data
write.csv(dropped_locs, file = paste0(visDir, "aim_2/08_dropped_locations.csv"))

# find locations that are missing info on skilled birth attendants
skill_att_miss <- dt1 %>% select(location, year, gbd_location_id, iso_code, iso_num_code, perc_skill_attend)
skill_att_miss <- pivot_wider(skill_att_miss, id_cols = c(location, year, gbd_location_id, iso_code, iso_num_code), values_from = perc_skill_attend, names_from = year)
skill_att_miss$total_missing <- rowSums(is.na(skill_att_miss))
skill_att_miss <- skill_att_miss %>% filter(total_missing>=31)

# save csv of locations that do not have any data on skilled birth attendants
write.csv(skill_att_miss, file = paste0(visDir, "aim_2/09_locations_missing_skill_birth_attendants.csv"))
# %>% filter(is.na(perc_skil_attend))
# unique(skill_att_miss$location)

# now calculate the missigness pattern using the original data and dropping these locations
kept_locations_dt <- dt1 %>% filter(!location %in% dropped_locs)

# Find missigness pattern of original data set (dt) ----
missingness1 <- kept_locations_dt %>% filter(between(year, 1995, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.))) 
missingness2 <- kept_locations_dt %>% filter(between(year, 2000, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
missingness3 <- kept_locations_dt %>% filter(between(year, 2010, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
missingness4 <- kept_locations_dt %>% filter(between(year, 2015, 2020)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
missingness5 <- kept_locations_dt %>% filter(between(year, 2015, 2019)) %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))

# add new column indicating year range
missingness1$range <- "1995-2020"
missingness2$range <- "2000-2020"
missingness3$range <- "2010-2020"
missingness4$range <- "2015-2020"
missingness5$range <- "2015-2019"

# bind into one table
miss_table_2 <- rbind(missingness1, missingness2, missingness3, missingness4, missingness5)

miss_table_2 <- miss_table_2 %>% pivot_longer(cols = !range, names_to = "variable", values_to = "missingness")

# reshape miss_table to be long
miss_table_reformat2 <- miss_table_2 %>% pivot_wider(names_from = range, values_from=missingness)
write.csv(miss_table_reformat2, file=paste0(visDir, "aim_2/10_missingness_in_merged_data_without_locations_dropped.csv"))

# plot DAH trends for locations that are eligible to receive DAH funds
# load list of ineligible locations for DAH
ineligible <- readRDS(file=paste0(codebook_directory, "locations_ineligible_for_dah.RDS"))

test <- plotdt1 %>% filter(!location%in%ineligible) %>% filter(variable=="dah_per_cap_ppp_mean")
dahVar <- unique(test$variable)

# plot time series of the data for DAH with ineligible locations dropped
dahlist <- list()
i <- 1
for(h in unique(test$location)) {
  dahlist[[i]] <- ggplot(test[location==h], aes_string(y='value', x='year')) + 
    geom_point() + labs(title = paste0(h))
  i = i +1
  }

outputFile11c <- paste0(visDir, "aim_2/12_dah_data_among_eligible_countries.PDF")
pdf(outputFile11c, height=5.5, width=9)
for (i in 1:length(dahlist)){
  print(dahlist[[i]])
}
dev.off()

# pltlist3[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point()+ labs(title = paste0(h)) + geom_point(aes(y=tmp),color='red')
# ggplot(test, aes(value)) + geom_histogram()
# histograms1 = lapply(dahVar, function(v) {
#   l = labelTable[Variable==v]$Label
#   ggplot(test[variable==v], aes(value)) +
#     geom_histogram() +
#     labs(title = paste('Histogram of Pre-transformation', l), y = 'Value', x = l,
#          caption='Variables are pre-imputation.') +
#     theme_minimal()
# })

# save pdf

# plot pdf 
summary(test$dah_per_cap_ppp_mean)


