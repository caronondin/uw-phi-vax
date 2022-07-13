# Purpose: Create visuals to inspect data
# Author: Francisco Rios 
# Date: July 12, 2022

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/second_version/01_set_up_R.R"))

# Set up
library(ggcorrplot)
library(vtable)

# load data sets
dt1 <- as.data.table(readRDS(file=paste0(prepped_data_dir, "aim_2/13_merged_dataset_second_version.RDS"))) # merged data set with missing values
dt2 <- as.data.table(readRDS(file=paste0(prepped_data_dir, "aim_2/14_prepped_data_for_analysis_second_version.RDS"))) # prepped data set with full series and dropped rows

# Load "codeTable" for easy labeling
codeTable <- as.data.table(read_xlsx(path=paste0(codebook_directory, "vaccine_index_variable_codebook.xlsx")))
labelTable <- unique(codeTable[,.(Variable, Label)])

# Reshape each transformed data for plotting
idVars <- unique(codeTable$Variable)[1:7]
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
outputFile11 <- paste0(visDir, "/aim_2/second_version/03_histograms_of_data.PDF")
print(paste('Saving:', outputFile11))

pdf(outputFile11, height=5.5, width=9)

for(i in seq(length(histograms1))) {
  print(histograms1[[i]])
  print(histograms2[[i]])
}
dev.off()

# add vaccine coverage data and compute correlation matrix for full dataset-----
# load vaccination coverage dataset
vax_data <- readRDS(paste0(prepped_data_dir, "aim_1/01_vaccine_trends.RDS"))

# reshape the vaccination data
vax_data <- pivot_wider(vax_data,
                        id_cols=c("location_id", "location_name", "year_id"),
                        names_from = "vaccine_name",
                        values_from = c(prop_val, prop_upper, prop_lower))  %>% 
  select(location_id, year_id, location_name, prop_val_BCG, prop_val_DTP1, prop_val_DTP3, prop_val_MCV1, prop_val_MCV2, prop_val_Pol3, 
         prop_val_RCV1, prop_val_RotaC)

# merge the data set with the vaccination data
corr_data <- dt2 %>% left_join(vax_data, by=c("gbd_location_id"="location_id", "year"="year_id", "location"="location_name"))

corr <- round(cor(corr_data[,8:24], use="complete.obs"), 1)
corrplot <- ggcorrplot(corr,
                       ggtheme = ggplot2::theme_gray,
                       outline.color = "white",
                       # type = "upper",
                       colors = c("#6D9EC1", "white", "#E46726"),
                       lab = TRUE,
                       title = "Correlation of variables ")

# Save correlations on a PDF
outputFile11b <- paste0(visDir, "aim_2/second_version/04_correlation_matrix.PDF")
pdf(outputFile11b, height=11, width=11)
corrplot
dev.off()

# view summary statistics for all variables
st(dt1, file=paste0(visDir, "aim_2/second_version/05_descriptive_stats_merged_data.PDF"))
st(dt2, file=paste0(visDir, "aim_2/second_version/06_descriptive_stats_extrapolated_data.PDF"))

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
write.csv(miss_table_reformat, file=paste0(visDir, "aim_2/second_version/07_missingness_in_merged_data.csv"))

# Compare which locations have been dropped
all_locs <- unique(dt1$location)
sub_locs <- unique(dt2$location)
dropped_locs <- setdiff(all_locs, sub_locs)

# Save vector of locations that were dropped due to missing data
write.csv(dropped_locs, file = paste0(visDir, "aim_2/second_version/08_dropped_locations.csv"))

# find locations that are missing info on skilled birth attendants
skill_att_miss <- dt1 %>% select(location, year, gbd_location_id, iso_code, iso_num_code, perc_skill_attend)
skill_att_miss <- pivot_wider(skill_att_miss, id_cols = c(location, gbd_location_id, iso_code, iso_num_code), values_from = perc_skill_attend, names_from = year)
skill_att_miss$total_missing <- rowSums(is.na(skill_att_miss))
skill_att_miss <- skill_att_miss %>% filter(total_missing>=31)

# save csv of locations that do not have any data on skilled birth attendants
write.csv(skill_att_miss, file = paste0(visDir, "aim_2/second_version/09_locations_missing_skill_birth_attendants.csv"))
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
write.csv(miss_table_reformat2, file=paste0(visDir, "aim_2/second_version/10_missingness_in_merged_data_without_locations_dropped.csv"))

##############################################
# Create plot of DAH trends among locations 
# eligible to receive such funds
##############################################

# load list of ineligible locations for DAH
# ineligible <- readRDS(file=paste0(codebook_directory, "locations_ineligible_for_dah.RDS"))

# create two data sets one with original data with missing values and one with imputed values
# dahdata1 <- plotdt1 %>% filter(!location%in%ineligible) %>% filter(variable=="dah_per_cap_ppp_mean")
# dahdata2 <- plotdt2 %>% filter(!location%in%ineligible) %>% filter(variable=="dah_per_cap_ppp_mean")
# 
# # dahVar <- unique(test$variable)
# 
# # plot time series of the data for DAH with ineligible locations dropped
# dahlist1 <- list()
# dahlist2 <- list()
# 
# i <- 1
# for(h in unique(dahdata1$location)) {
#   dahlist1[[i]] <- ggplot(dahdata1[location==h], aes_string(y='value', x='year')) + 
#     geom_point() + labs(title = paste0(h), subtitle = "Among original data")
#   i = i +1
# }
# 
# i <- 1
# for(h in unique(dahdata2$location)) {
#   dahlist2[[i]] <- ggplot(dahdata2[location==h], aes_string(y='value', x='year')) + 
#     geom_point() + labs(title = paste0(h), subtitle = "Among complete series data")
# }
# 
# outputFile11c <- paste0(visDir, "aim_2/12_dah_data_among_eligible_countries.PDF")
# pdf(outputFile11c, height=5.5, width=9)
# for (i in 1:length(dahlist1)){
#   print(dahlist1[[i]])
# }
# for (i in 1:length(dahlist2)) {
#   print(dahlist2[[i]])
# }
# dev.off()
# 
# # explore how often 0 appears in locations eligible for DAH funding
# zerodah <- dahdata1 %>% filter(value==0) %>% group_by(location) %>% count(location)

# save csv file of zerodah
# write.csv(zerodah, file = paste0(visDir, "aim_2/13_locations_with_value_0.csv"))

# plot pdf 
# summary(test$dah_per_cap_ppp_mean)



# explore a few variables that could serve as proxies to vaccine confidence


