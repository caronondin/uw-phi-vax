# Author: Francisco Rios 
# Purpose: calculate observed percent change for low-SDI countries
# Date: Last modified August 3, 2021

# load data ----
vax_dt <- readRDS(outputFile02)
sdi_dt <- readRDS(outputFile03)

# create vectors of variable names to make sure data sets align
vax_concat <- paste0(vax_dt$location_name)
sdi_concat <- paste0(sdi_dt$location_name)
unmapped_loc <- vax_dt[!vax_concat%in%sdi_concat]

# check if there are locations missing from the sdi data
if(nrow(unmapped_loc) > 0){
  print(unique(unmapped_loc[, c("location_name"), with = FALSE]))
  print(unique(unmapped_loc$file_name)) #For documentation in the comments above.
  stop("You have location names in the vaccine data that aren't in the SDI data!")
}

# merge the first two data sets together
data <- merge(vax_dt, sdi_dt, by = c("location_name", "location_id", "year_id"), all.x = TRUE)

# subset from year 1990 to present (as this is extent of SDI data)
data <- data[year_id >= 1990]

# Prep data -----

# subset data to include only values of Vaccine coverage between 2014-2019
dt <- data[year_id %in% c('2014', '2019')]

# subset to only low-SDI countries
dt <- dt[sdi_group_present == "low"]

# remove the unnecessary columns
dt <- dt[, c("location_name", "year_id", "vaccine_name", "prop_val")]

# reshape data from long to wide
dt <- dcast(dt, location_name + vaccine_name ~ year_id, value.var = c("prop_val"))

# calculate percent change for each country
dt[, percent_change := ((`2019`-`2014`)/`2014`)*100]

# remove locations without a real number
dt <- dt[!is.na(percent_change)]
dt <- dt[!is.infinite(percent_change)]

# Visualize the results ----

# names of variables that will be created
outputFile04_01 <- paste0(visDir, "aim_1/high_performing_locs/01_outliers_low_sdi.PDF")
outputFile04_02 <- paste0(visDir, "aim_1/high_performing_locs/02_barplot_low_sdi.PDF")
outputFile04_03 <- paste0(visDir, "aim_1/high_performing_locs/03_detailed_table_low_sdi.PDF")
outputFile04_04 <- paste0(visDir, "aim_1/high_performing_locs/04_timeseries_plots_improvement_low_sdi.PDF")
outputFile04_05 <- paste0(visDir, "aim_1/high_performing_locs/05_timeseries_plots_missing_data_low_sdi.PDF")

# 1. Create box plot to identify outliers -----

# create a function to spot outliers in data
is_outlier <- function(x) {
  return(x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# create a vector of all vaccines in order to make one plot per vaccine
vaccines <- unique(dt$vaccine_name)

# create new outlier variable label for plotting
dt[,outlier:=ifelse(is_outlier(percent_change), location_name, as.numeric(NA)), by = .(vaccine_name)]

# Save file
print(paste('Saving:', outputFile04_01)) 
pdf(outputFile04_01, height = 5.5, width = 9)

# create series of box plots
for (i in 1:length(vaccines)) {
  g <- ggplot(dt[vaccine_name == vaccines[i]], aes(x = vaccine_name, y = percent_change)) +
    geom_boxplot() + 
    theme_bw() +
    labs(title = paste('Change in vaccine coverage (with outliers labeled)'), 
         y = 'Percent Change', 
         x = 'Vaccine', 
         subtitle = paste0('between 2014 and 2019, among low-SDI countries')) + 
    geom_text_repel(aes(label = outlier), na.rm =TRUE, hjust = -0.3)
  print(g)
}

dev.off()

# 2. Create bar plot to compare countries -----
dt2 <- copy(dt)

# create dataframe of all locations that saw greater than expected values
dt2 <- dt2[!is.na(outlier), .N, by = .(outlier)]

# sort by outlier total
dt2 <- dt2[order(dt2$N), ]

# factor variable for plotting
dt2$outlier <- factor(dt2$outlier, levels = dt2$outlier[order(dt2$N)])

# cretae bar plot
h <- ggplot(data = dt2, aes(x = outlier, y = N)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label=N), hjust=1.6, color="white") +
  theme_minimal(base_size = 12) + 
  labs(title = paste('Countries that exceeded median improvement in vaccine coverage'), 
       y = 'Number of vaccines that saw greater-than-expected improvement', 
       x = 'Location', 
       subtitle = paste0('between 2014 and 2019, among low-SDI countries'))

# save pdf of bar plot
pdf(outputFile04_02, height = 9, width = 11)
print(h)
dev.off()

# 3. Make detailed tables with percent change ----

# copy dataset with percent change in it
median <- copy(dt)

# calculate median percent change for each vaccine in the 2014-2019 period
median[,median:=median(percent_change), by = .(vaccine_name)]

# subset table to relevant/unique values
median <- median[, .(vaccine_name, median)]
median <- unique(median) # save unique values

# add variable name indicator as location_name to facilitate merge
median$location_name <- "Median value"

# rename median to percent_change to facilitate merge
setnames(median, old = c("median"), new = c("percent_change"))

# create a table of countries showing how values of outliers compare to median values
dt3 <- dt[!is.na(outlier),.(location_name, vaccine_name, percent_change)]

# merge the two together
dt3 <- rbind(dt3, median)

# add median value to dt2 to help with factoring for table
median_order <- data.table(outlier = c("Median value"),
                           N = c(10))
factor_list <- rbind(dt2, median_order)
dt3 <- dt3[order(dt3$location_name), ]

# factor variable for plotting
dt3$location_name <- factor(dt3$location_name, levels = factor_list$outlier[order(factor_list$N)])

# create raster plot of locations
j <- ggplot(dt3, aes(vaccine_name, location_name)) +
  geom_tile(aes(fill = percent_change)) + 
  geom_text(aes(label = round(percent_change, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_tile(data=median, size=1, fill=NA, colour="black") +
  labs(title=paste('Percent change in countries that saw above-average improvement'), y = 'Location', x = 'Vaccine', 
       subtitle=paste0('between 2014 and 2019, among low-SDI countries'))

# save pdf of raster plot
pdf(outputFile04_03, height = 9, width = 11.5)
print(j)
dev.off()

# 4. make time series graphs of countries of interest -----
dt4 <- copy(data)

# create  time series graphs of five locations from low SDI group with greatest improvements:
# LBR, CAF, NGA, VUT, GTM

# save a datset for each country and vaccines that will be plotted to see overall trends
lbr <- dt4[location_name == "Liberia"                  & vaccine_name%in%unique(dt3[location_name == "Liberia"]$vaccine_name)]
caf <- dt4[location_name == "Central African Republic" & vaccine_name%in%unique(dt3[location_name == "Central African Republic"]$vaccine_name)]
nga <- dt4[location_name == "Nigeria"                  & vaccine_name%in%unique(dt3[location_name == "Nigeria"]$vaccine_name)]
vut <- dt4[location_name == "Vanuatu"                  & vaccine_name%in%unique(dt3[location_name == "Vanuatu"]$vaccine_name)]
gtm <- dt4[location_name == "Guatemala"                & vaccine_name%in%unique(dt3[location_name == "Guatemala"]$vaccine_name)]

# merge together the multiple datasets
dt4 <- do.call("rbind", list(lbr, caf, nga, vut, gtm))

# make vector of all the locations
lctns <- unique(dt4$location_name)
labelTable <- unique(dt4[,.(location_name, vaccine_name)])

tsPlots = lapply(seq(length(lctns)), function(g) {
  l = unique(labelTable[location_name == lctns[[g]]]$location_name)
  ggplot(dt4[location_name == lctns[[g]]], aes(y = prop_val, x = year_id, color = vaccine_name)) + 
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~vaccine_name) + 
    labs(title = paste('Time series of vaccine coverage for', l), y = 'Percent', x = 'Year', 
         subtitle = paste('vaccines with most improvement in low-SDI locations')) + 
    theme_minimal()
})

# Save file
print(paste('Saving:', outputFile04_04)) 
pdf(outputFile04_04, height = 5.5, width = 9)
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}
dev.off()

# 5. make time series graphs of countries with missing values in 2014 as an extra check -----
dt5 <- copy(data)
dt5 <- dt5[year_id == 2014 & prop_val == 0]

missing <- unique(dt5[sdi_group_present == "low",.(location_name, vaccine_name)])

msgPlots = lapply(seq(nrow(missing)), function(g) {
  ggplot(data[location_name == missing$location_name[[g]] & vaccine_name == missing$vaccine_name[[g]]], aes(y = prop_val, x = year_id, color = vaccine_name)) + 
    geom_line(size = 1, alpha = .8) +
    labs(title = paste('Time series of', missing$vaccine_name[[g]],  'vaccine coverage for', missing$location_name[[g]]), 
         y = 'Percent', 
         x = 'Year', 
         subtitle = paste('low-SDI locations that had 0% coverage in 2014')) + 
    theme_minimal() + 
    ylim(0, 1)
})

# Save file
print(paste('Saving:', outputFile04_05)) 
pdf(outputFile04_05, height = 5.5, width = 9)
par(mfrow = c(2,2))
for(i in seq(length(msgPlots))) { 
  print(msgPlots[[i]])
}
dev.off()
