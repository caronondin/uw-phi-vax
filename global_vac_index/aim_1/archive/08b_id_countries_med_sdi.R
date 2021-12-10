# Author: Francisco Rios 
# Purpose: calculate observed percent change for medium-SDI countries
# Date: Last modified August 3, 2021

# load data
data = readRDS(outputFile3)

##### ##### ##### #####
# save names of files to be created in this script
##### ##### ##### #####
outputFile5f = paste0(aim1_vis, "exemplar_countries/6_outliers_med_sdi.PDF")
outputFile5g = paste0(aim1_vis, "exemplar_countries/7_barplot_med_sdi.PDF")
outputFile5h = paste0(aim1_vis, "exemplar_countries/8_detailed_table_med_sdi.PDF")
outputFile5i = paste0(aim1_vis, "exemplar_countries/9_timeseries_plots_improvement_med_sdi.PDF")
outputFile5j = paste0(aim1_vis, "exemplar_countries/10_timeseries_plots_missing_data_med_sdi.PDF")

##### ##### ##### #####
# data prep
##### ##### ##### #####

# subset data to include only values of Vaccine coverage between 2014-2019
dt <- data[year_id %in% c('2014', '2019')]

# subset to only low-SDI countries
dt <- dt[sdi_group=="medium"]

# # remove the unnecessary columns
dt <- dt[,c("location_name", "year_id", "vaccine_name", "val")]

# reshape data from long to wide
dt <- dcast(dt, location_name + vaccine_name ~ year_id, value.var = c("val"))

# calculate percent change for each country
dt[, percent_change := ((`2019`-`2014`)/`2014`)*100]

# remove locations without a real number
dt <- dt[!is.na(percent_change)]
dt <- dt[!is.infinite(percent_change)]

##### ##### ##### #####
# create box plots 
##### ##### ##### #####

# create a function to spot outliers in data
is_outlier <- function(x) {
  return(x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# create a vector of all vaccines in order to make one plot per vaccine
vaccines = unique(dt$vaccine_name)

# create new outlier variable label for plotting
dt[,outlier:=ifelse(is_outlier(percent_change), location_name, as.numeric(NA)), by=.(vaccine_name)]

# Save file
print(paste('Saving:', outputFile5f)) 
pdf(outputFile5f, height=5.5, width=9)

# create series of boxplots
for (i in 1:length(vaccines)) {
  g <- ggplot(dt[vaccine_name==vaccines[i]], aes(x = vaccine_name, y = percent_change)) +
  geom_boxplot() +
  # geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)+
    theme_bw()+
    labs(title=paste('Change in vaccine coverage (with outliers labeled)'), y='Percent Change', x='Vaccine', 
         subtitle=paste0('between 2014 and 2019, among medium-SDI countries')) + 
    geom_text_repel(aes(label = outlier), na.rm =TRUE, hjust = -0.3)
print(g)
}

dev.off()

##### ##### ##### #####
# create bar plot
##### ##### ##### #####
dt2 <- copy(dt)

# create dataframe of all locations that saw greater than expected values
dt2 <- dt2[!is.na(outlier), .N, by=.(outlier)]

# sort by outlier total
dt2 <- dt2[order(dt2$N), ]

# factor variable for plotting
dt2$outlier <- factor(dt2$outlier, levels = dt2$outlier[order(dt2$N)])

# cretae bar plot
h <- ggplot(data=dt2, aes(x=outlier, y=N)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  coord_flip()+
  geom_text(aes(label=N), hjust=1.6, color="white")+
  theme_minimal(base_size = 12)+
  labs(title=paste('Countries that exceeded median improvement in vaccine coverage'), y='Number of vaccines that saw greater-than-expected improvement', x='Location', 
       subtitle=paste0('between 2014 and 2019, among medium-SDI countries'))

# save pdf of bar plot
pdf(outputFile5g, height=9, width=11)
print(h)
dev.off()

##### ##### ##### #####
# make table
##### ##### ##### #####

# copy dataset with percent change in it
median <- copy(dt)

# calculate median percent change for each vaccine in the 2014-2019 period
median[,median:=median(percent_change), by=.(vaccine_name)]

# subset table to relevant/unique values
median <- median[,.(vaccine_name, median)]
median <- unique(median) # save unique values

# add variable name indicator as location_name to facilitate merge
median$location_name <- "Median value"

# rename median to percent_change to facilitate merge
setnames(median, old=c("median"), new=c("percent_change"))

# create a table of countries showing how values of outliers compare to median values
dt3 <- dt[!is.na(outlier),.(location_name, vaccine_name, percent_change)]

# merge the two together
dt3 <- rbind(dt3, median)

# add median value to dt2 to help with factoring for table
median_order <- data.table(outlier=c("Median value"),
                           N=c(10))
factor_list <- rbind(dt2, median_order)
dt3 <- dt3[order(dt3$location_name), ]

# factor variable for plotting
dt3$location_name <- factor(dt3$location_name, levels = factor_list$outlier[order(factor_list$N)])

# create raster plot of locations
j <- ggplot(dt3, aes(vaccine_name, location_name)) +
  geom_tile(aes(fill = percent_change)) + 
  geom_text(aes(label = round(percent_change, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue")+
  geom_tile(data=median, size=1, fill=NA, colour="black")+
  labs(title=paste('Percent change in countries that saw above-average improvement'), y='Location', x='Vaccine', 
       subtitle=paste0('between 2014 and 2019, among medium-SDI countries'))

# save pdf of raster plot
pdf(outputFile5h, height=9, width=11.5)
print(j)
dev.off()

##### ##### ##### #####
# make trend graphs of countries of interest
##### ##### ##### #####
dt4 <- copy(data)

# create  time series graphs of five locations from medium SDI group with greatest improvements:
# IRQ, NRU, UKR, SYR, GNQ, DOM

# save a dataset for each country and vaccines that will be plotted to see overall trends
irq <- dt4[location_name=="Iraq" & vaccine_name%in%unique(dt3[location_name=="Iraq"]$vaccine_name)]
nru <- dt4[location_name=="Nauru" & vaccine_name%in%unique(dt3[location_name=="Nauru"]$vaccine_name)]
ukr <- dt4[location_name=="Ukraine" & vaccine_name%in%unique(dt3[location_name=="Ukraine"]$vaccine_name)]
syr <- dt4[location_name=="Syrian Arab Republic" & vaccine_name%in%unique(dt3[location_name=="Syrian Arab Republic"]$vaccine_name)]
gnq <- dt4[location_name=="Equatorial Guinea" & vaccine_name%in%unique(dt3[location_name=="Equatorial Guinea"]$vaccine_name)]
dom <- dt4[location_name=="Dominican Republic" & vaccine_name%in%unique(dt3[location_name=="Dominican Republic"]$vaccine_name)]

# merge together the multiple datasets
dt4 <- do.call("rbind", list(irq, nru, ukr, syr, gnq, dom))

# make vector of all the locations
lctns <- unique(dt4$location_name)
labelTable <- unique(dt4[,.(location_name, vaccine_name)])

tsPlots = lapply(seq(length(lctns)), function(g) {
  l = unique(labelTable[location_name==lctns[[g]]]$location_name)
  ggplot(dt4[location_name==lctns[[g]]], aes(y=val, x=year_id, color=vaccine_name)) +
     geom_line(size = 1, alpha = .8) +
     facet_wrap(~vaccine_name)+
     labs(title=paste('Time series of vaccine coverage for', l), y='Percent', x='Year',
          subtitle=paste('vaccines with most improvement in medium-SDI locations')) +
      theme_minimal()
})

# Save file
print(paste('Saving:', outputFile5i))
pdf(outputFile5i, height=5.5, width=9)
for(i in seq(length(tsPlots))) {
  print(tsPlots[[i]])
}
dev.off()

##### ##### ##### #####
# make trend graphs of countries that are missing values in 2014 as an extra check
##### ##### ##### #####
dt5 <- copy(data)
dt5 <- dt5[year_id==2014 & val==0]

missing <- unique(dt5[sdi_group=="medium",.(location_name, vaccine_name)])

msgPlots = lapply(seq(nrow(missing)), function(g) {
  ggplot(data[location_name==missing$location_name[[g]] & vaccine_name==missing$vaccine_name[[g]]], aes(y=val, x=year_id, color=vaccine_name)) + 
    geom_line(size = 1, alpha = .8)+
    labs(title=paste('Time series of', missing$vaccine_name[[g]],  'vaccine coverage for', missing$location_name[[g]]), y='Percent', x='Year', 
         subtitle=paste('medium-SDI locations that had 0% coverage in 2014')) + 
    theme_minimal()+
    ylim(0, 1)
})

# Save file
print(paste('Saving:', outputFile5j)) 
pdf(outputFile5j, height=5.5, width=9)
par(mfrow=c(2,2))
for(i in seq(length(msgPlots))) { 
  print(msgPlots[[i]])
}
dev.off()


