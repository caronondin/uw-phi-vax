# Author: Francisco Rios 
# Purpose: Create sample tables and visualizations for public-facing website
# Date: Last modified July 20, 2021

dt <- readRDS(outputFile06)

set.seed(26)

# randomly sample six locations in the low SDI group for plotting sample visuals
n=6
lctns <- sample(dt[sdi_group=="low"]$location_name, n)
sample = dt[location_name %in% lctns]

##############################################
# sample plot of vaccine trends in six regions
##############################################

# organize series and label for graphing
varGroups = unique(dt$vaccine_name)
labelTable <- unique(dt[,.(vaccine_name)])

# create example time series graphs of six locations from low SDI group
tsPlots = lapply(seq(length(varGroups)), function(g) {
  l = labelTable[vaccine_name==varGroups[[g]]]
  ggplot(sample[vaccine_name%in%varGroups[[g]]], aes(y=val, x=year_id, color=vaccine_name)) + 
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~location_name) + 
    labs(title=paste('Time series of vaccine coverage for', l), y='Percent', x='Year', 
         subtitle=paste('Random Sample of', n, 'locations'), caption='') + 
    theme_minimal()
}) 

# Save file
print(paste('Saving:', outputFile4)) 
pdf(outputFile4, height=5.5, width=9)
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}
dev.off()

# create graphs showing trends in each location for all vaccines
# locations = unique(dt$location_id)
# locations = sample(dt[sdi_group=="low"]$location_id, n)
# sample2 = dt[location_id %in% locations]
# locTable <- unique(dt[,.(location_id, location_name)])

tsPlots2 = lapply(seq(length(locations)), function(g) {
  l2 = locTable[location_id%in%locations[[g]]]$location_name
  ggplot(sample[location_id%in%]
    dt[location_id%in%locations[[g]]], aes(y=val, x=year_id, color=vaccine_name)) + 
    geom_line(size = 1, alpha = .8) +
    labs(title=paste('Time series of vaccine coverage in', l2), y='Percent', x='Year')
    theme_minimal()
})

# Save file
print(paste('Saving:', outputFile4b)) 
pdf(outputFile4b, height=5.5, width=9)
for(i in seq(length(loctsPlots))) { 
  print(tsPlots2[[i]])
}

dev.off()

# create sample ratio tables
# ggplot(plot_dt, aes(loc_name, plot_module, fill= module_percent_of_total_rssh)) + geom_tile() + theme_bw() + 
#   scale_x_discrete(position = 'top') + labs( x = '', y = "", fill = "% of Country's Total \nRSSH Spending") + 
#   scale_fill_continuous(high = "#132B43", low = "#9fd4fc") +
#   scale_y_discrete(limits = rev(levels(plot_dt$plot_module))) +
#   theme(axis.text=element_text(size=12), legend.text = element_text(size=11), legend.title = element_text(size = 13)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = -0.01)) +
#   geom_text(aes(label= paste0(plot_dt$module_percent_of_total_rssh, '%', '\n', plot_dt$indicators)), size = 3.5, color = '#FFFFFF')
# # scale_fill_continuous(type = 'viridis') 

# 56B1F7 - alternate/close to default color 
# create sample tables of relevant values for data
