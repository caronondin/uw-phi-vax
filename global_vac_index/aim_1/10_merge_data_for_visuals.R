# Author: Francisco Rios 
# Purpose: Merges all data to be used in analyses (such as vaccination trends, SDI, disease trends)
# Date: Last modified July 13, 2021

# Read in the previously saved files for vaccination trends
vax_dt <- readRDS(outputFile02) # vaccination trends
sdi_dt <- readRDS(outputFile03) # SDI 
dx_dt <- readRDS(outputFile08) # disease trends
  
# reformat vaccine name column
vax_dt$vaccine_name <- tolower(vax_dt$vaccine_name)

# Reshape the vaccine data to be wider
vax_dt_rsh <- vax_dt %>% pivot_wider(id_cols = c(location_id, location_name, year_id),
                          names_from = vaccine_name,
                          values_from = c(prop_val, prop_upper, prop_lower),
                          values_fill = NA)

# Reshape the disease data to be wider
dx_dt <- dx_dt %>% select(location_id, location_name, year_id, cause_name,
                          deaths_number_val, ylds_number_val, 
                          deaths_percent_val, ylds_percent_val,
                          deaths_rate_val, ylds_rate_val)

# Rename the variable of cause_name
dx_dt$cause_name <- tolower(dx_dt$cause_name)
dx_dt <- dx_dt %>% mutate(cause_name=recode(cause_name,
                                   'diarrheal diseases'='diarrheal_diseases',
                                   'total burden related to hepatitis b'='hepb',
                                   'drug-susceptible tuberculosis'='drug-susceptible_tb',
                                   'multidrug-resistant tuberculosis without extensive drug resistance'='mdr_tb',
                                   'tuberculosis'='tb',
                                   'extensively drug-resistant tuberculosis'='xdrtb',
                                   'latent tuberculosis infection'='latent_tb',
                                   'whooping cough'='whooping_cough'))

dx_dt_rsh <- dx_dt %>% pivot_wider(id_cols=c(location_id, location_name, year_id),
                               names_from = cause_name,
                               values_from = c(deaths_number_val, ylds_number_val, 
                                               deaths_percent_val, ylds_percent_val,
                                               deaths_rate_val, ylds_rate_val))

# Set the merge variables
mergeVars <- c('location_id', 'year_id')

# Only keep certain variables in each data set
sdi_dt <- sdi_dt %>% select(c(location_id, year_id, sdi))
dx_dt_rsh <- rename(dx_dt_rsh, dx_location_name = location_name)
vax_dt_rsh <- vax_dt_rsh %>% select(-c(location_name))

# Merge data together
full_data <- sdi_dt %>% 
  full_join(vax_dt_rsh, by=mergeVars) %>%
  full_join(dx_dt_rsh, by=mergeVars)

# add level variable
location_map <- read_xlsx(paste0(codebook_directory, "aim_1/IHME_GBD_2019_GBD_LOCATION_HIERARCHY_Y2020M10D15.XLSX"))
location_map <- rename(location_map, 
       location_id=`Location ID`, 
       level=Level,
       location_name=`Location Name`)

# keep only data that applies to level 3 (country)
full_data <- full_data %>% full_join(location_map, by="location_id")

# drop locations without a level
full_data <- full_data %>% 
  filter(!is.na(level)) %>%
  filter(level==3) %>% 
  select(-c(`Sort Order`, `Parent ID`, `Location Set Version ID`, dx_location_name)) # drop columns no longer necessary

# create dataframe of sdi groups
sdi_groups_map <- full_data %>% filter(year_id==2019) %>% select(location_id, sdi)
sdi_groups_map <- sdi_groups_map %>% 
  mutate(sdi_group_in_2019 = case_when(sdi > 0.5790 & sdi <= 0.7423 ~ "medium",
                                       sdi <= 0.5790 ~ "low",
                                       sdi > 0.7423 ~ "high")) %>%
  select(location_id, sdi_group_in_2019)
                                      
# merge SDI group present onto the dataset
full_data <- full_data %>% full_join(sdi_groups_map, by="location_id")

# Re arrange columns
full_data <- relocate(full_data, c(location_name, level, sdi_group_in_2019), .after=location_id)

# Save full data
saveRDS(full_data, file=outputFile09)

# visualize trends in the vaccination data
plot_data <- full_data %>% select(c(1:17))
plot_data <- plot_data %>% pivot_longer(
  cols = starts_with("prop_val"),
  names_to = c("vaccine_name"),
  values_to = "proportion"
)
plot_data <- as.data.table(plot_data)
lctns <- unique(plot_data$location_name)
labelTable <- unique(plot_data[,.(location_name, vaccine_name)])

tsPlots = lapply(seq(length(lctns)), function(g) {
  l = unique(labelTable[location_name == lctns[[g]]]$location_name)
  ggplot(plot_data[location_name == lctns[[g]]], aes(y = proportion, x = year_id)) + 
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~vaccine_name) + 
    labs(title = paste('Time series of vaccine coverage for', l), y = 'Percent', x = 'Year', 
         subtitle = paste()) + 
    theme_minimal()
})

outputFile10 <- paste0(visDir, "aim_1/data_checks/01_timeseries_plots_all_locations.PDF")

# Save file
print(paste('Saving:', outputFile10)) 
pdf(outputFile10, height = 5.5, width = 9)
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}
dev.off()

# Print final statement
print("Step 06: Merging complete; data set saved in prepped data folder")