# Author: Francisco Rios 
# Purpose: final data prep and visualize results
# Date: Last modified January 26, 2022

# PENDING IDEAS
# Add survey weights
# try calculating relative ratios
# try fitting one model per year and per vaccine?
# check for statistical significance

rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

data <- readRDS(paste0(prepped_data_dir, "03_estimates_vaccine_coverage_2007-2019.RDS"))

# final variable prep
data$VACCINE <- tolower(data$VACCINE)
data$VACCINE <- gsub(" ", "", data$VACCINE)

data <- data %>% 
  mutate(RACEETHK_R = case_when(RACEETHK_R==1 ~ "white",
                                RACEETHK_R==2 ~ "hispa",
                                RACEETHK_R==3 ~ "black",
                                RACEETHK_R==4 ~ "other"),
         INCPOV1 = case_when(INCPOV1==1 ~ "high",
                             INCPOV1==2 ~ "med", 
                             INCPOV1==3 ~ "low",
                             INCPOV1==4 ~ "miss"))

# re-shape the data to facilitate calculation
dt2 <- pivot_wider(data, id_cols = c(ESTIAP, RACEETHK_R, INCPOV1, VACCINE), names_from = c(YEAR), values_from = PredictedProb)

# calculate the change in the time periods of interest for each vaccine
dt2$change <- ((dt2$`2019`-dt2$`2007`)/dt2$`2007`)*100

# reshape data for additional calculations
dt3 <- pivot_wider(dt2, id_cols = c(ESTIAP, INCPOV1, VACCINE), names_from = RACEETHK_R, values_from = c(change))

# Calculate equitable immunization improvement
# dt3$eii_hispa <- ((dt3$hispa - dt3$white)/dt3$white)*100
# dt3$eii_black <- ((dt3$black - dt3$white)/dt3$white)*100
# dt3$eii_other <- ((dt3$other - dt3$white)/dt3$white)*100

dt3$eii_hispa <- (dt3$hispa - dt3$white)
dt3$eii_black <- (dt3$black - dt3$white)
dt3$eii_other <- (dt3$other - dt3$white)

# ggplot(dt3, aes(x=eii_hispa)

# Reshape data for additional calculations
# dt4 <- dt3 %>% select(ESTIAP, INCPOV1, VACCINE, eii_hispa, eii_black, eii_other)
# dt4 <- dt3 %>% select(ESTIAP, INCPOV1, VACCINE, eii_hispa, eii_black, eii_black)
dt4 <- pivot_wider(dt3, id_cols = c(ESTIAP), names_from = c(INCPOV1, VACCINE), values_from = c(eii_hispa, eii_black, eii_other))

# create function that deletes outliers
is_outlier <- function(x){
  lower_bound <- quantile(x, 0.05)
  upper_bound <- quantile(x, 0.95)
  ifelse((x < lower_bound | x > upper_bound), NA, x)
}

numVars <- names(dt4)[2:37]
for (v in numVars) {
  dt4 <- as.data.table(dt4)
  dt4[, (v):=is_outlier(get(v))]
}

# calculate the quantile groups for each column
# summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply( dt4[,2:37] , 2 , quantile , probs = quants , na.rm = TRUE))
quantiles$quant <- NA
quantiles$quant[1] <- "first_q"
quantiles$quant[2] <- "third_q"

quantiles <- pivot_longer(quantiles, cols = starts_with("eii"), names_to = c("group"))
quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value" )
quantiles <- quantiles %>% separate(group, into = c("variable", "race", "income", "vaccine"))
quantiles <- quantiles %>% select(-c(variable))

# reshape each dataset
dt5 <- dt4 %>%
  pivot_longer(
    !c(ESTIAP),
    names_to = c("group"),
    values_to = "eii",
    values_drop_na = FALSE) %>% 
  separate(group, into = c("variable", "race", "income", "vaccine")) %>%
  select(-c(variable))

# merge the dt_groups data to the quantiles cut off points
dt6 <- merge(dt5, quantiles, by=c("race", "income", "vaccine"))

# calculate the group for each location, race, income and vaccine
dt6 <- dt6 %>% 
  mutate(category = case_when(eii > first_q & eii < third_q ~ "medium",
                     eii < first_q ~ "low",
                     eii > third_q ~ "high"))

# count how many states were most likely to exceed the change within racial group and ethnicity and vaccine
dt6 <- dt6 %>% filter(!is.na(eii))
plotdt6 <- dt6 %>% group_by(ESTIAP, category) %>%
  tally()

levels(plotdt6$ESTIAP)


# create figures to compare states
ggplot(plotdt6, aes(fill=category, y =n, x=ESTIAP)) + geom_bar(position="stack", stat="identity")
ggplot(data=dt6, aes(x=category)) + geom_bar(stat="count") + facet_wrap(~ESTIAP)

# reshape the dt4 to facilitate the merge
# dt5 <- dt4 %>% 
# # create new columns that will help group each variable
# dt4$group_hispa_high_dtp <- NA_character_
# dt4$group_hispa_med_dtp <- NA_character_
# dt4$group_hispa_low_dtp <- NA_character_
# dt4$group_hispa_miss_dtp <- NA_character_
# dt4$group_hispa_high_mmr <- NA_character_
# dt4$group_hispa_med_mmr <- NA_character_
# dt4$group_hispa_low_mmr <- NA_character_
# dt4$group_hispa_miss_mmr <- NA_character_
# dt4$group_hispa_high_hepb <- NA_character_
# dt4$group_hispa_med_hepb <- NA_character_
# dt4$group_hispa_low_hepb <- NA_character_
# dt4$group_hispa_miss_hepb <- NA_character_
# 
# dt4$group_black_high_dtp <- NA_character_
# dt4$group_black_med_dtp <- NA_character_
# dt4$group_black_low_dtp <- NA_character_
# dt4$group_black_miss_dtp <- NA_character_
# dt4$group_black_high_mmr <- NA_character_
# dt4$group_black_med_mmr <- NA_character_
# dt4$group_black_low_mmr <- NA_character_
# dt4$group_black_miss_mmr <- NA_character_
# dt4$group_black_high_hepb <- NA_character_
# dt4$group_black_med_hepb <- NA_character_
# dt4$group_black_low_hepb <- NA_character_
# dt4$group_black_miss_hepb <- NA_character_
# 
# dt4$group_other_high_dtp <- NA_character_
# dt4$group_other_med_dtp <- NA_character_
# dt4$group_other_low_dtp <- NA_character_
# dt4$group_other_miss_dtp <- NA_character_
# dt4$group_other_high_mmr <- NA_character_
# dt4$group_other_med_mmr <- NA_character_
# dt4$group_other_low_mmr <- NA_character_
# dt4$group_other_miss_mmr <- NA_character_
# dt4$group_other_high_hepb <- NA_character_
# dt4$group_other_med_hepb <- NA_character_
# dt4$group_other_low_hepb <- NA_character_
# dt4$group_other_miss_hepb <- NA_character_

# quantiles$new_variable <- quantiles$group
# quantiles$new_variable <- gsub("eii", "group", quantiles$new_variable)
# quantiles <- as.data.table(quantiles)

# dt4 %>% mutate(
#   
# )

# create a loop that will group each variable
# groupVars <- names(dt4)[38:73]
# 
# i <- 1
# for (i in 1:nrow(dt4)){
#   v <- numVars[i]
#   g <- groupVars[i]
#   dt4 %>% select(g, v) %>% filter(v==quantiles[group==v]) <- 
#   dt4[i, get(g)] <- ifelse(dt4[i ,get(v)] < quantiles[group==v]$first_q, "low", ifelse(dt4[i, get(v)] > quantiles[group==v]$third_q, "high", ifelse(is.na(dt4[i ,get(v)]), NA_character_, "medium")))
#   rm(g)
# }
# # g <- groupVars[1]
# for (r in 1:nrow(dt4)){
#   for (c in 38:73){
#     dt4[r,c] <- ifelse(dt4[,get(v)][r] < quantiles[group==v]$first_q, "low", ifelse(dt4[,get(v)][r] > quantiles[group==v]$third_q, "high", ifelse(is.na(dt4[,get(v)][r]), NA_character_, "medium")))
#   }
# }
# for (v in newVars){
#   data[is.na(get(v)), (v):=0]
# }

# 
# # separate the data into two separate files
# dt_groups <- dt4 %>% select(c(ESTIAP), starts_with("group"))
# dt_eii <- dt4 %>% select(c(ESTIAP), starts_with("eii"))

# # reshape each dataset
# dt_groups <- dt_groups %>%
#   pivot_longer(
#     !c(ESTIAP),
#     names_to = c("group"),
#     values_to = "category",
#     values_drop_na = FALSE) %>% 
#   separate(group, into = c("variable", "race", "income", "vaccine"))

# dt_eii <- dt_eii %>%
#   pivot_longer(
#     !c(ESTIAP),
#     names_to = c("group"),
#     values_to = "eii",
#     values_drop_na = FALSE) %>% 
#   separate(group, into = c("variable", "race", "income", "vaccine"))

# drop unnecessary columns
# dt_groups <- dt_groups %>% select(-c(variable))
# dt_eii <- dt_eii %>% select(-c(variable))

# merge two datasets into one
# dt5 <- dt_eii %>% inner_join(dt_groups, by=c("ESTIAP", "race", "income", "vaccine"))


# create visuals and tables to explore the data

# format the location variable
ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
                "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
                "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
dt5$ESTIAP <- factor(dt5$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)
dt5[category=="high", .N, by = .(ESTIAP, vaccine)]
dt5[category=="medium", .N, by = .(ESTIAP, vaccine)]
dt5[category=="low", .N, by = .(ESTIAP, vaccine)]

# subset data to high-performing locations
# plot.data <- dt5 %>%
#   group_by(ESTIAP, VACCINE) %>%
#   dplyr::summarize(.N))


ggplot(data = dt5, aes(x = outlier, y = N)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label=N), hjust=1.6, color="white") +
  theme_minimal(base_size = 12) + 
  labs(title = paste('Countries that exceeded median improvement in vaccine coverage'), 
       y = 'Number of vaccines that saw greater-than-expected improvement', 
       x = 'Location', 
       subtitle = paste0('between 2014 and 2019, among low-SDI countries'))

dt5 <- as.data.table(dt5)

high.per.data <- dt5 %>% filter(category=="high")

ggplot(dt5, aes(x=category, y=vaccine, size=eii, color=ESTIAP)) +
  geom_point(alpha=0.5) 
  
  geom_tile(aes(fill = eii)) +
  geom_text(aes(label = round(eii, 2))) +
  scale_fill_gradient(low="white", high= "steelblue") +
  facet_grid(vars(income), vars(race))
  

# create raster plot of locations
j <- ggplot(dt3, aes(vaccine_name, location_name)) +
  geom_tile(aes(fill = percent_change)) + 
  geom_text(aes(label = round(percent_change, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_tile(data=median, size=1, fill=NA, colour="black") +
  labs(title=paste('Percent change in countries that saw above-average improvement'), y = 'Location', x = 'Vaccine', 
       subtitle=paste0('between 2014 and 2019, among low-SDI countries'))
# create tables described in DAP

# dobyearData <- dobyearData %>%
#   pivot_longer(
#     !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
#     names_to = c("variable", "child"),
#     names_sep = "_",
#     values_to = "birth_year",
#     values_drop_na = FALSE
#   )

# reshape the data


# 
# dt4[, get(g)] <- ifelse(dt4[,get(v)][1]<quantiles[group==v]$first_q, "low", ifelse(dt4[,get(v)][1]>quantiles[group==v]$third_q, "high", "medium"))
# for (v in numVars){
#   ifelse()
#   #   min = norm_cut$min[i]
#   #   max = norm_cut$max[i]
#   #   v = norm_cut$variable[i]
#   #   final_data[, (v):=(get(v)-min)/(max-min)]
#   dt4[, (v):=get(v):=(get(v)-min)/(max)]
# }
  # create loop to calculate the groups for each s
# i <- 1
# for (i in 1:length(normVars)) {
#   min = norm_cut$min[i]
#   max = norm_cut$max[i]
#   v = norm_cut$variable[i]
#   final_data[, (v):=(get(v)-min)/(max-min)]
# }
# calculate the three quintile groups for each vaccine and for each income group
# quantile_table_hispanic <- dt4 %>% 
#   group_by(INCPOV1, VACCINE) %>%
#   dplyr::summarize(quant25 = quantile(eii_2, na.rm=TRUE, probs = .25),
#                    quant75 = quantile(eii_2, na.rm=TRUE, probs = .75))
# quantile_table_black <- dt4 %>% 
#   group_by(INCPOV1, VACCINE) %>%
#   dplyr::summarize(quant25 = quantile(eii_3, na.rm=TRUE, probs = .25),
#                    quant75 = quantile(eii_3, na.rm=TRUE, probs = .75))
# quantile_table_other <- dt4 %>% 
#   group_by(INCPOV1, VACCINE) %>%
#   dplyr::summarize(quant25 = quantile(eii_4, na.rm=TRUE, probs = .25),
#                    quant75 = quantile(eii_4, na.rm=TRUE, probs = .75))

# select columns of interest
# dt4 <- dt4 %>% select(ESTIAP, INCPOV1, VACCINE, eii_2, eii_3, eii_4)

# pivot dataset longer
# dt5 <- pivot_longer(dt4, cols = c(eii_2, eii_3, eii_4), names_to = "RACEETHK_R")





# calculate risk ratios
# data_final$dtp_rr_2 <- (data_final$)

# eth_vac_cov_final$eii_dtp_1 <- (eth_vac_cov_final$change_dtp_1 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
# eth_vac_cov_final$eii_dtp_3 <- (eth_vac_cov_final$change_dtp_3 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
# eth_vac_cov_final$eii_dtp_4 <- (eth_vac_cov_final$change_dtp_4 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
# 
# eth_vac_cov_final$eii_mmr_1 <- (eth_vac_cov_final$change_mmr_1 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
# eth_vac_cov_final$eii_mmr_3 <- (eth_vac_cov_final$change_mmr_3 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
# eth_vac_cov_final$eii_mmr_4 <- (eth_vac_cov_final$change_mmr_4 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
# 
# eth_vac_cov_final$eii_hep_1 <- (eth_vac_cov_final$change_hep_1 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
# eth_vac_cov_final$eii_hep_3 <- (eth_vac_cov_final$change_hep_3 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
# eth_vac_cov_final$eii_hep_4 <- (eth_vac_cov_final$change_hep_4 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
# # for each vaccine and racial/ethnic group calculate the equitable improvement index: which is the relative difference between the two values
# data_wide$dtp_2007

# remove outliers
# from each of the values of interest remove the outliers using the following formula

# create a function to spot outliers in data
# is_outlier <- function(x) {
#   return(x > quantile(x, 0.75) + 1.5 * IQR(x))
# }



# # calculate the three quintile groups for each vaccine and for each income group
# data_final %>% 
#   group_by(INCPOV1) %>%
#   dplyr::summarize(quant25 = quantile(eii_dtp_2, probs = .25),
#             quant75 = quantile(eii_dtp_2, probs = .75))
# 
# data_final <- as.data.table(data_final)
# data_final[, lapply(.SD, quantile, prob = c(0.25, 0.75), na.rm=TRUE), .SDcols = c('eii_dtp_2')]
# # change income to a non-factored variable
# out <- dt.data[ ,lapply(.SD, quantile, prob = c(.05, .25, .5, .75, .95), na.rm = TRUE),  .SDcols = grep("V", names(dt.data), value = TRUE)]
# data_final %>% 
#   mutate(eii_dtp_2_test = 
#            case_when(is_outlier(eii_dtp_2)==TRUE ~ NA,
#                      is_outlier(eii_dtp_2)==FALSE ~ eii_dtp_2))
# 
# data_final[,outlier:=ifelse(is_outlier(eii_dtp_2), eii_dtp, as.double(NA))]
# dt[,outlier:=ifelse(is_outlier(percent_change), location_name, as.double(NA)), by = .(vaccine_name),
#    TRUE]
# 

# create tables identified in data analysis plan

# 1: Quartile identification according to equitable
# 2
# 3
# 4

# save figures and tables


vaccines <- unique(data$VACCINE)

for (i in 1:length(vaccines)) {
  g <- ggplot(data %>% filter(VACCINE==vaccines[i]), aes(YEAR, PredictedProb, group=factor(ESTIAP))) + 
    geom_line(aes(color=ESTIAP), show.legend = FALSE) +
    facet_grid(vars(INCPOV1), vars(RACEETHK_R)) +
    labs(title = paste0(unique(vaccines[i]), " vaccination trends between 2007 and 2019"),
         subtitle = "Stratified according to race/ethnicity and family income") +
    ylab('Predicted probability of being vaccinated') +
    xlab('Year') +
    theme_minimal()
  print(g)
}

# make vector of all the locations
lctns <- unique(data$ESTIAP)
labelTable <- as.data.table(unique(data %>% select(ESTIAP, VACCINE)))

tsPlots = lapply(seq(length(lctns)), function(g) {
  l = unique(labelTable[ESTIAP == lctns[[g]]]$ESTIAP)
  ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = VACCINE)) + 
    geom_ribbon(aes(ymin = LL,
                    ymax = UL, fill = VACCINE), alpha = 0.2) +
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~INCOME) +
    labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
         subtitle = "Stratified according to race/ethnicity and family income") +
    theme_minimal() +
    facet_grid(vars(INCPOV1), vars(RACEETHK_R))
})

# make vector of all the locations
lctns <- unique(data$ESTIAP)
labelTable <- as.data.table(unique(data %>% select(ESTIAP, VACCINE)))

tsPlots = lapply(seq(length(lctns)), function(g) {
  l = unique(labelTable[ESTIAP == lctns[[g]]]$ESTIAP)
  ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = VACCINE)) + 
    geom_ribbon(aes(ymin = LL,
                    ymax = UL, fill = VACCINE), alpha = 0.2) +
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~INCOME) +
    labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
         subtitle = "Stratified according to race/ethnicity and family income") +
    theme_minimal() +
    facet_grid(vars(INCPOV1), vars(RACEETHK_R))
})

# Save file
print(paste('Saving:', outputFile05_02)) 
pdf(outputFile07, height = 8.5, width = 11)
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}
dev.off()


# create tables identified in data analysis plan

# 1: Quartile identification according to equitable
# 2
# 3
# 4

# save figures and tables

# Calculate equitable immunization improvement
# data_final$eii_dtp_2 <- ((data_final$dtp_change_2 - data_final$dtp_change_1)/data_final$dtp_change_1)*100
# data_final$eii_dtp_3 <- ((data_final$dtp_change_3 - data_final$dtp_change_1)/data_final$dtp_change_1)*100
# data_final$eii_dtp_4 <- ((data_final$dtp_change_4 - data_final$dtp_change_1)/data_final$dtp_change_1)*100
# 
# data_final$eii_mmr_2 <- ((data_final$mmr_change_2 - data_final$mmr_change_1)/data_final$mmr_change_1)*100
# data_final$eii_mmr_3 <- ((data_final$mmr_change_3 - data_final$mmr_change_1)/data_final$mmr_change_1)*100
# data_final$eii_mmr_4 <- ((data_final$mmr_change_4 - data_final$mmr_change_1)/data_final$mmr_change_1)*100
# 
# data_final$eii_hep_2 <- ((data_final$hep_change_2 - data_final$hep_change_1)/data_final$hep_change_1)*100
# data_final$eii_hep_3 <- ((data_final$hep_change_3 - data_final$hep_change_1)/data_final$hep_change_1)*100
# data_final$eii_hep_4 <- ((data_final$hep_change_4 - data_final$hep_change_1)/data_final$hep_change_1)*100
# data_wide$dtp_change <- ((data_wide$dtp_2019-data_wide$dtp_2007)/data_wide$dtp_2007)*100
# data_wide$mmr_change <- ((data_wide$mmr_2019-data_wide$mmr_2007)/data_wide$mmr_2007)*100
# data_wide$hep_change <- ((data_wide$hepb_2019-data_wide$hepb_2007)/data_wide$hepb_2007)*100

# data_final$eii_2
# data_final$eii_dtp_2 <- (data_final$dtp_change_2 / data_final$dtp_change_1) #/data_final$dtp_change_1*100
# data_final$eii_dtp_3 <- (data_final$dtp_change_3 / data_final$dtp_change_1) #/data_final$dtp_change_1*100
# data_final$eii_dtp_4 <- (data_final$dtp_change_4 / data_final$dtp_change_1) #/data_final$dtp_change_1*100
# 
# data_final$eii_mmr_2 <- (data_final$mmr_change_2 / data_final$mmr_change_1) #/data_final$mmr_change_1*100
# data_final$eii_mmr_3 <- (data_final$mmr_change_3 / data_final$mmr_change_1) #/data_final$mmr_change_1*100
# data_final$eii_mmr_4 <- (data_final$mmr_change_4 / data_final$mmr_change_1) #/data_final$mmr_change_1*100
# 
# data_final$eii_hep_2 <- (data_final$hep_change_2 / data_final$hep_change_1) #/data_final$hep_change_1*100
# data_final$eii_hep_3 <- (data_final$hep_change_3 / data_final$hep_change_1) #/data_final$hep_change_1*100
# data_final$eii_hep_4 <- (data_final$hep_change_4 / data_final$hep_change_1) #/data_final$hep_change_1*100

# data_final$eii_2 <- (data_final$`2` / data_final$`1`)
# data_final$eii_3 <- (data_final$`3` / data_final$`1`)
# data_final$eii_4 <- (data_final$`4` / data_final$`1`)
