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
dt3$eii_hispa <- ((dt3$hispa - dt3$white)/dt3$white)*100
dt3$eii_black <- ((dt3$black - dt3$white)/dt3$white)*100
dt3$eii_other <- ((dt3$other - dt3$white)/dt3$white)*100

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
summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply( dt4[,2:37] , 2 , quantile , probs = quants , na.rm = TRUE))
quantiles$quant <- NA
quantiles$quant[1] <- "first_q"
quantiles$quant[2] <- "third_q"

quantiles <- pivot_longer(quantiles, cols = starts_with("eii"), names_to = c("group"))
quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value" )

# create new columns that will help group each variable
dt4$group_hispa_high_dtp <- NA_character_
dt4$group_hispa_med_dtp <- NA_character_
dt4$group_hispa_low_dtp <- NA_character_
dt4$group_hispa_miss_dtp <- NA_character_
dt4$group_hispa_high_mmr <- NA_character_
dt4$group_hispa_med_mmr <- NA_character_
dt4$group_hispa_low_mmr <- NA_character_
dt4$group_hispa_miss_mmr <- NA_character_
dt4$group_hispa_high_hepb <- NA_character_
dt4$group_hispa_med_hepb <- NA_character_
dt4$group_hispa_low_hepb <- NA_character_
dt4$group_hispa_miss_hepb <- NA_character_

dt4$group_black_high_dtp <- NA_character_
dt4$group_black_med_dtp <- NA_character_
dt4$group_black_low_dtp <- NA_character_
dt4$group_black_miss_dtp <- NA_character_
dt4$group_black_high_mmr <- NA_character_
dt4$group_black_med_mmr <- NA_character_
dt4$group_black_low_mmr <- NA_character_
dt4$group_black_miss_mmr <- NA_character_
dt4$group_black_high_hepb <- NA_character_
dt4$group_black_med_hepb <- NA_character_
dt4$group_black_low_hepb <- NA_character_
dt4$group_black_miss_hepb <- NA_character_

dt4$group_other_high_dtp <- NA_character_
dt4$group_other_med_dtp <- NA_character_
dt4$group_other_low_dtp <- NA_character_
dt4$group_other_miss_dtp <- NA_character_
dt4$group_other_high_mmr <- NA_character_
dt4$group_other_med_mmr <- NA_character_
dt4$group_other_low_mmr <- NA_character_
dt4$group_other_miss_mmr <- NA_character_
dt4$group_other_high_hepb <- NA_character_
dt4$group_other_med_hepb <- NA_character_
dt4$group_other_low_hepb <- NA_character_
dt4$group_other_miss_hepb <- NA_character_

quantiles$new_variable <- quantiles$group
quantiles$new_variable <- gsub("eii", "group", quantiles$new_variable)
quantiles <- as.data.table(quantiles)

# create a loop that will group each variable
groupVars <- names(dt4)[38:73]
g <- groupVars[1]
for (r in 1:nrow(dt4)){
  for (c in 38:73){
    dt4[r,c] <- ifelse(dt4[,get(v)][r]<quantiles[group==v]$first_q, "low", ifelse(dt4[,get(v)][r]>quantiles[group==v]$third_q, "high", "medium"))
  }
}

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
