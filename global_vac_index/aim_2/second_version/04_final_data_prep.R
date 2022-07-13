# Name: Final prep for analysis
# Author: Francisco Rios
# Date: July 12, 2022

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/second_version/01_set_up_R.R"))

# Load prepped data
dt <- as.data.table(read_rds(paste0(prepped_data_dir, "aim_2/13_merged_dataset_second_version.RDS")))

# subset data to specific time frame
data <- dt %>% filter(between(year, 1990, 2020))

# divide certain variables by 100 to ensure they range between 0 and 1 only
rescaleVars = c('haqi', 'perc_skill_attend', 'cpi')

rescaleTransform = function(x) {
  x/100
}

for (v in rescaleVars) {
  data[, (v):=rescaleTransform(get(v))]
}

# create complement variables
complVars = c('cpi', 'imm_pop_perc')

complTransform = function(x) {
  1-x
}

for (v in complVars) {
  data[, (v):=complTransform(get(v))]
}

# add constant of 1 to DAH to avoid values of 0
constantVars = c("dah_per_cap_ppp_mean")

addConstant = function(x) {
  x+1
}

for (v in constantVars) {
  data[, (v):=addConstant(get(v))]
}

# extrapolate where necessary using GLM for variables that range between 0 and 1
percentVars <-  c("sdi", "haqi", "cpi", "ghes_per_the_mean",
                  "perc_skill_attend", "imm_pop_perc", "perc_urban", 
                  "mean_agree_vac_safe", "mean_agree_vac_important", "mean_agree_vac_effective")

i<-1
pltlist <- list()
for(v in percentVars) {
  for(h in unique(data$location)) {
    # i=i+1
    if (!any(is.na(data[location==h][[v]]))) next
    if (!any(!is.na(data[location==h][[v]]))) next
    form = as.formula(paste0(v,'~year'))
    lmFit = glm(form, data[location==h], family='binomial')
    data[location==h, tmp:=(predict(lmFit, newdata=data[location==h], type="response"))]
    # lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
    # data[location==h & tmp>lim, tmp:=lim]
    pltlist[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title = paste0(h))
    data[location==h & is.na(get(v)), (v):=tmp]
    i=i+1
    # pct_complete = floor(i/(length(percentVars)*length(unique(data$location)))*100)
    # cat(paste0('\r', pct_complete, '% Complete'))
    flush.console()
  }
}

data$tmp = NULL

outputFile10 <- paste0(visDir, "aim_2/second_version/01_percent_data_extrapolated_with_logistic_regression.pdf")
pdf(outputFile10, height=5.5, width=9)

for(i in seq(length(pltlist))) {
  print(pltlist[[i]])
}
dev.off()

# extrapolate where necessary using GLM for numeric variables

monetaryVars <- c("the_per_cap_mean", "dah_per_cap_ppp_mean")

i=1
pltlist2 <- list()
for(v in monetaryVars) {
  for(h in unique(data$location)) {
    
    if (!any(is.na(data[location==h][[v]]))) next
    if (!any(!is.na(data[location==h][[v]]))) next
    form = as.formula(paste0(v,'~year'))
    lmFit = lm(form, data[location==h])
    data[location==h, tmp:=(predict.lm(lmFit, newdata=data[location==h]))]
    lim = min(data[location==h][[v]], na.rm=T)
    data[location==h & tmp<0, tmp:=lim]
    pltlist2[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point()+ labs(title = paste0(h)) + geom_point(aes(y=tmp),color='red') 
    data[location==h & is.na(get(v)), (v):=tmp]
    
    i=i+1
    # pct_complete = floor(i/(length(percentVars)*length(unique(data$location)))*100)
    # cat(paste0('\r', pct_complete, '% Complete'))
    flush.console()
  }
}

data$tmp = NULL

outputFile10_b <- paste0(visDir, "aim_2/second_version/02_monetary_data_extrapolated_with_linear_regression.pdf")
pdf(outputFile10_b, height=5.5, width=9)

for(i in seq(length(pltlist2))) {
  print(pltlist2[[i]])
}
dev.off()

# Drop variables that have too much missing values (namely locations without any estimates for specific variables)
prepped_data <- na.omit(data)

# re arrange variables and save final
prepped_data <- prepped_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, region, 
                                        dah_eligible,  sdi, the_per_cap_mean, ghes_per_the_mean, 
                                        dah_per_cap_ppp_mean, haqi, cpi, perc_skill_attend, imm_pop_perc, perc_urban,
                                        mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective)

# Save the prepped data set for analysis
saveRDS(prepped_data, file = paste0(prepped_data_dir, "aim_2/14_prepped_data_for_analysis_second_version.RDS"))
