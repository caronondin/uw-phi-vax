# Name: Final prep for analysis
# Author: Francisco Rios
# Date: Dec 28, 2021

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

# Load prepped data
dt <- as.data.table(read_rds(paste0(prepped_data_dir, "aim_2/08_merged_dataset.RDS")))

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

# extrapolate where necessary using GLM (three different kinds of models depending on the variable type)
percentVars <- names(data)[c(6:7, 9:17)]
# monetaryVars <- names(data)[8]
# ordVars <- names(data)[]
# percentVars <- names(data)[17]
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

outputFile10 <- paste0(visDir, "aim_2/01_percent_data_extrapolated_with_logistic_regression.pdf")
pdf(outputFile10, height=5.5, width=9)

for(i in seq(length(pltlist))) {
  print(pltlist[[i]])
}
dev.off()

# extrapolate where necessary using GLM (three different kinds of models depending on the variable type)
# percentVars <- names(data)[c(7, 9:17)]
monetaryVars <- names(data)[8]
# ordVars <- names(data)[]
# percentVars <- names(data)[17]
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

outputFile10_b <- paste0(visDir, "aim_2/02_monetary_data_extrapolated_with_linear_regression.pdf")
pdf(outputFile10_b, height=5.5, width=9)

for(i in seq(length(pltlist2))) {
  print(pltlist2[[i]])
}
dev.off()

# Drop variables that do not have too much missing values (namely locations without any estimates for specific variables)
prepped_data <- na.omit(data)

# Create new categorical variable to organize the development assistance

# load list of ineligible locations for DAH
ineligible <- readRDS(file=paste0(codebook_directory, "locations_ineligible_for_dah.RDS"))

# subset data to exclude countries not eligible for development assistance
test <- prepped_data %>% filter(!location%in%ineligible)
summary(test$dah_per_the_mean)
prepped_data$dah_per_the_mean_cat <- prepped_data$dah_per_the_mean

# recode variables that are not eligible for funds and did not receive any as dah_per_the

prepped_data <- prepped_data %>%
  mutate(
    dah_per_the_mean_cat = case_when(dah_per_the_mean==0 & location %in% ineligible ~ "5", # not eligible for funds and did not receive any
                                   dah_per_the_mean >0  & dah_per_the_mean <= 0.005 ~ "3", # received first quartile (least)
                                   dah_per_the_mean >0.005 & dah_per_the_mean <0.121 ~ "2", # between first and third quartile (average)
                                   dah_per_the_mean >= 0.121 ~ "1", # received above third quartile (most)
                                   dah_per_the_mean==0 ~ "4")) # eligible for funds but did not receive any

# save as numeric variable
prepped_data$dah_per_the_mean_cat <- as.numeric(prepped_data$dah_per_the_mean_cat)

# Change direction of the variables
complVars = c('cpi', 'imm_pop_perc')

complTransform = function(x) {
  1-x
}

for (v in complVars) {
  prepped_data[, (v):=complTransform(get(v))]
}

# make sure all variables are within a reasonable range none of them exceed the realistic maximum
# prepped_data[perc_skil_attend>100, perc_skil_attend:=100]
# prepped_data[perc_urban>100, perc_urban:=100]
# prepped_data[mean_agree_vac_important>100, mean_agree_vac_important:=100]

# re arrange variables and save final
prepped_data <- prepped_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, sdi, dah_per_the_mean_cat, the_per_cap_mean, 
                                      ghes_per_the_mean, haqi, cpi, perc_skill_attend, imm_pop_perc, perc_urban,
                                      mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective)

# Save the prepped data set for analysis
saveRDS(prepped_data, file = paste0(prepped_data_dir, "aim_2/09_prepped_data_for_analysis.RDS"))
