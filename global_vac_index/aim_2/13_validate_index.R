# Purpose: Create a model the predicts vaccination coverage based on vaccine improvement index
# Author: Francisco Rios Casas
# Date: Dec 22 2021

# clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/01_set_up_R.R"))

library(sklearn)
library(DescTools)

# load index dataset
index_data <- readRDS(paste0(prepped_data_dir, "aim_2/10_index_results.RDS"))

# load vaccination coverage dataset
vax_data <- readRDS(paste0(prepped_data_dir, "aim_1/01_vaccine_trends.RDS"))

# reshape the vaccination data
vax_data <- pivot_wider(vax_data,
            id_cols=c("location_id", "location_name", "year_id"),
            names_from = "vaccine_name",
            values_from = c(prop_val, prop_upper, prop_lower))

# merge together files for each location and year
full_data <- index_data %>% left_join(vax_data, by=c("gbd_location_id"="location_id", "year"="year_id", "location"="location_name"))

# subset columns
full_data <- full_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, result, prop_val_MCV1, prop_val_DTP1, prop_val_DTP3) %>% filter(year!=2020)

# create a training dataset
row.number <- sample(1:nrow(full_data), 0.8*nrow(full_data))
train <- full_data[row.number,]
test <- full_data[-row.number,]

# fit a model for each vaccine
model1 <- glm(prop_val_MCV1~factor(location)+factor(year)+result, data=train, family = "binomial")
summary(model1)
par(mfrow=c(2,2))
plot(model1)

# evaluate the success of the model 
pred1 <- predict(model1, newdata = test, type = "response")
BrierScore(model1)
par(mfrow=c(1,1))
plot(test$prop_val_MCV1, pred1)

# fit second model
model2 <- glm(prop_val_DTP1~factor(location)+factor(year)+result, data=train, family = "binomial")
summary(model2)
par(mfrow=c(2,2))
plot(model2)

# evaluate the predictions of the model 
pred2 <- predict(model2, newdata = test, type = "response")
BrierScore(model2)
par(mfrow=c(1,1))
plot(test$prop_val_DTP1, pred2)

# fit third model
model3 <- glm(prop_val_DTP3~factor(location)+factor(year)+result, data=train, family = "binomial")
summary(model3)
par(mfrow=c(2,2))
plot(model3)

# evaluate the predictions of the model 
pred3 <- predict(model3, newdata = test, type = "response")
BrierScore(model3)
par(mfrow=c(1,1))
plot(test$prop_val_DTP3, pred3)

# # create duplicated columns that will be imputed
# full_data$prop_val_MCV1_estimated <- full_data$prop_val_MCV1
# full_data$prop_val_DTP1_estimated <- full_data$prop_val_DTP1
# full_data$prop_val_DTP3_estimated <- full_data$prop_val_DTP3

# # drop the values from 2019
# data <- full_data %>% filter(between(year, 1990, 2019))
# data[year==2019, prop_val_MCV1:=NA]
# data[year==2019, prop_val_DTP1:=NA]
# data[year==2019, prop_val_DTP3:=NA]

# # Use values to extrapolate per country
# v <- "prop_val_MCV1"
# h <- "Afghanistan"
# form = as.formula(paste0(v,'~result'))
# lmFit = glm(form, data[location==h], family='poisson')
# data[location==h,tmp:=exp(predict(lmFit, newdata=data[location==h]))]
# lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
# data[location==h & tmp>lim, tmp:=lim]
# ggplot(data[location==h], aes_string(y=v, x='result')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title=paste0(h))

# numVars <- names(data)[7:9]
# i=1
# pltlist <- list()
# for(v in numVars) {
#   for(h in unique(data$location)) {
#     
#     if (!any(is.na(data[location==h][[v]]))) next
#     if (!any(!is.na(data[location==h][[v]]))) next
#     form = as.formula(paste0(v,'~result'))
#     lmFit = glm(form, data[location==h], family='binomial')
#     data[location==h, tmp:=predict(lmFit, newdata=data[location==h], type = "response")]
#     # lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
#     # data[location==h & tmp>lim, tmp:=lim]
#     pltlist[[i]] <- ggplot(data[location==h], aes_string(y=v, x='result')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title = paste0(h))
#     data[location==h & is.na(get(v)), (v):=tmp]
#     i=i+1
#     pct_complete = floor(i/(length(numVars)*length(unique(data$location)))*100)
#     cat(paste0('\r', pct_complete, '% Complete'))
#     flush.console()
#   }
# }
# 
# data$tmp = NULL
# 
# outputFile13 <- paste0(visDir, "aim_2/11_index_vacc_coverage.pdf")
# pdf(outputFile13, height=5.5, width=9)
# 
# for(i in seq(length(pltlist))) {
#   print(pltlist[[i]])
# }
# dev.off()

# # plot relationship between index result and values of vaccine coverage
# ggplot(data, aes_string(y='prop_val_MCV1', x='result')) + geom_point()
# ggplot(data, aes_string(y='prop_val_DTP1', x='result')) + geom_point()
# ggplot(data, aes_string(y='prop_val_DTP3', x='result')) + geom_point()
# ggplot(data[location=="Afghanistan"], aes_string(y='prop_val_DTP3', x='result')) + geom_point()

# # Plot relationship between predicted and observed values of vaccine coverage
# ggplot(data[year=="2019"], aes_string(y='prop_val_MCV1', x='prop_val_MCV1_estimated')) + geom_point()
# ggplot(data[year=="2019"], aes_string(y='prop_val_DTP1', x='prop_val_DTP1_estimated')) + geom_point()
# ggplot(data[year=="2019"], aes_string(y='prop_val_DTP3', x='prop_val_DTP3_estimated')) + geom_point()

# Quantify how much agreement there is between predicted and estimated value
