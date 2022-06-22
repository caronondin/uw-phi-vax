# archived_index_validation


########################################
##### METHOD 2 OF VALIDATING INDEX #####
########################################

# create a training dataset
# row.number <- sample(1:nrow(full_data), 0.8*nrow(full_data))
# train <- full_data %>% filter(year<2019)
# test <- full_data %>% filter(year==2019)
# 
# # fit a model for each vaccine
# model1 <- glm(prop_val_MCV1~factor(region)+year+result, data=train, family = "binomial")
# summary(model1)
# par(mfrow=c(2,2))
# plot(model1)

# evaluate the success of the model 
# pred1 <- predict(model1, newdata = test, type = "response")
# BrierScore(model1)
# par(mfrow=c(1,1))
# plot(test$prop_val_MCV1, pred1)

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
