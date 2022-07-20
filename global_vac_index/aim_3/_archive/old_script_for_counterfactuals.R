




# old code to scrap--eventually
# # plot(dalys_measles_rate ~ prop_val_MCV1, data = full_data)
# plot(pred2019mcv, newdata$prop_val_MCV1,
#      xlab='Predicted Vaccination Coverage',
#      ylab='Actual Vaccination Coverage',
#      main='Predicted vs. Actual Values MCV, 2019')
# abline(a=0, b=1)
# BrierScore(pred2019mcv, newdata$prop_val_MCV1)
# 
# pred2019dtp1 <- predict(model2, newdata=newdata, type = "response")
# plot(pred2019dtp1, newdata$prop_val_DTP1,
#      xlab='Predicted Vaccination Coverage',
#      ylab='Actual Vaccination Coverage',
#      main='Predicted vs. Actual Values DTP1, 2019')
# abline(a=0, b=1)
# BrierScore(pred2019dtp1, newdata$prop_val_MCV1)
# 
# pred2019dtp3 <- predict(model3, newdata=newdata, type = "response")
# plot(pred2019dtp3, newdata$prop_val_DTP3)
# plot(pred2019dtp3, newdata$prop_val_DTP3,
#      xlab='Predicted Vaccination Coverage',
#      ylab='Actual Vaccination Coverage',
#      main='Predicted vs. Actual Values DTP3, 2019')
# abline(a=0, b=1)
# BrierScore(pred2019dtp3, newdata$prop_val_DTP3)
# 
# 
# # evaluate the success of the model 
# pred1 <- predict(model1, newdata = test, type = "response")
# BrierScore(model1)
# par(mfrow=c(1,1))
# plot(pred1, test$prop_val_MCV1,
#      xlab='Predicted Vaccination Coverage',
#      ylab='Actual Vaccination Coverage',
#      main='Predicted vs. Actual Values MCV, 1990-2018')
# abline(a=0, b=1)
# BrierScore(test$prop_val_MCV1, pred1)
# 
# 
# 
# # conduct analysis with one country
# # best performing location in low-SDI group is Eswatini, Worst performing location on the index in low-SDI group: Somalia
# 
# 
# load vaccination disease burden
# measles_data <- readRDS(paste0(prepped_data_dir, "aim_1/05_disease_trends.RDS")) %>%
#   filter(cause_name=="Measles")
# 
# # ####
# # MIGHT NEED TO DOWNLOAD DALYs FOR AIM 3--AS AN OPTION
# # ####
# 
# # merge by location
# 
# # save output of results