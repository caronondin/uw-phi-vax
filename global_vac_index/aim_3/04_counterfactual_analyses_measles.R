# Author: Francisco Rios 
# Purpose: Prep model for Aim 3 Analyses
# Date: Last modified May 4 2022

rm(list=ls())

# set up files
if (Sys.info()[6]=="frc2"){
  source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_3/01_set_up_R.R"))
}

# load plm package
library(plm)

# Load data set
full_data <- readRDS(file=paste0(prepped_data_dir, "aim_3/02_prepped_full_data.RDS"))

########################################
##### Part 1: Model the impacts of the worst-performer
##### becoming the best performer 
##### Measles 2019
########################################

# subset data to exclude 2019 since this will be the year of prediction
data_subset <- full_data %>% filter(year <2019)

# create a training dataset
row.number <- sample(1:nrow(data_subset), 0.8*nrow(data_subset))
train <- data_subset[row.number,]
test <- data_subset[-row.number,]

# Model 1: Use index to predict measles vaccine coverage
model1 <- glm(prop_val_MCV1~factor(region)+year+result, data=train, family = "binomial")

# create training data
newdata <- with(full_data, data.frame(location = rep(unique(full_data$location), each=1, length.out=175),
                                      year = rep(2019, length.out=175)))

newdata <- newdata %>% left_join(full_data, by=c("location", "year"))

# split data according to worst performer and best performer
worst_performer <- newdata %>% filter(location=="Somalia")
best_performer <- newdata %>% filter(location=="Eswatini")

# set index value for worst performer to equal the best performer's value
worst_performer$result <- best_performer$result

# use Model 1 (measles) to predict vaccination coverage given new index value
pred2019mcv <- predict(model1, newdata = worst_performer, type = "response")

########################################
##### Part 2:  
##### counterfactual analysis: disease burden
##### under different levels of vax coverage
########################################

# fit model predicting disease burden based on vaccination coverage for measles

# the best coverage predicted in 2019:  .903
# actual vaccination coverage in 2019:  .245

# fit model that predicts disease burden according to level of vaccination coverage

# outcome variable: burden (in DALYs per 100,000 persons)
# predictor variable: vaccination coverage (as percentage)

# log transform the outcome variable
full_data$log_dalys_measles_rate <- log(full_data$dalys_measles_rate+1)

# predict new disease burden with new vaccination coverage levels in Somalia
model2 <- lm(formula = log_dalys_measles_rate ~ prop_val_MCV1+location+year, data=full_data)

sink(paste0(file_folder, "/Results/aim_3/01_regression_output.txt"))
print(summary(model2))
sink()  # returns output to the console

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)),
         xlab = "Vaccination coverage (MCV1)",
         ylab = "Measles DALYs")
}

# ggplotRegression(model1)
ggplotRegression(model2)

# create new data that has different levels of vaccination coverage ranging from worst to best
somalia_data <- with(full_data, data.frame(location = rep("Somalia", each=1, length.out=6),
                                      year = rep(2019, each=6),
                                      prop_val_MCV1 = seq(from=.245, to=.903, by=.1316)))

# create new data that has different levels of vaccination coverage
full_data_subset <- full_data %>% rename(orig_prop_val_MCV1=prop_val_MCV1)
somalia_newdata <- somalia_data %>% left_join(full_data_subset, by=c("location", "year"))

# Use model 2 to predict disease burden under different levels of vaccination coverage
pred2019measdalys <- predict(model2, newdata = somalia_newdata, type = "response")

# create table of predicted disease burden under different vaccination coverage scenarios
table <- somalia_data

# create new predicted coverage variable
table$predicted_meas_log_dalys <- NA
for (i in 1:6) {
  table$predicted_meas_log_dalys[i] <- pred2019measdalys[i]
}

# transform the predicted values so they are not on the log scale
table$predicted_meas_dalys <- exp(table$predicted_meas_log_dalys) - 1

# save table of predicted disease burden under different levels of vaccination coverage
write.csv(table, file=paste0(resDir, "aim_3/02_predicted_measles_counterfactual_results.csv"))




