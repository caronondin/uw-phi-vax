## Prep NIS Vaccination Data
# Author: Francisco Rios 
# Purpose: Load NIS data and run estimation models
# Date: Last modified January 07, 2022

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

# load the dataset
data <- readRDS(paste0(prepped_data_dir, "01_complete_nis_data.RDS"))

# fit 1 model per vaccine
glm.dtp.fit <- glm(dtp_vac ~ factor(YEAR) + as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data, family = binomial)
glm.mmr.fit <- glm(mmr_vac ~ factor(YEAR) + as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data, family = binomial)
glm.hep.fit <- glm(hep_vac ~ factor(YEAR) + as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data, family = binomial)

# create table of values of interest
newdata <- with(data, data.frame(ESTIAP = rep(unique(data$ESTIAP), each=224, length.out=12544),
                                    YEAR = rep(2007:2020, each=16, length.out=12544),
                                    RACEETHK_R = factor(rep(1:4, each=4, length.out=12544)),
                                    INCPOV1 = factor(rep(1:4, length.out=12544))))

# predict vaccination probability using newly fit models
predictDTP <- cbind(newdata, predict(glm.dtp.fit, newdata = newdata, type = "link",
                                    se = TRUE))
predictMMR <- cbind(newdata, predict(glm.mmr.fit, newdata = newdata, type = "link",
                                    se = TRUE))
predictHEP <- cbind(newdata, predict(glm.hep.fit, newdata = newdata, type = "link",
                                    se = TRUE))

# compute confidence intervals
predictDTP <- within(predictDTP, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

predictMMR <- within(predictMMR, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
predictHEP <- within(predictHEP, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# add indicator for vaccine type for each model
predictDTP$VACCINE <- "DTP"
predictMMR$VACCINE <- "MMR"
predictHEP$VACCINE <- "HEP B"

# bind files together
estimated_coverage_data <- bind_rows(predictDTP, predictMMR, predictHEP)
  
# save estimated coverage from models
saveRDS(estimated_coverage_data, file=paste0(prepped_data_dir, "02_estimated_vaccine_coverage.RDS" ))
