# Author: Francisco Rios 
# Purpose: Subset data to years of interest and model vaccination coverage
# Date: Last modified January 26, 2022

rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

# Maybe fit a new model using only two years of data
# load the dataset
data <- readRDS(paste0(prepped_data_dir, "01_complete_nis_data.RDS")) %>% 
  filter(YEAR==2007 | YEAR==2019) %>% # keep only first and last year
  select(-c(ESTIAP)) # remove previously calculated area variable

# edit the location variable to make sure it includes the additional locations that are available in both years of interest
data <- data %>%
  mutate(ESTIAP = case_when(
    ESTIAP_ORIG==24 ~ 22, # FL-Miami-Dade County
    ESTIAP_ORIG==37 ~ 36, # IN-Marion County
    ESTIAP_ORIG==69 ~ 68, # CA-Los Angeles County
    ESTIAP_ORIG==79 ~ 68, # CA-Alameda County
    ESTIAP_ORIG==80 ~ 68, # CA-San Bernardino County
    ESTIAP_ORIG==773 ~ 77, # TX-Western Washington
    TRUE ~ as.double(ESTIAP_ORIG)
  ))

# fit 1 model per vaccine
glm.dtp.fit <- glm(dtp_vac ~ factor(YEAR) + as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data, family = binomial)
glm.mmr.fit <- glm(mmr_vac ~ factor(YEAR) + as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data, family = binomial)
glm.hep.fit <- glm(hep_vac ~ factor(YEAR) + as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data, family = binomial)

# create table of values of interest
newdata <- with(data, data.frame(ESTIAP = rep(unique(data$ESTIAP), each=32, length.out=1856),
                                 YEAR = rep(c(2007,2019), each=16, length.out=1856),
                                 RACEETHK_R = factor(rep(1:4, each=4, length.out=1856)),
                                 INCPOV1 = factor(rep(1:4, length.out=1856))))


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

# save data
saveRDS(estimated_coverage_data, file = paste0(prepped_data_dir, "03_estimates_vaccine_coverage_2007-2019.RDS"))