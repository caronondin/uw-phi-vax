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

# seperate the data into years
data.2007 <- data %>% filter(YEAR==2007)
data.2019 <- data %>% filter(YEAR==2019)

# fit 1 model per vaccine
glm.dtp.fit.07 <- glm(dtp_vac ~ as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data.2007, family = binomial)
glm.mmr.fit.07 <- glm(mmr_vac ~ as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data.2007, family = binomial)
glm.hep.fit.07 <- glm(hep_vac ~ as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data.2007, family = binomial)

glm.dtp.fit.19 <- glm(dtp_vac ~ as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data.2019, family = binomial)
glm.mmr.fit.19 <- glm(mmr_vac ~ as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data.2019, family = binomial)
glm.hep.fit.19 <- glm(hep_vac ~ as.factor(ESTIAP) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = data.2019, family = binomial)

# create table of values of interest
newdata2007 <- with(data, data.frame(ESTIAP = rep(unique(data$ESTIAP), each=16, length.out=928),
                                 YEAR = rep(c(2007), each=16, length.out=928),
                                 RACEETHK_R = factor(rep(1:4, each=4, length.out=928)),
                                 INCPOV1 = factor(rep(1:4, length.out=928))))

newdata2019 <- with(data, data.frame(ESTIAP = rep(unique(data$ESTIAP), each=16, length.out=928),
                                 YEAR = rep(c(2019), each=16, length.out=928),
                                 RACEETHK_R = factor(rep(1:4, each=4, length.out=928)),
                                 INCPOV1 = factor(rep(1:4, length.out=928))))



# predict vaccination probability using newly fit models for each year
predictDTP07 <- cbind(newdata2007, predict(glm.dtp.fit.07, newdata = newdata2007, type = "link",
                                     se = TRUE))
predictMMR07 <- cbind(newdata2007, predict(glm.mmr.fit.07, newdata = newdata2007, type = "link",
                                     se = TRUE))
predictHEP07 <- cbind(newdata2007, predict(glm.hep.fit.07, newdata = newdata2007, type = "link",
                                     se = TRUE))

predictDTP19 <- cbind(newdata2019, predict(glm.dtp.fit.19, newdata = newdata2019, type = "link",
                                     se = TRUE))
predictMMR19 <- cbind(newdata2019, predict(glm.mmr.fit.19, newdata = newdata2019, type = "link",
                                     se = TRUE))
predictHEP19 <- cbind(newdata2019, predict(glm.hep.fit.19, newdata = newdata2019, type = "link",
                                     se = TRUE))

# compute confidence intervals
predictDTP07 <- within(predictDTP07, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

predictMMR07 <- within(predictMMR07, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
predictHEP07 <- within(predictHEP07, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

predictDTP19 <- within(predictDTP19, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

predictMMR19 <- within(predictMMR19, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
predictHEP19 <- within(predictHEP19, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})


# add indicator for vaccine type for each model
predictDTP07$VACCINE <- "DTP"
predictMMR07$VACCINE <- "MMR"
predictHEP07$VACCINE <- "HEP B"

predictDTP19$VACCINE <- "DTP"
predictMMR19$VACCINE <- "MMR"
predictHEP19$VACCINE <- "HEP B"

# bind files together
estimated_coverage_data <- bind_rows(predictDTP07, predictMMR07, predictHEP07, predictDTP19, predictMMR19, predictHEP19)

# save data
saveRDS(estimated_coverage_data, file = paste0(prepped_data_dir, "03_estimates_vaccine_coverage_2007-2019.RDS"))
