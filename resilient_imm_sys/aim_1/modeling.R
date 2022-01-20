## Prep NIS Vaccination Data
# Author: Francisco Rios 
# Purpose: Load NIS data and necessary variables
# Date: Last modified January 07, 2022

## This script will create two data sets
# Data set 1: Change in vaccination rates for all regions in the NIS
# Data set 2: Change in vaccination rates for all racial/ethnic groups for all regions in the NIS

## Load prepped data for 2019 and 2007 ----
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/prepped_data/NISPUF19.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/prepped_data/NISPUF07.RData")

## General data prep -----

# subset columns in each dataset
nis19 <- NISPUF19 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACE_K, RACEETHK, I_HISP_K, STATE, EST_GRANT, ESTIAP19, EDUC1, INCQ298A, P_NUMHEP, P_NUMMMR, P_NUMDTP, INCPOV1)
nis07 <- NISPUF07 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACE_K, RACEETHK, I_HISP_K, STATE,            ESTIAP07, EDUC1, INCQ298A, P_NUMHEP, P_NUMMMR, P_NUMDTP, INCPOV1)

# filter children with adequate provider data
nis19 <- nis19 %>% filter(PDAT==1)
nis07 <- nis07 %>% filter(PDAT==1)

# drop Puerto Rico from 2019 survey data  (which only exists in 2019 survey)
nis19 <- nis19 %>% filter(ESTIAP19!=106)

# create a comparable Geographic strata variable in the 2007 survey
nis07$ESTIAP19 <- nis07$ESTIAP07
nis07 <- nis07 %>% 
  mutate(ESTIAP19 = case_when(
    ESTIAP07==24 ~ 22,
    ESTIAP07==37 ~ 36,
    ESTIAP07==69 ~ 68,
    ESTIAP07==79 ~ 68,
    ESTIAP07==80 ~ 68,
    ESTIAP07==773 ~ 77,
    TRUE ~ as.double(ESTIAP07)
  ))

# change format to facilitate later merge
nis07$ESTIAP19 <- as.integer(nis07$ESTIAP19)
label(nis07$ESTIAP19) <- "ESTIMATION AREA OF RESIDENCE"

# derive new variables on vaccination coverage for DTP, MMR, and Hep B
nis19$dtp_vac <- nis19$P_NUMDTP
nis19$mmr_vac <- nis19$P_NUMMMR
nis19$hep_vac <- nis19$P_NUMHEP

nis07$dtp_vac <- nis07$P_NUMDTP
nis07$mmr_vac <- nis07$P_NUMMMR
nis07$hep_vac <- nis07$P_NUMHEP

# create new indicator of fully vaccinated status
nis19 <- nis19 %>% 
  mutate(dtp_vac = case_when(
    dtp_vac>=4 ~ 1,
    dtp_vac<4 ~ 0)) %>% 
  mutate(mmr_vac = case_when(
    mmr_vac>=1 ~ 1,
    mmr_vac<1 ~ 0)) %>% 
  mutate(hep_vac = case_when(
    hep_vac>=3 ~ 1,
    hep_vac<3 ~ 0))

nis07 <- nis07 %>% 
  mutate(dtp_vac = case_when(
    dtp_vac>=4 ~ 1,
    dtp_vac<4 ~ 0)) %>% 
  mutate(mmr_vac = case_when(
    mmr_vac>=1 ~ 1,
    mmr_vac<1 ~ 0)) %>% 
  mutate(hep_vac = case_when(
    hep_vac>=3 ~ 1,
    hep_vac<3 ~ 0))

# change reference values for raceethk
nis07$RACEETHK_R <- nis07$RACEETHK

nis07 <- nis07 %>% 
  mutate(RACEETHK_R = case_when(
    RACEETHK_R==4 ~ 4,
    RACEETHK_R==3 ~ 3,
    RACEETHK_R==2 ~ 1, 
    RACEETHK_R==1 ~ 2))


# fit model 1
glm.fit <- glm(dtp_vac ~ as.factor(ESTIAP19) + as.factor(RACEETHK_R) + as.factor(INCPOV1), data = nis07, family = binomial)
summary(glm.fit)

# create table of values of interest
newdata2 <- with(nis07, data.frame(ESTIAP19 = rep(unique(nis07$ESTIAP19), 4), 
                                   RACEETHK_R = factor(rep(1:4, each = 58)),
                                   INCPOV1 = factor(rep(1:3, each = 232))))

# predict vaccination probability using newly fit model
newdata3 <- cbind(newdata2, predict(glm.fit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
