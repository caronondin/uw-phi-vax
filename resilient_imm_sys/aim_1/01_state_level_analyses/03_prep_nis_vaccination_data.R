## Prep NIS Vaccination Data
# Author: Francisco Rios 
# Purpose: This script will create complete NIS dataset for past few years
# Date: Last modified January 07, 2022

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

## Load prepped data between 2007 and 2020----
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF07.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF08.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF09.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF10.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF11.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF12.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF13.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF14.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF15.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF16.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF17.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF18.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF19.RData")
load("C:/Users/frc2/UW/og_merck_resilient_immunization_programs_project - Aim 1/Data/raw_data/cdc/r_files/NISPUF20.RData")

## General data prep -----

# subset columns in each dataset -----
nis07 <- NISPUF07 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP07, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, # demographics
                             INS_1, INS_2, INS_3, INS_4, INS_5, INS_6, INS_11,  # insurance information
                             P_NUMHEP, P_NUMMMR, P_NUMDTP) # vaccination doses

nis08 <- NISPUF08 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP08, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4, INS_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis09 <- NISPUF09 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP09, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis10 <- NISPUF10 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP10, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis11 <- NISPUF11 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP11, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis12 <- NISPUF12 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP12, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis13 <- NISPUF13 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP13, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis14 <- NISPUF14 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP14, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis15 <- NISPUF15 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP15, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis16 <- NISPUF16 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP16, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_STAT_I, INS_BREAK_I, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis17 <- NISPUF17 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP17, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_STAT2_I, INS_BREAK_I, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis18 <- NISPUF18 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP18, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_STAT2_I, INS_BREAK_I, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis19 <- NISPUF19 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP19, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_STAT2_I, INS_BREAK_I, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

nis20 <- NISPUF20 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP20, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_STAT2_I, INS_BREAK_I, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

# merge datasets together into four groups based on iterations of key variables -----
data1 <- bind_rows(nis07, nis08)
data2 <- bind_rows(nis09, nis10, nis11, nis12, nis13, nis14, nis15)
data3 <- nis16
data4 <- bind_rows(nis17, nis18, nis19, nis20)

# create indicator of insurance type-----

# replace NAs with 0 for datasets with more detailed insurance indicators
data1 <- data1 %>% replace_na(list(INS_1 = 0, INS_2 = 0, INS_3 = 0, INS_4   = 0, INS_5 = 0, INS_6 = 0))
data2 <- data2 %>% replace_na(list(INS_1 = 0, INS_2 = 0, INS_3 = 0, INS_4_5 = 0, INS_6 = 0))

# create three new indicator variables for private insurance, medicaid, or other insurance

# ------
# WHAT ABOUT MISSIGNESS: HOW TO TAKE THOSE INTO ACCOUNT?
# WHAT ABOUT BREAK IN INSURANCE
# -----

data1 <- data1 %>%
  mutate(private_ins = case_when(
    INS_1==1 ~ 1,
    TRUE ~ 0 )) %>%
  mutate(medicaid = case_when(
    INS_2==1 ~ 1,
    TRUE ~ 0 )) %>% 
  mutate(other_ins = case_when(
    INS_3==1 | INS_4==1 | INS_5==1 | INS_6==1 ~ 1, # any other insurance 
    TRUE ~ 0 ))

data2 <- data2 %>% 
  mutate(private_ins = case_when(
    INS_1==1 ~ 1,
    TRUE ~ 0 )) %>%
  mutate(medicaid = case_when(
    INS_2==1 ~ 1, 
    TRUE ~ 0)) %>%
  mutate(other_ins = case_when(
    INS_3==1 | INS_4_5==1 | INS_6==1 ~ 1,
    TRUE ~ 0))

# create composite index to create variable that matches other years
data1 <- data1 %>% 
  mutate(INSURANCE = case_when(
    private_ins==1 & medicaid==0 & other_ins==0 ~ 1, # private insurance only
    private_ins==0 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==1 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==0 & medicaid==0 & other_ins==0 ~ 4  # uninsured
  ))

data2 <- data2 %>% 
  mutate(INSURANCE = case_when(
    private_ins==1 & medicaid==0 & other_ins==0 ~ 1, # private insurance only
    private_ins==0 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==1 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==0 & medicaid==0 & other_ins==0 ~ 4  # uninsured
  ))

data3$INSURANCE <- data3$INS_STAT_I

data4$INSURANCE <- data4$INS_STAT2_I

# bind each data frame together ----
full_data <- bind_rows(data1, data2, data3, data4)

# create new variable of original estimation area
full_data <- full_data %>%
  mutate(ESTIAP_ORIG = case_when(
    YEAR==2007 ~ ESTIAP07,
    YEAR==2008 ~ ESTIAP08,
    YEAR==2009 ~ ESTIAP09,
    YEAR==2010 ~ ESTIAP10,
    YEAR==2011 ~ ESTIAP11,
    YEAR==2012 ~ ESTIAP12,
    YEAR==2013 ~ ESTIAP13,
    YEAR==2014 ~ ESTIAP14,
    YEAR==2015 ~ ESTIAP15,
    YEAR==2016 ~ ESTIAP16,
    YEAR==2017 ~ ESTIAP17, 
    YEAR==2018 ~ ESTIAP18,
    YEAR==2019 ~ ESTIAP19,
    YEAR==2020 ~ ESTIAP20))

# create new variable that will standardize estimation areas
full_data <- full_data %>%
  mutate(ESTIAP = case_when(
    ESTIAP_ORIG==15 ~ 14, # City of Baltimore
    ESTIAP_ORIG==24 ~ 22, #Miami-Dade County
    ESTIAP_ORIG==37 ~ 36, # IN-Marion County
    ESTIAP_ORIG==52 ~ 51, # TX-Dallas County
    ESTIAP_ORIG==53 ~ 51, # TX-El Paso County
    ESTIAP_ORIG==69 ~ 68, # CA-Los Angeles County
    ESTIAP_ORIG==70 ~ 68, # CA-Santa Clara County
    ESTIAP_ORIG==79 ~ 68, # CA-Alameda County
    ESTIAP_ORIG==80 ~ 68, # CA-San Bernardino County
    ESTIAP_ORIG==85 ~ 68, # CA- Northern Ca
    ESTIAP_ORIG==91 ~ 22, # FL-Orange County
    ESTIAP_ORIG==92 ~ 34, # IL-Madison/St.Clair Counties
    ESTIAP_ORIG==93 ~ 40, # MN-Twin Cities
    ESTIAP_ORIG==96 ~ 36, # IN-Lake County
    ESTIAP_ORIG==97 ~ 77, # WA-Eastern Washington
    ESTIAP_ORIG==102 ~ 77, # WA-Western WA
    ESTIAP_ORIG==103 ~ 14, # MD-Prince George's County
    ESTIAP_ORIG==107 ~ 51, # TX-Hidalgo County
    ESTIAP_ORIG==108 ~ 51, # TX-Travis County
    ESTIAP_ORIG==109 ~ 51, # TX-Tarrant County
    ESTIAP_ORIG==773 ~ 77, # TX-Western Washington
    ESTIAP_ORIG==774 ~ 77, # WA-Eastern/Western WA
    TRUE ~ as.double(ESTIAP_ORIG)
  ))

# create new indicator of fully vaccinated status
full_data <- full_data %>% 
  mutate(dtp_vac = case_when(
    P_NUMDTP>=4 ~ 1,
    P_NUMDTP<4 ~ 0)) %>% 
  mutate(mmr_vac = case_when(
    P_NUMMMR>=1 ~ 1,
    P_NUMMMR<1 ~ 0)) %>% 
  mutate(hep_vac = case_when(
    P_NUMHEP>=3 ~ 1,
    P_NUMHEP<3 ~ 0))

# change reference values for raceethk
full_data <- full_data %>% 
  mutate(RACEETHK_R = case_when(
    RACEETHK==4 ~ 4,
    RACEETHK==3 ~ 3,
    RACEETHK==2 ~ 1, 
    RACEETHK==1 ~ 2))


full_data <- full_data %>% 
  filter(PDAT==1) %>% # filter children with adequate provider data
  filter(ESTIAP!=95 & ESTIAP!=105 & ESTIAP!=106) # filter US Virgin Islands, Guam, Puerto Rico (only a few surveys)


# select columns of interest in each data set
full_data <- full_data %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, RACEETHK_R, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                              INSURANCE, ESTIAP, ESTIAP_ORIG, 
                              P_NUMHEP, P_NUMMMR, P_NUMDTP, dtp_vac, mmr_vac, hep_vac)

# Save final dataset
saveRDS(full_data, file = paste0(prepped_data_dir, "01_complete_nis_data.RDS"))
