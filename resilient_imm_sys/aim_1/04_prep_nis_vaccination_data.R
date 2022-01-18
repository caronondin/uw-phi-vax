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
nis19 <- NISPUF19 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACE_K, RACEETHK, I_HISP_K, STATE, EST_GRANT, ESTIAP19, EDUC1, INCQ298A, P_NUMHEP, P_NUMMMR, P_NUMDTP)
nis07 <- NISPUF07 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACE_K, RACEETHK, I_HISP_K, STATE,            ESTIAP07, EDUC1, INCQ298A, P_NUMHEP, P_NUMMMR, P_NUMDTP)

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

## Calculate vaccination rates in each state -----
state_cov_19 <- nis19 %>% 
  group_by(ESTIAP19, YEAR) %>% 
  summarise(across(c(dtp_vac, mmr_vac, hep_vac), list(prop = ~sum((.))/length(.))))

state_cov_07 <- nis07 %>% 
  group_by(ESTIAP19, YEAR) %>% 
  summarise(across(c(dtp_vac, mmr_vac, hep_vac), list(prop = ~sum((.))/length(.))))

# merge to derive new variables and calculations of interest
state_vac_cov <- rbind(state_cov_07, state_cov_19)

## Calculate the sample size for each year and state -----
state_n_cov_19 <- nis19 %>% 
  group_by(ESTIAP19, YEAR) %>%
  summarise(sample_size=n())

state_n_cov_07 <- nis07 %>%
  group_by(ESTIAP19, YEAR) %>%
  summarise(sample_size=n())

# bind sample size from different years together
state_sample_size <- rbind(state_n_cov_07, state_n_cov_19)

# merge sample size info to dataset
stat_vac_cov_merged <- state_vac_cov %>% full_join(state_sample_size, by=c("ESTIAP19", "YEAR"))

# reshape the data to facilitate analyses
state_vac_cov_final <- pivot_wider(stat_vac_cov_merged,
            id_cols = ESTIAP19, 
            names_from = YEAR,
            values_from = c(dtp_vac_prop, mmr_vac_prop, hep_vac_prop, sample_size))

# calculate the difference between the two time points
state_vac_cov_final$change_dtp <- (state_vac_cov_final$dtp_vac_prop_2019 - state_vac_cov_final$dtp_vac_prop_2007)/state_vac_cov_final$dtp_vac_prop_2007*100
state_vac_cov_final$change_mmr <- (state_vac_cov_final$mmr_vac_prop_2019 - state_vac_cov_final$mmr_vac_prop_2007)/state_vac_cov_final$mmr_vac_prop_2007*100
state_vac_cov_final$change_hep <- (state_vac_cov_final$hep_vac_prop_2019 - state_vac_cov_final$hep_vac_prop_2007)/state_vac_cov_final$hep_vac_prop_2007*100

# re-arrange columns
state_vac_cov_final <- state_vac_cov_final %>% relocate( 
                                 c(sample_size_2007, sample_size_2019),
                                 .after = ESTIAP19)
                                                        

# reformat state names for output


## Save data set 1 -----
saveRDS(state_vac_cov_final, file = paste0(prepped_data_dir, "01_state_vaccination_rates.RDS"))

## Calculate vaccination rates in each area of estimation and racial/ethnic group -----

# sum vaccination coverage according to estimation area and race and ethnicity
eth_cov_19 <- nis19 %>% 
  group_by(ESTIAP19, YEAR, RACEETHK) %>% 
  summarise(across(c(dtp_vac, mmr_vac, hep_vac), list(prop = ~sum((.))/length(.))))

eth_cov_07 <- nis07 %>%
  group_by(ESTIAP19, YEAR, RACEETHK) %>% 
  summarise(across(c(dtp_vac, mmr_vac, hep_vac), list(prop = ~sum((.))/length(.))))

# merge to derive new variables and calculations of interest
eth_vac_cov <- rbind(eth_cov_07, eth_cov_19)

## Calculate the sample size for each year and state and racial/ethnic group -----
eth_n_cov_19 <- nis19 %>% 
  group_by(ESTIAP19, YEAR, RACEETHK) %>%
  summarise(sample_size=n())

eth_n_cov_07 <- nis07 %>%
  group_by(ESTIAP19, YEAR, RACEETHK) %>%
  summarise(sample_size=n())

# merge and reshape the data
raceeth_sample_size <- rbind(eth_n_cov_07, eth_n_cov_19)

# merge sample size with vaccination coverage data
eth_vac_cov_merged <- eth_vac_cov %>% full_join(raceeth_sample_size, by=c("ESTIAP19", "YEAR", "RACEETHK"))

# reshape the data to facilitate analyses
eth_vac_cov_wide <- pivot_wider(eth_vac_cov_merged,
                             id_cols = c(ESTIAP19), 
                             names_from = c(YEAR,RACEETHK),
                             values_from = c(dtp_vac_prop, mmr_vac_prop, hep_vac_prop, sample_size)) 

## Save the final with complete information -----
saveRDS(eth_vac_cov_wide, file = paste0(prepped_data_dir, "02_state_vaccination_rates_by_race_ethnicity.RDS"))

# Drop values with insufficient sample size of 30
eth_vac_cov_final <- eth_vac_cov_wide

# set cutoff value
n_cutoff <- 30

eth_vac_cov_final$dtp_vac_prop_2007_1 <- replace(eth_vac_cov_final$dtp_vac_prop_2007_1, eth_vac_cov_final$sample_size_2007_1<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2007_2 <- replace(eth_vac_cov_final$dtp_vac_prop_2007_2, eth_vac_cov_final$sample_size_2007_2<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2007_3 <- replace(eth_vac_cov_final$dtp_vac_prop_2007_3, eth_vac_cov_final$sample_size_2007_3<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2007_4 <- replace(eth_vac_cov_final$dtp_vac_prop_2007_4, eth_vac_cov_final$sample_size_2007_4<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2019_1 <- replace(eth_vac_cov_final$dtp_vac_prop_2019_1, eth_vac_cov_final$sample_size_2019_1<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2019_2 <- replace(eth_vac_cov_final$dtp_vac_prop_2019_2, eth_vac_cov_final$sample_size_2019_2<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2019_3 <- replace(eth_vac_cov_final$dtp_vac_prop_2019_3, eth_vac_cov_final$sample_size_2019_3<n_cutoff, NA)
eth_vac_cov_final$dtp_vac_prop_2019_4 <- replace(eth_vac_cov_final$dtp_vac_prop_2019_4, eth_vac_cov_final$sample_size_2019_4<n_cutoff, NA)

eth_vac_cov_final$mmr_vac_prop_2007_1 <- replace(eth_vac_cov_final$mmr_vac_prop_2007_1, eth_vac_cov_final$sample_size_2007_1<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2007_2 <- replace(eth_vac_cov_final$mmr_vac_prop_2007_2, eth_vac_cov_final$sample_size_2007_2<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2007_3 <- replace(eth_vac_cov_final$mmr_vac_prop_2007_3, eth_vac_cov_final$sample_size_2007_3<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2007_4 <- replace(eth_vac_cov_final$mmr_vac_prop_2007_4, eth_vac_cov_final$sample_size_2007_4<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2019_1 <- replace(eth_vac_cov_final$mmr_vac_prop_2019_1, eth_vac_cov_final$sample_size_2019_1<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2019_2 <- replace(eth_vac_cov_final$mmr_vac_prop_2019_2, eth_vac_cov_final$sample_size_2019_2<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2019_3 <- replace(eth_vac_cov_final$mmr_vac_prop_2019_3, eth_vac_cov_final$sample_size_2019_3<n_cutoff, NA)
eth_vac_cov_final$mmr_vac_prop_2019_4 <- replace(eth_vac_cov_final$mmr_vac_prop_2019_4, eth_vac_cov_final$sample_size_2019_4<n_cutoff, NA)

eth_vac_cov_final$hep_vac_prop_2007_1 <- replace(eth_vac_cov_final$hep_vac_prop_2007_1, eth_vac_cov_final$sample_size_2007_1<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2007_2 <- replace(eth_vac_cov_final$hep_vac_prop_2007_2, eth_vac_cov_final$sample_size_2007_2<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2007_3 <- replace(eth_vac_cov_final$hep_vac_prop_2007_3, eth_vac_cov_final$sample_size_2007_3<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2007_4 <- replace(eth_vac_cov_final$hep_vac_prop_2007_4, eth_vac_cov_final$sample_size_2007_4<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2019_1 <- replace(eth_vac_cov_final$hep_vac_prop_2019_1, eth_vac_cov_final$sample_size_2019_1<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2019_2 <- replace(eth_vac_cov_final$hep_vac_prop_2019_2, eth_vac_cov_final$sample_size_2019_2<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2019_3 <- replace(eth_vac_cov_final$hep_vac_prop_2019_3, eth_vac_cov_final$sample_size_2019_3<n_cutoff, NA)
eth_vac_cov_final$hep_vac_prop_2019_4 <- replace(eth_vac_cov_final$hep_vac_prop_2019_4, eth_vac_cov_final$sample_size_2019_4<n_cutoff, NA)

# calculate the difference between the two time points for each racial/ethnic group
eth_vac_cov_final$change_dtp_1 <- (eth_vac_cov_final$dtp_vac_prop_2019_1 - eth_vac_cov_final$dtp_vac_prop_2007_1)/eth_vac_cov_final$dtp_vac_prop_2007_1*100
eth_vac_cov_final$change_mmr_1 <- (eth_vac_cov_final$mmr_vac_prop_2019_1 - eth_vac_cov_final$mmr_vac_prop_2007_1)/eth_vac_cov_final$mmr_vac_prop_2007_1*100
eth_vac_cov_final$change_hep_1 <- (eth_vac_cov_final$hep_vac_prop_2019_1 - eth_vac_cov_final$hep_vac_prop_2007_1)/eth_vac_cov_final$hep_vac_prop_2007_1*100

# calculate the difference between the two time points for each racial/ethnic group
eth_vac_cov_final$change_dtp_2 <- (eth_vac_cov_final$dtp_vac_prop_2019_2 - eth_vac_cov_final$dtp_vac_prop_2007_2)/eth_vac_cov_final$dtp_vac_prop_2007_2*100
eth_vac_cov_final$change_mmr_2 <- (eth_vac_cov_final$mmr_vac_prop_2019_2 - eth_vac_cov_final$mmr_vac_prop_2007_2)/eth_vac_cov_final$mmr_vac_prop_2007_2*100
eth_vac_cov_final$change_hep_2 <- (eth_vac_cov_final$hep_vac_prop_2019_2 - eth_vac_cov_final$hep_vac_prop_2007_2)/eth_vac_cov_final$hep_vac_prop_2007_2*100

# calculate the difference between the two time points for each racial/ethnic group
eth_vac_cov_final$change_dtp_3 <- (eth_vac_cov_final$dtp_vac_prop_2019_3 - eth_vac_cov_final$dtp_vac_prop_2007_3)/eth_vac_cov_final$dtp_vac_prop_2007_3*100
eth_vac_cov_final$change_mmr_3 <- (eth_vac_cov_final$mmr_vac_prop_2019_3 - eth_vac_cov_final$mmr_vac_prop_2007_3)/eth_vac_cov_final$mmr_vac_prop_2007_3*100
eth_vac_cov_final$change_hep_3 <- (eth_vac_cov_final$hep_vac_prop_2019_3 - eth_vac_cov_final$hep_vac_prop_2007_3)/eth_vac_cov_final$hep_vac_prop_2007_3*100

eth_vac_cov_final$change_dtp_4 <- (eth_vac_cov_final$dtp_vac_prop_2019_4 - eth_vac_cov_final$dtp_vac_prop_2007_4)/eth_vac_cov_final$dtp_vac_prop_2007_4*100
eth_vac_cov_final$change_mmr_4 <- (eth_vac_cov_final$mmr_vac_prop_2019_4 - eth_vac_cov_final$mmr_vac_prop_2007_4)/eth_vac_cov_final$mmr_vac_prop_2007_4*100
eth_vac_cov_final$change_hep_4 <- (eth_vac_cov_final$hep_vac_prop_2019_4 - eth_vac_cov_final$hep_vac_prop_2007_4)/eth_vac_cov_final$hep_vac_prop_2007_4*100

# for each vaccine and racial/ethnic group calculate the equitable improvement index: which is the relative difference between the two values
eth_vac_cov_final$eii_dtp_1 <- (eth_vac_cov_final$change_dtp_1 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
eth_vac_cov_final$eii_dtp_3 <- (eth_vac_cov_final$change_dtp_3 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
eth_vac_cov_final$eii_dtp_4 <- (eth_vac_cov_final$change_dtp_4 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100

eth_vac_cov_final$eii_mmr_1 <- (eth_vac_cov_final$change_mmr_1 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
eth_vac_cov_final$eii_mmr_3 <- (eth_vac_cov_final$change_mmr_3 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
eth_vac_cov_final$eii_mmr_4 <- (eth_vac_cov_final$change_mmr_4 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100

eth_vac_cov_final$eii_hep_1 <- (eth_vac_cov_final$change_hep_1 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
eth_vac_cov_final$eii_hep_3 <- (eth_vac_cov_final$change_hep_3 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
eth_vac_cov_final$eii_hep_4 <- (eth_vac_cov_final$change_hep_4 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100

## Save data set 2 -----
saveRDS(eth_vac_cov_final, file = paste0(prepped_data_dir, "03_state_equitable_improv_by_race_ethnicity.RDS"))



  