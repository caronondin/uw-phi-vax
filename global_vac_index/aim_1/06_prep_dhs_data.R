# AUTHOR: Francisco Rios 
# PURPOSE: Prep of DHS data for MOV analyses
# DATE: Last updated October 14 2021

# Load data -----
dt <- as_tibble(readRDS(outputFile05))

# =====
# Define variables necessary for risk analysis: -----
# Identify youth with health card -----
dt$has_health_card_bin <- as.character(dt$has_health_card)

dt <- dt %>%
  mutate(has_health_card_bin = na_if(has_health_card_bin, 9)) %>%
  mutate(has_health_card_bin=recode(has_health_card_bin,
                                    `0`=0,                                 
                                    `1`=1,
                                    `2`=0,
                                    `3`=1,
                                    `4`=0,
                                    `5`=1,
                                    `6`=1,
                                    `7`=1,
                                    `8`=0))

dt$has_health_card_bin <- factor(dt$has_health_card_bin,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))

# Assign the MINIMUM and MAXIMUM age at which each vaccine should be given -----
# Assumption: Vaccine is due at month X. We will accept vaccines given at month X-0.5 through X+1.5.

# Example: Measles due at 9 mos is acceptable 8.5-10.5 months of age.
dt$mea1_age_due_min <- 9*30.4 - 15.2 # 258 days
dt$mea1_age_due_max <- 10*30.4 +  15.2 # 319.2 days

# DPT (or pentavalent) (3 series) (due due at 6, 10, 14 weeks)
# timeliness considered appropriate if between 6th and end of the 7th week of life (with a range of 4 days before and 7 days after)
dt$dpt1_age_due_min <- 6*7 - 4  # 38 days
dt$dpt1_age_due_max <- 7*7 + 7  # 56 days

dt$dpt2_age_due_min <- 10*7 - 4 # 66 days
dt$dpt2_age_due_max <- 11*7 + 7 # 84 days

dt$dpt3_age_due_min <- 14*7 - 4 # 94 days
dt$dpt3_age_due_max <- 15*7 + 7 # 112 days

# Calculate the age of child in days -----
dt$age_in_days <- time_length(difftime(dt$intv_date, dt$dob), "days")

# Calculate the age at which child was vaccinated with measles-containing vaccine -----
dt$age_at_mea1 <- time_length(difftime(dt$mea1, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")

# Calculate the age at which child was vaccinated with DPT vaccines -----
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")

# Find when the last vaccination date took place -----
dt <- dt %>% mutate(oldest_visit = pmax(bcg, dpt1, pol1, dpt2, pol2, dpt3, pent1, pent2, pent3, pneu1, pneu2, pneu3, rota1, rota2, rota3, ipv, hepb1, hepb2, hepb3, hib1, hib2, hib3, yelfev,
                                        na.rm=TRUE))

# Calculate child's age at the oldest visit -----
dt$age_at_oldest_visit = time_length(difftime(dt$oldest_visit, dt$dob), "days")

# Add indicator for the difference in days between appropriate timing and the actual age at vaccination -----
# How to interpret: 
# (1) a value of <0 for mmr_age_minus_min means that they were vaccinated too early -- all of the days lived after the end of the mmr window will be considered days at risk; 
# (2) a value of >0 for mmr_age_minus_min and <0 for mmr_age_minus_max means that they were vaccinated in the proper time window and they will accrue 0 days at risk; 
# (3) a value of >0 for mmr_age_minus_max means that they lived days at risk after appropriate vaccination 

# measles
dt$mea1_age_minus_min <- dt$age_at_mea1 - dt$mea1_age_due_min
dt$mea1_age_minus_max <- dt$age_at_mea1 - dt$mea1_age_due_max

# dpt
dt$dpt1_age_minus_min <- dt$age_at_dpt1 - dt$dpt1_age_due_min
dt$dpt1_age_minus_max <- dt$age_at_dpt1 - dt$dpt1_age_due_max

dt$dpt2_age_minus_min <- dt$age_at_dpt2 - dt$dpt2_age_due_min
dt$dpt2_age_minus_max <- dt$age_at_dpt2 - dt$dpt2_age_due_max

dt$dpt3_age_minus_min <- dt$age_at_dpt3 - dt$dpt3_age_due_min
dt$dpt3_age_minus_max <- dt$age_at_dpt3 - dt$dpt3_age_due_max

# Identify variables related to timing of the measles-containing vaccine ----
dt <- dt %>% 
  mutate(
    # vaccinated too early
    early_mea1 = case_when(mea1_age_minus_min<0 ~ 1,
                           mea1_age_minus_min>0 ~ 0),
    
    # vaccinated within appropriate time frame
    mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 1,
                                     mea1_age_minus_min<0 | mea1_age_minus_max>0 ~ 0),
    
    # vaccinated but too late
    mea1_late = case_when(mea1_age_minus_max>0 ~ 1,
                          mea1_age_minus_max<0 ~ 0),
    
    #  never received vaccine (1 is never received, 0 is did receive))                
    never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
                               is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), 
    
    # transpose the age at which children that did receive measles were counted
    mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 ~ age_at_mea1,
                                        mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ age_at_mea1,
                                        mea1_age_minus_max>0 ~ age_at_mea1),
    
    # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
    mea1_days_at_risk = case_when(never_got_mea1==1 & age_in_days>=mea1_age_due_max  ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min<0 & age_in_days>=mea1_age_due_max ~ age_in_days - mea1_age_due_max,
                                  mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 0,
                                  mea1_age_minus_max>0  ~ mea1_age_minus_max))

# Calculate how many doses of DPT were received -----
dt$tot_num_dpt <- NA
for (i in 1:nrow(dt)){
  dt$tot_num_dpt[i] <- sum(!is.na(dt$dpt1[i]), !is.na(dt$dpt2[i]), !is.na(dt$dpt3[i]))
}

## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ----
## ! REVIEW ! chunk is missing from here to account for DPT dates that are too close to each other!
## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! ## ! 

# Calculate variables related to timing among the DPT-containing vaccine (pentavalent) -----
dt <- dt %>% 
  mutate(
    # calculate variable indicating if never received dpt vaccine
    never_got_dpt = case_when(is.na(age_at_dpt1) & is.na(age_at_dpt2) & is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 1,
                              !is.na(age_at_dpt1) | !is.na(age_at_dpt2) | !is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt1 = case_when(is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt1) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt2 = case_when(is.na(age_at_dpt2) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt2) & has_health_card_bin=="Yes" ~ 0),
    
    never_got_dpt3 = case_when(is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~ 1,
                               !is.na(age_at_dpt3) & has_health_card_bin=="Yes" ~0),
    
    # calculate whether they got the first dose in sixth week
    dpt_dose_6wks = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                              never_got_dpt1==1 ~ 0),
    
    # calculate age when first dose was received
    dpt_dose_6wks_when = case_when(age_at_dpt1>=dpt1_age_due_min & age_at_dpt1<=dpt1_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt1))

# Find out how old chid was at the first eligible dose during 6 weeks -----
dt$first_elig_dpt_after_6wks <- NA
i <- 1
for (i in 1:nrow(dt)){
  d <-sort(c(dt$age_at_dpt1[i], dt$age_at_dpt2[i], dt$age_at_dpt3[i]))
  
  y <- which(d >= dt$dpt1_age_due_min[i])[1]
  dt$first_elig_dpt_after_6wks[i] <- d[y]
}
    
# Only keep newly created values if age_in_days>=dpt3_age_due_max
dt <- dt %>% 
  mutate(first_elig_dpt_after_6wks = if_else(age_in_days>=dpt3_age_due_max, 
                                             first_elig_dpt_after_6wks, NA_real_))

dt <- dt %>% mutate(
    # calculate whether they got the second dose in the tenth week
    dpt_dose_10wks = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                               never_got_dpt2==1 ~ 0),
    
    # calculate age when second dose was received
    dpt_dose_10wks_when = case_when(age_at_dpt2>=dpt2_age_due_min & age_at_dpt2<=dpt2_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt2))

# Find out how old child was at the first eligible dose during 10 weeks -----
dt$first_elig_dpt_after_10wks <- NA
i <- 1
for (i in 1:nrow(dt)){
  d <-sort(c(dt$age_at_dpt1[i], dt$age_at_dpt2[i], dt$age_at_dpt3[i]))
  
  y <- which(d >= dt$dpt2_age_due_min[i] & d!=dt$first_elig_dpt_after_6wks[i])[1]
  dt$first_elig_dpt_after_10wks[i] <- d[y]
}

# Only keep newly created values if age_in_days>=dpt3_age_due_max
dt <- dt %>% 
  mutate(first_elig_dpt_after_10wks = if_else(age_in_days>=dpt3_age_due_max, 
                                             first_elig_dpt_after_10wks, NA_real_))

dt <- dt %>% mutate(    
    # calculate whether they got the third dose in the 14th week
    dpt_dose_14wks = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ 1,
                               never_got_dpt3==1 ~ 0),
    
    # calculate age when third dose was received
    dpt_dose_14wks_when = case_when(age_at_dpt3>=dpt3_age_due_min & age_at_dpt3<=dpt3_age_due_max & age_in_days>= dpt3_age_due_max ~ age_at_dpt3))

# Find out how old child was at the first eligible dose during 14 weeks -----
dt$first_elig_dpt_after_14wks <- NA
i <- 1
for (i in 1:nrow(dt)){
  d <-sort(c(dt$age_at_dpt1[i], dt$age_at_dpt2[i], dt$age_at_dpt3[i]))
  
  y <- which(d >= dt$dpt3_age_due_min[i] & d!=dt$first_elig_dpt_after_6wks[i] & d!=dt$first_elig_dpt_after_10wks[i])[1]
  dt$first_elig_dpt_after_14wks[i] <- d[y]
}

# only keep newly created values if age_in_days>=dpt3_age_due_max
dt <- dt %>% 
  mutate(first_elig_dpt_after_14wks = if_else(age_in_days>=dpt3_age_due_max, 
                                              first_elig_dpt_after_14wks, NA_real_))
    
dt <- dt %>% mutate(    
    # generate indicator for correct interval
    dpt_within_interval = case_when(
      
      # kids that did not have all visits during the interval, but had 3 doses with the right spacing
      tot_num_dpt==3 & first_elig_dpt_after_14wks>=dpt3_age_due_min & first_elig_dpt_after_14wks <=dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~1,
      
      # kids that have perfect adherence
      dpt_dose_6wks==1 & dpt_dose_10wks==1 & dpt_dose_14wks==1 & age_in_days>=dpt3_age_due_max ~ 1,
      
      # kids without perfect adherence 
      # dpt_dose_6wks!=1 & dpt_dose_10wks!=1 & dpt_dose_14wks!=1 ~ 0
      ),
    
    # generate indicator for vaccine 3rd dose too late
    dpt_late = case_when(first_elig_dpt_after_14wks>dpt3_age_due_max & age_in_days>=dpt3_age_due_max ~ 1),
    
    # generate indicator for age at counted vaccine
    dpt_age_at_counted_vac = case_when(
      
      # kids that have perfect adherence
      dpt_within_interval==1 ~ dpt_dose_14wks_when,
      
      # kids that did not have all visits during the interval, but had 3 doses with the right spacing
      dpt_within_interval==1 ~ first_elig_dpt_after_14wks,
      
      # third dose was too late
      dpt_late==1 ~ first_elig_dpt_after_14wks),
    
    # assign indicator for too few dpt vacccines
    too_few_elig_dpt = case_when(is.na(first_elig_dpt_after_6wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1,
                                 is.na(first_elig_dpt_after_10wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1,
                                 is.na(first_elig_dpt_after_14wks) & tot_num_dpt>0 & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1
                                 ),
    
    # assign days at risk
    dpt_days_at_risk = case_when(never_got_dpt==1 & age_in_days>=dpt3_age_due_max ~ age_in_days - dpt3_age_due_max,
                                 dpt_within_interval==1 ~ 0,
                                 dpt_late==1 ~ first_elig_dpt_after_14wks - dpt3_age_due_max,
                                 too_few_elig_dpt==1 ~ age_in_days - dpt3_age_due_max))


# Calculate ages at other vaccines  -----
dt$age_at_bcg <- time_length(difftime(dt$bcg, dt$dob), "days")
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_pol1 <- time_length(difftime(dt$pol1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_pol2 <- time_length(difftime(dt$pol2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")
dt$age_at_pol3 <- time_length(difftime(dt$pol3, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")
dt$age_at_polbirth <- time_length(difftime(dt$polbirth, dt$dob), "days")
dt$age_at_hepbbirth <- time_length(difftime(dt$hepbbirth, dt$dob), "days")
dt$age_at_pent1 <- time_length(difftime(dt$pent1, dt$dob), "days")
dt$age_at_pent2 <- time_length(difftime(dt$pent2, dt$dob), "days")
dt$age_at_pent3 <- time_length(difftime(dt$pent3, dt$dob), "days")
dt$age_at_pneu1 <- time_length(difftime(dt$pneu1, dt$dob), "days")
dt$age_at_pneu2 <- time_length(difftime(dt$pneu2, dt$dob), "days")
dt$age_at_pneu3 <- time_length(difftime(dt$pneu3, dt$dob), "days")
dt$age_at_rota1 <- time_length(difftime(dt$rota1, dt$dob), "days")
dt$age_at_rota2 <- time_length(difftime(dt$rota2, dt$dob), "days")
dt$age_at_rota3 <- time_length(difftime(dt$rota3, dt$dob), "days")
dt$age_at_ipv <- time_length(difftime(dt$ipv, dt$dob), "days")
dt$age_at_hepb1 <- time_length(difftime(dt$hepb1, dt$dob), "days")
dt$age_at_hepb2 <- time_length(difftime(dt$hepb2, dt$dob), "days")
dt$age_at_hepb3 <- time_length(difftime(dt$hepb3, dt$dob), "days")
dt$age_at_hib1 <- time_length(difftime(dt$hib1, dt$dob), "days")
dt$age_at_hib2 <- time_length(difftime(dt$hib2, dt$dob), "days")
dt$age_at_hib3 <- time_length(difftime(dt$hib3, dt$dob), "days")
dt$age_at_yelfev <- time_length(difftime(dt$yelfev, dt$dob), "days")

# Find out how old child was at the earliest possible visit (using all vaccination dates except measles) -----
dt$no_mea1_mop_age <- NA
i <- 1
for (i in 1:nrow(dt)){

b <- sort(c(dt$age_at_bcg[i], dt$age_at_hepbbirth[i], dt$age_at_polbirth[i], 
            dt$age_at_pol1[i], dt$age_at_dpt1[i], dt$age_at_pneu1[i], dt$age_at_rota1[i], dt$age_at_hepb1[i], dt$age_at_hib1[i],
            dt$age_at_pol2[i], dt$age_at_dpt2[i], dt$age_at_pneu2[i], dt$age_at_hepb2[i], dt$age_at_hib2[i],
            dt$age_at_ipv[i], dt$age_at_pol3[i], dt$age_at_dpt3[i], dt$age_at_pneu3[i], dt$age_at_hepb3[i], dt$age_at_hib3[i], 
            dt$age_at_rota2[i], dt$age_at_rota3[i], dt$age_at_yelfev[i], dt$age_at_mea1[i], dt$age_at_mea2[i]))

y <- which(b > dt$mea1_age_due_min[i])[1]
dt$no_mea1_mop_age[i] <- b[y]
} 

# clear no_mea1_mop_age if not never_got_mea1 and if age_in_days is < mea1_age_due_max 
dt <- dt %>% 
  mutate(no_mea1_mop_age = if_else(age_in_days>=mea1_age_due_max & never_got_mea1==1 & !is.na(mea1_days_at_risk),
                                       no_mea1_mop_age, NA_real_))

# Case 2: The Child got Mea1 late, but there was another visit for another vaccine in between the mea1 due age and actual age at Mea1 ----
# find out how old child was at the earliest possible visit (using all vaccination dates except measles)
dt$earliest_visit_btwn_mea1 <- NA
i <- 1
for (i in 1:nrow(dt)){
  
  b <- sort(c(dt$age_at_bcg[i], dt$age_at_hepbbirth[i], dt$age_at_polbirth[i], 
              dt$age_at_pol1[i], dt$age_at_dpt1[i], dt$age_at_pneu1[i], dt$age_at_rota1[i], dt$age_at_hepb1[i], dt$age_at_hib1[i],
              dt$age_at_pol2[i], dt$age_at_dpt2[i], dt$age_at_pneu2[i], dt$age_at_hepb2[i], dt$age_at_hib2[i],
              dt$age_at_ipv[i], dt$age_at_pol3[i], dt$age_at_dpt3[i], dt$age_at_pneu3[i], dt$age_at_hepb3[i], dt$age_at_hib3[i], 
              dt$age_at_rota2[i], dt$age_at_rota3[i], dt$age_at_yelfev[i], dt$age_at_mea1[i], dt$age_at_mea2[i]))
  
  y <- which(b > dt$mea1_age_due_min[i] & b < dt$mea1_age_at_counted_vac[i])[1]
  dt$earliest_visit_btwn_mea1[i] <- b[y]
} 

# only keep newly created values if mea1_late==1 and !is.na(mea1_age_at_counted_vac) and age_in_days>=mea1_age_due_max
dt <- dt %>% 
  mutate(earliest_visit_btwn_mea1 = if_else(age_in_days>=mea1_age_due_max & mea1_late==1 & !is.na(mea1_age_at_counted_vac),
                                                earliest_visit_btwn_mea1, NA_real_))

# Calculate WHO HAS A MISSED OPPORTUNITY & WHAT IS POTENTIAL COVERAGE? -----
dt <- dt %>% mutate(
  
  # variable that indicates missed opportunities
  mea1_missed_opportunity = case_when(
    
    # CASE 1: the child never got measles vaccination but had another vaccination visit during or after measles vaccine was due
    never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
    
    # replace measles missed opportunity with 1 if they had an earlier visit that they could have attended
    !is.na(earliest_visit_btwn_mea1) & age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 1,
    
    # default value for missed opportunities
    age_in_days>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 0
    
  ))

dt <- dt %>% mutate(
  # we want to know how old the child was when they could have gotten Mea1
  mea1_age_at_mop_vac = case_when(
    !is.na(earliest_visit_btwn_mea1) ~ earliest_visit_btwn_mea1,
    is.na(earliest_visit_btwn_mea1) ~ no_mea1_mop_age))

# Create new variables of when children could have had potential dpt doses ----
dt$potential_dpt_6wks <- NA
dt$potential_dpt_10wks <- NA
dt$potential_dpt_14wks <- NA

# for each row i in dataframe create a vector of all the ages at which vaccines were distributed
for (i in 1:nrow(dt)){
  b <- sort(c(dt$age_at_bcg[i], dt$age_at_hepbbirth[i], dt$age_at_polbirth[i], 
            dt$age_at_pol1[i], dt$age_at_dpt1[i], dt$age_at_pneu1[i], dt$age_at_rota1[i], dt$age_at_hepb1[i], dt$age_at_hib1[i],
            dt$age_at_pol2[i], dt$age_at_dpt2[i], dt$age_at_pneu2[i], dt$age_at_hepb2[i], dt$age_at_hib2[i],
            dt$age_at_ipv[i], dt$age_at_pol3[i], dt$age_at_dpt3[i], dt$age_at_pneu3[i], dt$age_at_hepb3[i], dt$age_at_hib3[i], 
            dt$age_at_rota2[i], dt$age_at_rota3[i], dt$age_at_yelfev[i], dt$age_at_mea1[i], dt$age_at_mea2[i]))
  
  x <- which(b > dt$dpt1_age_due_min[i])[1]
  dt$potential_dpt_6wks[i] <- b[x]
  
  y <- which(b > dt$dpt2_age_due_min[i] & b > dt$potential_dpt_6wks[i])[1]
  dt$potential_dpt_10wks[i] <- b[y]
  
  z <- which(b>dt$dpt3_age_due_min[i] & b > dt$potential_dpt_10wks[i])[1]
  dt$potential_dpt_14wks[i] <- b[z]
}

# only keep newly created values if age_in_days>=dpt3_age_due_max and 
dt <- dt %>% 
  mutate(potential_dpt_6wks = if_else(age_in_days>=dpt3_age_due_max,
                                       potential_dpt_6wks, NA_real_),
         potential_dpt_10wks = if_else(age_in_days>=dpt3_age_due_max,
                                       potential_dpt_10wks, NA_real_),
         potential_dpt_14wks = if_else(age_in_days>=dpt3_age_due_max,
                                       potential_dpt_14wks, NA_real_))

# Calculate missed opportunities for DPT vaccines -----
dt <- dt %>% mutate(
  
  dpt_missed_opportunity=case_when(
    
    # Case 3: The child was vaccinated for DPT late and had an earlier dose (can this be combined with Case 2?)
    dpt_late==1 & potential_dpt_14wks < first_elig_dpt_after_14wks & !is.na(first_elig_dpt_after_14wks) & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # Case 1: The child was never vaccinated for DPT, but had at least 3 visits 4 weeks apart during or after the DPT due ages (by dose)  
    never_got_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # Case 2: The child WAS vaccinated for DPT, but did not have 3 doses with correct spacing by age 6 mos 
    too_few_elig_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ 1,
    
    # default value
    age_in_days>=dpt3_age_due_max & !is.na(dpt_days_at_risk) ~ 0
    ),
  
  dpt_age_at_mop_vac = case_when(
    
    # Case 3: The child was vaccinated for DPT late and had an earlier dose (can this be combined with Case 2?)
    dpt_late==1 & potential_dpt_14wks < first_elig_dpt_after_14wks & !is.na(first_elig_dpt_after_14wks) & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>= dpt3_age_due_max ~ potential_dpt_14wks,
    
    # Case 1: The child was never vaccinated for DPT, but had at least 3 visits 4 weeks apart during or after the DPT due ages (by dose)  
    never_got_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ potential_dpt_14wks,
    
    # Case 2: The child WAS vaccinated for DPT, but did not have 3 doses with correct spacing by age 6 mos 
    too_few_elig_dpt==1 & !is.na(potential_dpt_14wks) & has_health_card_bin=="Yes" & age_in_days>=dpt3_age_due_max ~ potential_dpt_14wks))

# COMPUTE DAYS AT RISK IF OPPORTUNITY WAS NOT MISSED -----

# measles 
dt <- dt %>% mutate(
  mea1_mop_age_minus_max = case_when(
    # if child did have MO, then we need to calculate days at risk with our new number 
    !is.na(earliest_visit_btwn_mea1) & mea1_missed_opportunity==1 ~ earliest_visit_btwn_mea1 - mea1_age_due_max))

dt <- dt %>% mutate(
  mea1_days_at_risk_mop = case_when(
    # If the kid didn't have a missed opportunity, then the days at risk stays the same
    mea1_missed_opportunity==0 ~ mea1_days_at_risk,
    
    # if they did add the new days of risk
    mea1_mop_age_minus_max>0 & age_in_days>=mea1_age_due_max & mea1_missed_opportunity==1 ~ mea1_mop_age_minus_max))


# DPT3 

# if kid has missed opportunity, calculate a new total days at risk
dt <- dt %>% mutate(
  dpt_mop_age_minus_max = case_when(
    !is.na(dpt_age_at_mop_vac) & dpt_missed_opportunity==1 ~ dpt_age_at_mop_vac - dpt3_age_due_max))

dt <- dt %>% mutate(
  dpt_days_at_risk_mop = case_when(
    # if they did have a  MO, add new days of risk
    dpt_mop_age_minus_max>0 & age_in_days>=dpt3_age_due_max & dpt_missed_opportunity==1 ~ dpt_mop_age_minus_max,
    
    # if the child didn't have a missed opportunity then the days of risk stays the same
    dpt_missed_opportunity==0 ~ dpt_days_at_risk))

# Calculate dpt missed opportunities for each individual dpt vaccine (1-3)
dt <- dt %>% mutate(
  dpt1_missed_opportunity = case_when(
    # Case 1: the child did not receive the first vaccine but had another vaccine date in that early range
    is.na(age_at_dpt1) & !is.na(potential_dpt_6wks) & age_in_days>=dpt1_age_due_max & has_health_card_bin=="Yes" ~ 1,
    
    # the child did not receive the first dose but had another visit 
    # # default value for missed opportunity
    # is.na(age_at_dpt1) & !is.na(potential_dpt_6wks) & age+
  ),
  
  dpt2_missed_opportunity = case_when(
    # the child did not receive the second vaccine but had another vaccine visit during that range
    is.na(age_at_dpt2) & !is.na(potential_dpt_10wks) & age_in_days>=dpt2_age_due_max & has_health_card_bin=="Yes" ~ 1
    
  ),
  dpt3_missed_opportunity = case_when(
    # the child did not receive the third vaccine does but hat another visit during that range
    is.na(age_at_dpt3) & !is.na(potential_dpt_14wks) & age_in_days>=dpt3_age_due_max & has_health_card_bin=="Yes" ~ 1
  )
)

# Prep variables for plotting -----

# Factor mother's education -----
dt$edu <- as.character(dt$v106)
dt <- dt %>% mutate(edu = recode(edu,
                                     `0`=0,
                                     `1`=1,
                                     `2`= 2,
                                     `3`=2))
dt$edu <- factor(dt$edu,
                   levels = c(0,1,2),
                   labels = c("No education", "Primary", "Secondary or higher"))

# Factor mother's literacy levels ----
dt$literate <- as.character(dt$v155)

dt <- dt %>% replace_with_na(replace = list(literate = 9))

dt <- dt %>% mutate(literate = recode(literate,
                                          `0`=0,
                                          `1`=1,
                                          `2`=1,
                                          `3`=9,
                                          `4`=9))

dt$literate <- factor(dt$literate,
                        levels = c(0,1),
                        labels = c("Iliterate", "Literate"))

# Factor mother's age -----
dt$wom_agecat <- dt$v012

dt <- dt %>%
  mutate(wom_agecat=case_when(
    wom_agecat %in% 15:19 ~ "1",
    wom_agecat %in% 20:34 ~ "2",
    wom_agecat %in% 35:49 ~ "3"))

dt$wom_agecat <- factor(dt$wom_agecat,
                          levels = c(1,2,3),
                          labels = c("15-19", "20-34", "35-49"))

# Factor parity -----
dt$total_children_born <- dt$v201 

dt <- dt %>% 
  mutate(total_children_born=case_when(
    total_children_born %in% 1 ~ "1",
    total_children_born %in% 2:3 ~ "2",
    total_children_born %in% 4:5 ~ "3",
    total_children_born %in% 6:20 ~ "4"
  ))

dt$total_children_born <- factor(dt$total_children_born,
                                   levels = c(1,2,3,4),
                                   labels = c("1 child", "2-3 children", "4-5 children", "6+ children"))

# Factor marital status -----
dt$marital <- as.character(dt$v501)

dt <- dt %>% mutate(marital = recode(marital,
                                         `0`=1,
                                         `1`=2,
                                         `2`=3,
                                         `3`=4,
                                         `4`=4,
                                         `5`=4))

dt$marital<- factor(dt$marital,
                      levels = c(1,2,3,4),
                      labels = c("Single", "Married", "Union", "Divorced, seperated, widowed, or other"))

# Factor mother's occupation -----
dt$wom_occ <- dt$v717

dt <- dt %>% replace_with_na(replace = list(wom_occ = 99))

dt <- dt %>% 
  mutate(wom_occ=case_when(
    wom_occ %in% 0 ~ 1,
    wom_occ %in% 1:97 ~ 2
  ))

dt$wom_occ <- factor(dt$wom_occ,
                       levels = c(1,2),
                       labels = c("Not employed", "Employed"))

# Factor household assets -----
# each survey should have either v190a or v190 for the household assets
dt$assets <- ifelse(!is.na(dt$v190a), dt$v190a, dt$v190)

dt$assets <- factor(dt$assets,
                      levels = c(1,2,3,4,5),
                      labels = c("Quintile 1", "Quintile 2", "Quintile 3","Quintile 4", "Quintile 5"))

# Rename variable for household size -----
dt$hhsize <- dt$v136

# Factor urban/rural household -----
dt$urban <- abs(dt$v025-2)

dt$urban <-factor(dt$urban,
                    levels = c(0,1),
                    labels = c("Rural household", "Urban household"))

# Factor sex of head of household -----
dt$female_head <- dt$v151

dt$female_head <- factor(dt$female_head,
                           levels = c(1,2),
                           labels = c('Male', 'Female'))

# Factor sex of child -----
dt$sex_of_child <- factor(dt$sex_of_child,
                            levels=c(1,2),
                            labels=c("Male", "Female"))

# Factors kid's age -----
dt$kid_agecat <- round(time_length(difftime(dt$intv_date, dt$dob), "years"), digits = 0)
dt$kid_agecat <- factor(dt$kid_agecat, 
                          levels = c(0,1,2,3),
                          labels = c("0 years", "1 year", "2 years", "3 years"))

# Format measles missed opportunity variable -----
dt$mea1_missed_opportunity <-factor(dt$mea1_missed_opportunity,
                                      levels=c(0,1),
                                      labels=c("No", "Yes"))

# Factor year and location of DHS survey -----
dt$strata <- dt$v000

dt$strata <- factor(dt$strata, 
                      levels = c("NG7", "NG6"),
                      labels=c("2018", "2013"))

# Factor DPT missed opportunity-----
dt$dpt_missed_opportunity <-factor(dt$dpt_missed_opportunity,
                                     levels=c(0,1),
                                     labels=c("No", "Yes"))

# Factor states
dt$state <- as.numeric(dt$sstate)

dt <- dt %>% mutate(state=recode(state,
                               `10`='Sokoto',
                               `20`='Zamfara',
                               `30`='Katsina',
                               `40`='Jigawa',
                               `50`='Yobe',
                               `60`='Borno',
                               `70`='Adamawa',
                               `80`='Gombe',
                               `90`='Bauchi',
                               `100`='Kano',
                               `110`='Kaduna',
                               `120`='Kebbi',
                               `130`='Niger',
                               `140`='FCT Abuja',
                               `150`='Nasarawa',
                               `160`='Plateau',
                               `170`='Taraba',
                               `180`='Benue',
                               `190`='Kogi',
                               `200`='Kwara',
                               `210`='Oyo',
                               `220`='Osun',
                               `230`='Ekiti',
                               `240`='Ondo',
                               `250`='Edo',
                               `260`='Anambra',
                               `270`='Enugu',
                               `280`='Ebonyi',
                               `290`='Cross River',
                               `300`='Akwa Ibom',
                               `310`='Abia',
                               `320`='Imo',
                               `330`='Rivers',
                               `340`='Bayelsa',
                               `350`='Delta',
                               `360`='Lagos',
                               `370`='Ogun'))

dt$state <- factor(dt$state)

# Factor Zone
dt$zone <- as.numeric(dt$sstate)

dt <- dt %>% mutate(
  # create zones from subnational state
  zone = case_when(sstate%in%c(130, 140, 150, 160, 180, 190, 200) ~ 1, # North Central
                   sstate%in%c(50, 60, 70, 80, 90, 170) ~ 2, # North East
                   sstate%in%c(40, 110, 100, 30, 120, 10, 20) ~ 3, # North West
                   sstate%in%c(310, 260, 280, 270, 320) ~ 4, # South East
                   sstate%in%c(300, 340, 290, 330, 350, 250) ~ 5, # South South
                   sstate%in%c(230, 360, 370, 240, 220, 210) ~ 6  # South West
                   ))

dt$zone <- factor(dt$zone,
                  levels = c(1,2,3,4,5,6),
                  labels = c("North Central", "North East", "North West", "South East", "South South", "South West"))

# Label newly created variables -----
label(dt$sex_of_child) <- "Child's sex"
label(dt$kid_agecat) <- "Child's age (in years)"
label(dt$edu) <- "Mother's education"
label(dt$literate) <- "Literacy"
label(dt$wom_agecat) <- "Mother's age (in years)"
label(dt$total_children_born) <- "Parity"
label(dt$marital) <- "Marital status"
label(dt$wom_occ) <- "Mother's occupation"
label(dt$hhsize) <- "Household size"
label(dt$female_head) <-"Sex of head of household"
label(dt$urban) <-"Urbanicity"
label(dt$mea1_missed_opportunity) <-"Missed measles opportunity"
label(dt$assets) <-"Household assets"
label(dt$strata) <- "DHS version"
label(dt$state) <- "State"
label(dt$zone) <- "Zone"

# Only keep the variable names that are in the codebook for consistency. -----
# This should constantly be reviewed. 

codebook <- as.data.table(read_xlsx(paste0(codebook_directory, 'dhs_mov_codebook.xlsx')))

dropped_vars = names(dt)[!names(dt)%in%codebook$Variable]
if (length(dropped_vars)!=0){
  print("Some variables are being dropped because they aren't in the codebook - Review to make sure these shouldn't be in the final data.")
  print(dropped_vars)
}
dt <- dt[, names(dt)%in%codebook$Variable, with=FALSE]

# Make visuals to explore the data -----

dataVariables = unique(codebook[Category=="derived variable" & Class=="numeric" & `Possible Values`=="many possible values"]$Variable)

histograms = lapply(dataVariables, function(v){
ggplot(dt, aes_string(x=v)) + 
  geom_histogram(bins=30) +
    facet_wrap(~strata, scales = "free")
})


# save histograms as a PDF -----
outputImage09 <- paste0(visDir, "aim_1/missed_opportunities/01_prepped_dhs_data_histograms.pdf") 

print(paste('Saving:', outputImage09)) 
pdf(outputImage09, height=5.5, width=9)
for(i in seq(length(histograms))) { 
  print(histograms[[i]])
}
dev.off()

# view summary statistics for all variables
# pdf(file = paste0(visDir, "aim_1/missed_opportunities/summary_statistics_prepped_dhs_data.pdf"))
st(dt, file=paste0(visDir, "aim_1/missed_opportunities/02_summary_stats_prepped_dhs_data"))

# Save output -----
saveRDS(dt, outputFile06)

# Print final statement -----
print("Step 09: Prepping of DHS data for analyses complete.")
