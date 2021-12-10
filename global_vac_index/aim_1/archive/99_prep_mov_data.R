# # Author: Francisco Rios 
# # Purpose: prep missed opportunities analyses using DHS data
# # Date: Last modified August 13, 2021

#####
# to-do: 
# recreate each of the analyses for each of the two time points in Liberia

# - exponentiate the logistic regression to get an OR


####################################################
# read in dataset
####################################################
dt <- readRDS(outputFile2d)


# Additional calculations: 
#   1. Days at risk per vaccine per child
#   2. Yes/no there was a missed opportunity per vaccine per child 
#   3. Potential days at risk per vaccine per child (based on missed opportunity status) 
# 
# Assumptions (others are tagged with "Assumption:" throughout the code): 
#   1. Children without health cards are NOT immunized. (We are not considering recall information)
# 
# Definitions: 
#   1. Days at risk = number of days between when the child was due for the vaccine and when they actually received it

#####  Assign the MINIMUM and MAXIMUM ages each vaccine should be given 
#  Assumption: Vaccine is due at month X. We will accept vaccines given at month X-0.5 through X+1.5. 
# Example: Measles due at 9 mos is acceptable 8.5-10.5 months of age.
dt$mea1_age_due_min = 9*30.4 - 15.2 # 258 days
dt$mea1_age_due_max = 10*30.4 +  15.2 # 319.2 days

##### do Rotavirus (3 series) as well as DPT

# calculate the age of child in days
dt$kid_age <- time_length(difftime(dt$intv_date, dt$dob), "days")

# calculate the age at which child was vaccinated with measles-containing vaccine and all other vaccines
dt$age_at_mea1 <- time_length(difftime(dt$mea1, dt$dob), "days")
dt$age_at_bcg <- time_length(difftime(dt$bcg, dt$dob), "days")
dt$age_at_dpt1 <- time_length(difftime(dt$dpt1, dt$dob), "days")
dt$age_at_pol1 <- time_length(difftime(dt$pol1, dt$dob), "days")
dt$age_at_dpt2 <- time_length(difftime(dt$dpt2, dt$dob), "days")
dt$age_at_pol2 <- time_length(difftime(dt$pol2, dt$dob), "days")
dt$age_at_dpt3 <- time_length(difftime(dt$dpt3, dt$dob), "days")

dt$age_at_pol3 <- time_length(difftime(dt$pol3, dt$dob), "days")
dt$age_at_mea2 <- time_length(difftime(dt$mea2, dt$dob), "days")
dt$age_at_pol0 <- time_length(difftime(dt$pol0, dt$dob), "days")
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
dt$age_at_poln <- time_length(difftime(dt$poln, dt$dob), "days")

dt$age_at_hepb1 <- time_length(difftime(dt$hepb1, dt$dob), "days")
dt$age_at_hepb2 <- time_length(difftime(dt$hepb2, dt$dob), "days")
dt$age_at_hepb3 <- time_length(difftime(dt$hepb3, dt$dob), "days")
dt$age_at_hib1 <- time_length(difftime(dt$hib1, dt$dob), "days")
dt$age_at_hib2 <- time_length(difftime(dt$hib2, dt$dob), "days")
dt$age_at_hib3 <- time_length(difftime(dt$hib3, dt$dob), "days")

# variable indicating when the oldest vaccination date took place
dt <- dt %>% mutate(oldest_visit = pmax(bcg, dpt1, pol1, dpt2, pol2, dpt3, pent1, pent2, pent3, pneu1, pneu2, pneu3, rota1, rota2, rota3, poln, hepb1, hepb2, hepb3, hib1, hib2, hib3,
                    na.rm=TRUE))

# calculate the age at the oldest visit
dt$age_at_oldest_visit = time_length(difftime(dt$oldest_visit, dt$dob), "days")

# Add indicator for the difference in days between appropriate timing and the actual age at vaccination
# How to interpret: 
# (1) a value of <0 for mmr_age_minus_min means that they were vaccinated too early -- all of the days lived after the end of the mmr window will be considered days at risk; 
# (2) a value of >0 for mmr_age_minus_min and <0 for mmr_age_minus_max means that they were vaccinated in the proper time window and they will accrue 0 days at risk; 
# (3) a value of >0 for mmr_age_minus_max means that they lived days at risk after appropriate vaccination 

dt$mea1_age_minus_min <- dt$age_at_mea1 - dt$mea1_age_due_min
dt$mea1_age_minus_max <- dt$age_at_mea1 - dt$mea1_age_due_max

# Identify whether children were (1) vaccinated too early,
# (2) vaccinated within the appropriate interval
# (3) vaccinated too late, or
# (4) never vaccinated

dt <- dt %>% 
  mutate(
    # vaccinated too early
    early_mea1 = case_when(mea1_age_minus_min<0 ~ 1, 
                                           TRUE ~ 0),
    
    # vaccinated within appropriate time frame
    mea1_within_interval = case_when(mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 1, 
                                                     TRUE ~ 0),
    
    # vaccinated but too late
    mea1_late = case_when(mea1_age_minus_max>0 ~ 1, 
                                          TRUE ~ 0),
    
    #  never received vaccine (1 is never received, 0 is did receive))                
    never_got_mea1 = case_when(!is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 0,
                                               is.na(age_at_mea1) & has_health_card_bin=="Yes" ~ 1), 
    
    # transpose the age at which children that did receive measles were counted
    mea1_age_at_counted_vac = case_when(mea1_age_minus_min<0 ~ age_at_mea1,
                                        mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ age_at_mea1,
                                        mea1_age_minus_max>0 ~ age_at_mea1),
    
    # variable that indicates how much time each child was at risk (for those that were vaccinated early or late)
    mea1_days_at_risk = case_when(mea1_age_minus_min<0 & kid_age>=mea1_age_due_max ~ kid_age - mea1_age_due_max,
                                  mea1_age_minus_min>=0 & mea1_age_minus_max<=0 ~ 0,
                                  mea1_age_minus_max>0  ~ mea1_age_minus_max,
                                  never_got_mea1==1 & kid_age>=mea1_age_due_max  ~ kid_age - mea1_age_due_max),
    
    # variable that indicates missed opportunities
    mea1_missed_opportunity = case_when(never_got_mea1==1 & age_at_oldest_visit>mea1_age_due_min & kid_age>=mea1_age_due_max & !is.na(mea1_days_at_risk) & !is.na(age_at_oldest_visit) ~ 1,
                                        kid_age>=mea1_age_due_max & !is.na(mea1_days_at_risk) ~ 0)
)

# calculate age at each vaccine

# make vector of vaccine_dates
# dateVars = names(dt)[grepl('age_at', names(dt))][4:29]
   

# dt <- dt %>% 
#   mutate_at(
#     vars(one_of(dateVars)),
#     list(case_when(
#       .>=mea1_age_due_min ~ .
#     )))


  #   ))
  # # We also want to know how old the child was at the earliest eligible visit 
  # no_mea1_mop_age = case_when(never_got_mea1==0 ~ NA,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_dpt1>=mea1_age_due_min ~ age_at_dpt1,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,
  #                             never_got_mea1==1 & age_at_bcg>=mea1_age_due_min ~ age_at_bcg,)
  #                   
  #                   # The child got measles late, but there was a visit for another vaccine in between the measles due age and actual age at measles 
  #                   
  #                   # Now compute a compliance with MO
  #                   mea1_compliant_card_mop = case_when(mea1_date_recorded%in%c(0) ~0,
  #                                                       mea1_date_recorded%in%c(1,3) ~1,
  #                                                       mea1_missed_opportunity==1 ~1),
  #                   
  #                   # compute days of risk if opportunity was not missed
  #                   mea1_days_at_risk_mop = case_when(mea1_missed_opportunity==0 ~ mea1_days_at_risk,
  #                                                     )
  #                   )
                    

# // 7a. MMR
# {
#   gen mmr_days_at_risk_mop = .
#   // If the kid didn't have a missed opportunity, then the days at risk stays the same
# 	replace mmr_days_at_risk_mop = mmr_days_at_risk if mmr_missed_opportunity==0
# 	
# // But, if they did have MO, then we need to calculate days at risk with our new number
# 	gen mmr_mop_age_minus_max = earliest_visit_btwn_mmr - `mmr_age_due_max' if earliest_visit_btwn_mmr!=. & mmr_missed_opportunity==1
#   replace mmr_days_at_risk_mop = mmr_mop_age_minus_max if mmr_mop_age_minus_max>0 & age_in_days>=`mmr_age_due_max' & age_in_days!=. & mmr_missed_opportunity==1
# 	
# 	hist mmr_days_at_risk_mop 
# 	* graph box mmr_days_at_risk_mop if mmr_days_at_risk_mop>=0, over(AGE_MO)

##### calculate the missed opportunities and what potential coverage could be

# Case 1: The child never got vaccinated for Measles, but had other vaccination visits during or after the measles due age
# dt <- dt %>% mutate(mea1_missed_oppourtinity)
# replace mmr_missed_opportunity = 1 if never_got_mmr==1 & oldest_visit>`mmr_age_due_min' & age_in_days>=`mmr_age_due_max' & age_in_days!=. & mmr_days_at_risk!=. & oldest_visit!=.

# save output
saveRDS(dt, file=outputFile6a)

# print final statement
print("Step 6a: Prepping DHS data now complete.")

