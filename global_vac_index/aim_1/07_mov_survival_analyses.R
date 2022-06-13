# AUTHOR: Francisco Rios Casas
# PURPOSE: Creates survival curves and calculates hazard ratios for mov immunizations
# DATE: latest version October 13, 2021

# Part I -----
# Load prepped dataset for analyses

data <- readRDS(outputFile06)

# subset data to only Nigeria
data <- data %>% filter(v000 %in% c("NG6", "NG7"))

############## Part II: Explore what variables are associated with a missed opportunity ######

# this function will automatically add pvalue from chisquare test or t-test to a table
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# make two datasets one for each year in Nigeria
data1 <- data %>% filter(v000=="NG7") # 2018
data2 <- data %>% filter(v000=="NG6") # 2013

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")
table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | mea1_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# calculate the chi-square for child's age seperately since children that are too young cannot have a missed opportunity yet
tes1 <- chisq.test(table(data1$kid_agecat, data1$mea1_missed_opportunity, exclude = "0 years"))$p.value
format.pval(tes1, digits=3, eps=0.001)

tes2 <- chisq.test(table(data2$kid_agecat, data2$mea1_missed_opportunity, exclude = c("0 years")))$p.value
format.pval(tes2, digits=3, eps=0.001)

##### repeat descriptive analysis with DPT vaccine #####

test <- table1( ~ sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data1, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

table1(~  sex_of_child + kid_agecat + edu + literate + wom_agecat + total_children_born + marital + assets + hhsize + urban + female_head | dpt_missed_opportunity, data=data2, overall=F, extra.col=list(`P-value`=pvalue), topclass="Rtable1-zebra")

# png(filename = paste0(visDir, "aim_1/missed_opportunities/nigeria_2018_dpt_mov_table1.png") )
# test
# dev.off()
# write.table(test, file = paste0(visDir, "aim_1/missed_opportunities/nigeria_2018_dpt_mov_table1.txt"), sep = ",", quote = FALSE, row.names = F)
###### Part III: Compare days to vaccination (observed and potential coverage) #####
# Comparing DHS data from 2013 and 2018

##### 1. Measles #####

######### Observed ########

# keep kids that are older than the max age of measles and with vaccination card
obsmea1dat <- data %>% filter(age_in_days>=mea1_age_due_max,
                            has_health_card_bin=="Yes")

# calculate hazard days
obsmea1dat <- obsmea1dat %>% mutate(hazard_days_mea1 = case_when(
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),

  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),

  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,

  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# Now we need a failure indicator -- consider failure to be getting a vaccine.
# Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days.
# Kids who got the vaccine in the interal or late will have known observation time.
obsmea1dat <- obsmea1dat %>% mutate(gotit = case_when(

  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0,

  # known observation time
  mea1_late==1 | mea1_within_interval ~ 1,
))

######### Survival curve #########

# create survival object using observed data
observed.mea1 <- Surv(time=obsmea1dat$hazard_days_mea1, event=obsmea1dat$gotit)

# use survfit function to create survival curves based on a formula
f0 <- survfit(observed.mea1 ~ strata, data = obsmea1dat)

# Plot Survival Curves
plot1 <- ggsurvplot(
  fit=f0,
  data=obsmea1dat,
  xlab="Days of observation",
  ylab="",
  title="Observed number of days to measles vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw(), 
  risk.table = FALSE)

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/measles_observed_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot1
dev.off()

# find the median survival time
f0

# statistical test to see if there was a difference in survival time according to year
sd <- survdiff(observed.mea1 ~ strata, data = obsmea1dat)
sd

##### Potential coverage #####

# keep kids older that the max age of measles1 and keep if kids have a vaccine card
potmea1dat <- data %>% filter(age_in_days>=mea1_age_due_max,
                        has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
potmea1dat <- potmea1dat %>% mutate(hazard_days_mea1 = case_when(
  
  # Replace time at risk for MOP
  mea1_missed_opportunity=="Yes" & !is.na(mea1_age_at_mop_vac) ~ mea1_age_at_mop_vac - mea1_age_due_min,
  
  # For the early group, this is just the total number of days since the start of the MMR interval that the child lived
  early_mea1==1 ~ mea1_days_at_risk + (mea1_age_due_max-mea1_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  mea1_late==1 ~ mea1_days_at_risk + (mea1_age_due_max - mea1_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  mea1_within_interval==1 ~ mea1_age_at_counted_vac - mea1_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_mea1==1 ~ age_in_days - mea1_age_due_min
))

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interal or late will have known observation time.

potmea1dat <- potmea1dat %>% mutate(gotit = case_when(
  # known observation time
  mea1_late==1 | mea1_within_interval==1 | mea1_missed_opportunity=="Yes" ~ 1,
  
  # censored
  never_got_mea1==1 | early_mea1==1 ~ 0))

# use survfit function to create survival curves based on a formula
f1 <- survfit(Surv(time = hazard_days_mea1, event=gotit) ~ strata, data = potmea1dat)

plot2 <- ggsurvplot(
  fit=f1,
  data=potmea1dat,
  xlab="Days of observation",
  ylab="",
  title="Potential number of days to measles vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw())

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/measles_potential_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot2
dev.off()

########## 2. DPT ########## 

######### Observed ##########

obsdptdat <- data %>% filter(age_in_days>=dpt3_age_due_max, 
                       has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis
obsdptdat <- obsdptdat %>% mutate(hazard_days_dpt = case_when(
  # For the group with insufficient # of dpt, this is just the total number of days since the start of the interval
  too_few_elig_dpt==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  dpt_late==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  dpt_within_interval==1 ~ dpt_age_at_counted_vac - dpt3_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_dpt==1 ~ age_in_days - dpt3_age_due_min
))

# create a failure indicator
obsdptdat <- obsdptdat %>% mutate(gotit = case_when(
  # for those that never got dpt or kids who got dpt early and not again in the interval or late
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0,
  dpt_late==1 | dpt_within_interval==1 ~ 1
))

# test <- obsdptdat %>% filter(is.na(gotit))

####################################################
# Survival curve

# create survival object using observed data
observed.dpt <- Surv(time=obsdptdat$hazard_days_dpt, event=obsdptdat$gotit)

# use survfit function to create survival curves based on a formula
f2 <- survfit(observed.dpt ~ strata, data = obsdptdat)

# Plot Survival Curves
plot3 <- ggsurvplot(
  fit=f2,
  data = obsdptdat,
  xlab="Days of observation",
  ylab="",
  title="Observed number of days to DPT vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
)

# save plot
png(filename = paste0(visDir, "aim_1/missed_opportunities/dpt_observed_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot3
dev.off()

# find the median survival time
f2

# statistical test to see if there was a difference in survival time according to year
sd2 <- survdiff(observed.dpt ~ strata, data = obsdptdat)
sd2

####################################################
#### Potential coverage
####################################################

# keep kids older that the max age of dpt3 and keep if kids have a vaccine card
potdptdat <- data %>% filter(age_in_days>=dpt3_age_due_max,
                        has_health_card_bin=="Yes")

# Calculate the days at risk for the hazard analysis  -- the first part is the same as non_MOP analysis
potdptdat <- potdptdat %>% mutate(hazard_days_dpt = case_when(
  # for those with a missed opportunity, replace days at risk
  dpt_missed_opportunity=="Yes" & !is.na(dpt_age_at_mop_vac) ~ dpt_age_at_mop_vac - dpt3_age_due_min,
  
  # For the group with insufficient # of dpt, this is just the total number of days since the start of the interval
  too_few_elig_dpt==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For the late group - this is the number of days lived from start of the interval to the date of vaccination. Use the already-calculated days at risk to help.
  dpt_late==1 ~ dpt_days_at_risk + (dpt3_age_due_max - dpt3_age_due_min),
  
  # For those who got vaccinated on time, the number of days to vaccination is just the age at vaccination minus the age at start of the interval
  dpt_within_interval==1 ~ dpt_age_at_counted_vac - dpt3_age_due_min,
  
  # For those who never got a vaccine it is their age minus minimum interval bound,
  never_got_dpt==1 ~ age_in_days - dpt3_age_due_min
))

# # Now we need a failure indicator -- consider failure to be getting a vaccine. 
# # Kids who NEVER got measles or kids who got Measles early (and not again in the interval or late) will be censored at our observation days. 
# # Kids who got the vaccine in the interval or late will have known observation time.

potdptdat <- potdptdat %>% mutate(gotit = case_when(
  # known observation time
  dpt_late==1 | dpt_within_interval==1 | dpt_missed_opportunity=="Yes" ~ 1,
  
  # censored
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0))

# create survival object using observed data
potential.dpt <- Surv(time=potdptdat$hazard_days_dpt, event=potdptdat$gotit)

# use survfit function to create survival curves based on a formula
f3 <- survfit(potential.dpt ~ strata, data = potdptdat)

plot4 <- ggsurvplot(
  fit=f3,
  data=potdptdat,
  xlab="Days of observation",
  ylab="",
  title="Potential number of days to dpt vaccination",
  fun="event",
  conf.int = FALSE,
  censor=FALSE,
  legend = "bottom", 
  legend.title = "Year of Nigeria DHS",
  ylim = c(0,1),
  ggtheme=theme_linedraw()
)

# save plots
png(filename = paste0(visDir, "aim_1/missed_opportunities/dpt_potential_curve.png"),
    height = 8, width = 10, units = "in", res = 300)
plot4
dev.off()

# find the median survival time
f3

# statistical test to see if there was a difference in survival time according to year
sd3 <- survdiff(potential.dpt ~ strata, data = potdptdat)
sd3

###### Create results table
dt <- as.data.table(data)
dt[, .(.N), by = .(v000)]


