
##############Step 8#######################
# Author: Francisco Rios Casas and Caroline Nondin
# Purpose: create table of coverage cascade for vaccinations for Liberia Dataset
# Date: Last modified November 15th 2022

#source("C:/Users/cnondin/Documents/uw-phi-vax/global_vac_index/aim_1/01_set_up_R.R")
#CAROLINE: add source of 01_set_up_Lib

# Load data
data <- readRDS(outputFile06)

# Subset data according to age and availability of vaccination document
data1 <- data %>% filter(age_in_days>=mea1_age_due_max,
                         has_health_card_bin=="Yes") # measles

data2 <- data %>% filter(age_in_days>=dpt3_age_due_max,
                         has_health_card_bin=="Yes") # all DPT doses

data3 <- data %>% filter(age_in_days>=dpt1_age_due_max,
                         has_health_card_bin=="Yes") # first dpt dose

data4 <- data %>% filter(age_in_days>=dpt2_age_due_max,
                         has_health_card_bin=="Yes") # second dpt dose

data5 <- data %>% filter(age_in_days>=dpt3_age_due_max,
                         has_health_card_bin=="Yes") # third dpt dose

# Create indicator for those that never got dpt or kids who got dpt early and not again in the interval, or late
data2 <- data2 %>% mutate(gotit = case_when(
  never_got_dpt==1 | too_few_elig_dpt==1 ~ 0,
  dpt_late==1 | dpt_within_interval==1 ~ 1))

# save as data table to making creating new variables simple
data1 <- as.data.table(data1)
data2 <- as.data.table(data2)
data3 <- as.data.table(data3)
data4 <- as.data.table(data4)
data5 <- as.data.table(data5)

# Part 1: Create vaccination coverage cascade for all vaccines

# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = strata]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = strata]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=strata]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=strata]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]

mea1_dt

# statistical test 
mea1_test <- mea1_dt %>% select(strata, no_vaccine, mop)
mea1_test[,no_mop:=no_vaccine-mop]
mea1_test

chisq.test(mea1_test %>% select(mop, no_mop), correct = FALSE)

# statistical test of change in vaccine coverage
mea1_cov_test <- mea1_dt %>% select(strata, no_vaccine, received_vaccine)
mea1_cov_test
chisq.test(mea1_cov_test %>% select(no_vaccine, received_vaccine), correct = FALSE)

# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = strata]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=strata]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=strata]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test
dpt_all_test <- dpt_all_dt %>% select(strata, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test
chisq.test(dpt_all_test %>% select(mop, no_mop), correct = FALSE)

# statistical test of change in vaccine coverage
dpt_cov_test <- dpt_all_dt %>% select(strata, no_vaccine, received_vaccine)
dpt_cov_test
chisq.test(dpt_cov_test %>% select(no_vaccine, received_vaccine), correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = strata]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=strata]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test
dpt1_test <- dpt1_dt %>% select(strata, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
chisq.test(dpt1_test %>% select(mop, no_mop), correct = FALSE)

# DPT 2 -----

# calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = strata]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=strata]

# merge dataset
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# statistical test
dpt2_test <- dpt2_dt %>% select(strata, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
dpt2_test
chisq.test(dpt2_test %>% select(mop, no_mop), correct = FALSE)

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = strata]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=strata]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=strata]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=strata]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test
dpt3_test <- dpt3_dt %>% select(strata, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test %>% select(mop, no_mop), correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))
write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/Liberia/mop_vaccine_table_Lib.csv"))



# Part 2: Stratify coverage cascade by mother's education -----

# Measles -----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, edu)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, edu)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, edu)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, edu)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt

# statistical test within each level of education
mea1_test <- mea1_dt %>% select(strata, edu, no_vaccine, mop)
mea1_test[,no_mop:=no_vaccine-mop]
mea1_test

chisq.test(mea1_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT All -----
# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, edu)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, edu)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test within each level of education
dpt_all_test <- mea1_dt %>% select(strata, edu, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test

chisq.test(dpt_all_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, edu)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, edu)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test within each level of education
dpt1_test <- dpt1_dt %>% select(strata, edu, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
dpt1_test

chisq.test(dpt1_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, edu)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, edu)]

# merge dataset
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# statistical test within each level of education
dpt2_test <- dpt1_dt %>% select(strata, edu, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
dpt2_test

chisq.test(dpt2_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# DPT 3 ----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, edu)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, edu)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, edu)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, edu)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test within each level of education
dpt3_test <- dpt3_dt %>% select(strata, edu, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test[edu=="No education",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[edu=="Primary",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[edu=="Secondary or higher",.(mop, no_mop)], correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "edu", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/Liberia/mop_vaccine_table_education_Lib.csv"))




# Part 3: Stratify coverage cascade by household assets -----
# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, assets)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, assets)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, assets)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, assets)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt

# statistical test within each level of household assets
mea1_test <- mea1_dt %>% select(strata, assets, no_vaccine, mop)
mea1_test[,no_mop:=no_vaccine-mop]
mea1_test

chisq.test(mea1_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# statistical test within each year for association with assets and mop
chisq.test(mea1_test[strata=="2013",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[strata=="2019",.(mop, no_mop)], correct = FALSE)
#CAROLINE: changed dates 
# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, assets)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, assets)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test within each level of household assets
dpt_all_test <- dpt_all_dt %>% select(strata, assets, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test

chisq.test(dpt_all_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, assets)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, assets)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test within each level of household assets
dpt1_test <- dpt1_dt %>% select(strata, assets, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
dpt1_test
chisq.test(dpt1_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, assets)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, assets)]

# merge data set
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]

# statistical test within each level of household assets
dpt2_test <- dpt2_dt %>% select(strata, assets, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
chisq.test(dpt2_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, assets)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, assets)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, assets)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, assets)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test within each level of household assets
dpt3_test <- dpt3_dt %>% select(strata, assets, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test[assets=="Quintile 1",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 2",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 3",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 4",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[assets=="Quintile 5",.(mop, no_mop)], correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "assets", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/Liberia/mop_vaccine_table_assets_Lib.csv"))


# Part 4: Stratify coverage cascade by sub-national states -----
# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, state)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, state)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, state)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, state)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt


# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, state)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, state)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, state)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, state)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, state)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, state)]

# merge data set
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, state)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, state)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, state)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, state)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

setcolorder(all_vax_data, 
            c("strata", "state", "total_with_card", "vaccine", "received_vaccine", "no_vaccine", 
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/Liberia/mop_vaccine_table_states_Lib.csv"))


# Part 5: Stratify coverage cascade by sub-national zones -----
# Measles ----
dt1 <- data1[,.(total_with_card= .N), by = .(strata, zone)]

# # calculate how many children received mea1 according to health card only
dt2 <- data1[mea1_within_interval==1 | mea1_late==1,.(received_vaccine= .N), by = .(strata, zone)]

# calculate how many children did not receive the measles vaccine
dt3 <- data1[never_got_mea1==1 | early_mea1==1, .(no_vaccine=.N), by=.(strata, zone)]

# calculate how many children that were not vaccinated had a missed opportunity
dt4 <- data1[(never_got_mea1==1 | early_mea1==1) & mea1_missed_opportunity=="Yes", .(mop=.N), by=.(strata, zone)]

# merge datatables together
mea1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
mea1_dt[,vaccine:="mea1"]
mea1_dt

# statistical test within each zone
mea1_test <- mea1_dt %>% select(strata, zone, no_vaccine, mop)
mea1_test

mea1_test[,no_mop:=no_vaccine-mop]
chisq.test(mea1_test[zone=="North Western",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="South Central",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="South Eastern A",.(mop, no_mop)], correct = FALSE)
chisq.test(mea1_test[zone=="South Eastern B",.(mop, no_mop)], correct = FALSE)

# DPT All -----

# calculate how many children has a vaccination card
dt1 <- data2[,.(total_with_card= .N), by = .(strata, zone)]

# calculate how many children received all dpt vaccines
dt2 <- data2[gotit==1,. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive all dpt doses
dt3 <- data2[gotit==0,. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive all dpt doses have a dpt missed opportunity
dt4 <- data2[gotit==0 & dpt_missed_opportunity=="Yes",. (mop=.N), by=.(strata, zone)]

# merge dataset
dpt_all_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt_all_dt[,vaccine:="dpt_all"]
dpt_all_dt

# statistical test within each zone
dpt_all_test <- dpt_all_dt %>% select(strata, zone, no_vaccine, mop)
dpt_all_test[,no_mop:=no_vaccine-mop]
dpt_all_test
chisq.test(dpt_all_test[zone=="North Western",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="South Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="South Eastern A",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt_all_test[zone=="South Eastern B",.(mop, no_mop)], correct = FALSE)

# DPT 1 -----

# calculate how many children have a vaccination card
dt1 <- data3[,.(total_with_card= .N), by = .(strata, zone)]

# calculate how many children received the dpt1 dose
dt2 <- data3[!is.na(age_at_dpt1),. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive dpt1 dose
dt3 <- data3[is.na(age_at_dpt1),. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive dpt1 dose have a dpt1 missed opportunity
dt4 <- data3[is.na(age_at_dpt1) & dpt1_missed_opportunity==1,. (mop=.N), by=.(strata, zone)]

# merge data sets together
dpt1_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt1_dt[,vaccine:="dpt1"]
dpt1_dt

# statistical test within each zone
dpt1_test <- dpt1_dt %>% select(strata, zone, no_vaccine, mop)
dpt1_test[,no_mop:=no_vaccine-mop]
dpt1_test

chisq.test(dpt1_test[zone=="North Western",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="South Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="South Eastern A",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt1_test[zone=="South Eastern B",.(mop, no_mop)], correct = FALSE)

# DPT 2 -----

# # calculate how many children have a vaccination card
dt1 <- data4[,.(total_with_card= .N), by = .(strata, zone)]

# # calculate how many children received dpt2
dt2 <- data4[!is.na(age_at_dpt2),. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive dpt2 dose
dt3 <- data4[is.na(age_at_dpt2),. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive dpt2 dose have a dpt2 missed opportunity
dt4 <- data4[is.na(age_at_dpt2) & dpt2_missed_opportunity==1,. (mop=.N), by=.(strata, zone)]

# merge data set
dpt2_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt2_dt[,vaccine:="dpt2"]
dpt2_dt

# statistical test within each zone
dpt2_test <- dpt2_dt %>% select(strata, zone, no_vaccine, mop)
dpt2_test[,no_mop:=no_vaccine-mop]
dpt2_test

chisq.test(dpt2_test[zone=="North Western",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="South Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="South Eastern A",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt2_test[zone=="South Eastern B",.(mop, no_mop)], correct = FALSE)

# DPT 3 -----

# # calculate how many children have a vaccination card
dt1 <- data5[,.(total_with_card= .N), by = .(strata, zone)]

# # calculate how many children received dpt3
dt2 <- data5[!is.na(age_at_dpt3),. (received_vaccine=.N), by=.(strata, zone)]

# calculate how many children did not receive dpt3 dose
dt3 <- data5[is.na(age_at_dpt3),. (no_vaccine=.N), by=.(strata, zone)]

# calculate how many of the children that did not receive dpt3 dose have a dpt3 missed opportunity
dt4 <- data5[is.na(age_at_dpt3) & dpt3_missed_opportunity==1,. (mop=.N), by=.(strata, zone)]

# merge dataset
dpt3_dt <- Reduce(merge,list(dt1,dt2,dt3,dt4))

# add vaccine indicator
dpt3_dt[,vaccine:="dpt3"]
dpt3_dt

# statistical test within each zone
dpt3_test <- dpt3_dt %>% select(strata, zone, no_vaccine, mop)
dpt3_test[,no_mop:=no_vaccine-mop]
dpt3_test
chisq.test(dpt3_test[zone=="North Western",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="North Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="South Central",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="South Eastern A",.(mop, no_mop)], correct = FALSE)
chisq.test(dpt3_test[zone=="South Eastern B",.(mop, no_mop)], correct = FALSE)

# merge all vaccine data together
all_vax_data <- rbind(mea1_dt, dpt_all_dt, dpt1_dt, dpt2_dt, dpt3_dt)

# calculate vaccination coverage
all_vax_data[,vac_coverage:=round((received_vaccine/total_with_card)*100, 1)]

# calculate percent of children with a missed opportunity
all_vax_data[,percent_with_mop:=round((mop/no_vaccine)*100, 1)]

# # calculate coverage if no missed opportunity
all_vax_data[,potential_coverage_with_no_mop:=round((mop+received_vaccine)/total_with_card*100, 1)]

# Save the results -----
setcolorder(all_vax_data,
            c("strata", "zone", "total_with_card", "vaccine", "received_vaccine", "no_vaccine",
              "vac_coverage", "mop", "percent_with_mop", "potential_coverage_with_no_mop"))

write.csv(all_vax_data, file=paste0(resDir, "aim_1/missed_opportunities/Liberia/mop_vaccine_table_zones_Lib.csv"))

# check data for correlation between three variables: education, assets, zones -----
x <- table(data$edu, data$assets)
x
chi2 <- chisq.test(x, correct = FALSE)
chi2
sqrt(chi2$statistic / sum(x))

y <- table(data$assets, data$zone)
y
chi2 <- chisq.test(y, data$zone)
chi2
sqrt(chi2$statistic / sum(y))

z <- table(data$edu, data$zone)
z
chi2 <- chisq.test(z, correct = FALSE)
chi2
sqrt(chi2$statistic / sum(z))
