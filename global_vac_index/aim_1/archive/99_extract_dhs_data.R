# # Author: Francisco Rios 
# # Purpose: prep dhs data for 'missed opportunities' analyses
# # Date: Last modified August 12, 2021

# read in raw dataset
dhs_data <- as.data.table(read_dta(file=paste0(data_dir,"raw_data/vaccination_trends/dhs/LBIR7ADT/LBIR7AFL.DTA")))

# to-do #####

###############################################################
# subset columns into id variables and variables that need tidying
###############################################################
idVars = c("caseid", "v000", "v005", "v007", "v006", "v016") # id variables for merging
demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v501", "v136", "v190", "v191", "v190a", "v191a", "v025") # demographic variables

dobyearVars = names(dhs_data)[grepl('b2_', names(dhs_data))] # child's year of birth
dobmonthVars = names(dhs_data)[grepl('b1_', names(dhs_data))] # child's month of birth
dobdayVars = names(dhs_data)[grepl('b17_', names(dhs_data))] # child's day of birth

childsexVars = names(dhs_data)[grepl('b4', names(dhs_data))] # child's sex
childlivingVars = names(dhs_data)[grepl('b5', names(dhs_data))] # child dead or living status
childresidVars = names(dhs_data)[grepl('b9_', names(dhs_data))] # where the child is residing

vaxcardVars = names(dhs_data)[grepl('h1a', names(dhs_data))] # indicates whether child had vaccine card
vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h9a_|h0_|h50_|h51_|h54_|h55_|h56_|h57_|h58_|h59_|h60_|h61_|h62_|h63_|h64_|h65_|h66_', names(dhs_data))]

vaxyearVars = names(dhs_data)[grepl('h.*y_', names(dhs_data))] # year vaccines given
vaxmonthVars = names(dhs_data)[grepl('h.*m_', names(dhs_data))] # months vaccine given
vaxdayVars = names(dhs_data)[grepl('h.*d_', names(dhs_data))] # day vaccine given

###############################################################
# tidy the data set according to variables types
###############################################################

######################
# dob year variable
######################
dobyearData <- dhs_data[, c(idVars, dobyearVars), with=F]

dobyearData <- dobyearData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("variable", "child"),
    names_sep = "_",
    values_to = "birth_year",
    values_drop_na = FALSE
  )

# drop extra variable column
dobyearData <- select(dobyearData, -(variable))

######################
# dob month variable
######################
dobmonthData <- dhs_data[, c(idVars, dobmonthVars), with=F]

dobmonthData <- dobmonthData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("variable", "child"),
    names_sep = "_",
    values_to = "birth_month",
    values_drop_na = FALSE
  )

# drop extra variable column
dobmonthData <- select(dobmonthData, -(variable))

######################
# dob day variable
######################
dobdayData <- dhs_data[, c(idVars, dobdayVars), with=F]

dobdayData <- dobdayData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("variable", "child"),
    names_sep = "_",
    values_to = "birth_day",
    values_drop_na = FALSE
  )

# drop extra variable column
dobdayData <- select(dobdayData, -(variable))

######################
# child sex data
######################
childsexData <- dhs_data[, c(idVars, childsexVars), with=F]

childsexData <- childsexData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "sex_of_child",
    values_drop_na = FALSE
  )

# remove extra column
childsexData <- select(childsexData, -(remove))

######################
# child living status
######################
childlivingData <- dhs_data[, c(idVars, childlivingVars), with=F]

childlivingData <- childlivingData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "is_child_alive",
    values_drop_na = FALSE
  )

# remove extra column
childlivingData <- select(childlivingData, -(remove))

######################
# who child resides with
######################
childresidData <- dhs_data[, c(idVars, childresidVars), with=F]

childresidData <- childresidData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "child_resid",
    values_drop_na = FALSE
  )

# remove extra column
childresidData <- select(childresidData, -(remove))

######################
# vaccine card coverage data
######################
vaxcardData <- dhs_data[, c(idVars, vaxcardVars), with=F]

vaxcardData <- vaxcardData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("remove", "child"),
    names_sep = "_",
    values_to = "has_health_card",
    values_drop_na = FALSE
  )

# remove extra column
vaxcardData <- select(vaxcardData, -(remove))

######################
# vaccines card has date recorded (yes or no)
######################
vaxcarddateData <- as_tibble(dhs_data[, c(idVars, vaxcarddateVars), with=F])

vaxcarddateData <- vaxcarddateData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "is_date_recorded",
    values_drop_na = FALSE
  )

######################
# vaccine year variable
######################
vaxyearData <- dhs_data[, c(idVars, vaxyearVars), with=F]

vaxyearData <- vaxyearData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "year",
    values_drop_na = FALSE
  )

# remove suffix specifying year vaccine given
vaxyearData <- vaxyearData %>% 
  separate(vaccine, c("vaccine", "y"), -1)

# drop extra column
vaxyearData <- select(vaxyearData, -(y))

######################
# vaccine month variable
######################
vaxmonthData <- dhs_data[, c(idVars, vaxmonthVars), with=F]

vaxmonthData <- vaxmonthData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "month",
    values_drop_na = FALSE
  )

# remove suffix specifying year vaccine given
vaxmonthData <- vaxmonthData %>% 
  separate(vaccine, c("vaccine", "m"), -1)

# drop extra column
vaxmonthData <- select(vaxmonthData, -(m))

######################
# vaccine day variable
######################
vaxdayData <- dhs_data[, c(idVars, vaxdayVars), with=F]

vaxdayData <- vaxdayData %>%
  pivot_longer(
    !c(caseid, v000, v005, v007, v006, v016),
    names_to = c("vaccine", "child"),
    names_sep = "_",
    values_to = "day",
    values_drop_na = FALSE
  )

# remove suffix specifying year vaccine given
vaxdayData <- vaxdayData %>% 
  separate(vaccine, c("vaccine", "d"), -1)

# drop extra column
vaxdayData <- select(vaxdayData, -(d))

###############################################################
# merge and tidy data in three different dataframes:
# 1: demographic variables
# 2. child health card coverage
# 3. child vaccination dates
###############################################################


######################### 
##### 1. demographic variables
#########################

# create table of mother and household demographic variables
demoData <- as_tibble(dhs_data[, c(idVars, demoVars), with=F])

# merge the mother and household demographic variables with child child characteristics
mergeCols <- c(idVars, "child")
dt1 <- demoData %>% full_join(dobyearData, by = idVars) %>% 
  full_join(dobmonthData,  by = mergeCols) %>%
  full_join(dobdayData, by = mergeCols) %>%
  full_join(childsexData, by = mergeCols) %>%
  full_join(childlivingData, by = mergeCols) %>%
  full_join(childresidData, by=mergeCols)

######################### 
##### 2. vaccine health card variables
#########################

# merge the variables related to vaccine card coverage together
dt2 <- vaxcardData %>% full_join(vaxcarddateData, by = mergeCols)

# bcg, dpt, polio, mea (measles only), hepb, pent, pneu, rota, hepb, hib
dt2 <- dt2 %>% mutate(vaccine=recode(vaccine, 
                                     `h2`="bcg",
                                     `h3`="dpt1",
                                     `h4`="pol1",
                                     `h5`="dpt2",
                                     `h6`="pol2",
                                     `h7`="dpt3",
                                     `h8`="pol3",
                                     `h9`="mea1",
                                     `h9a`="mea2",
                                     `h0`="pol0",
                                     `h50`="hepbbirth",
                                     `h51`="pent1",
                                     `h52`="pent2",
                                     `h53`="pent3",
                                     `h54`="pneu1",
                                     `h55`="pneu2",
                                     `h56`="pneu3",
                                     `h57`="rota1",
                                     `h58`="rota2",
                                     `h59`="rota3",
                                     `h60`="poln",
                                     `h61`="hepb1",
                                     `h62`="hepb2",
                                     `h63`="hepb3",
                                     `h64`="hib1",
                                     `h65`="hib2",
                                     `h66`="hib3"))

# revise vaccien_date_recorded to ensure it will be tidy variable name when pivoted wider
dt2 <- dt2 %>% mutate(
  vaccine_date_recorded = paste(vaccine, "date_recorded", sep = "_")
)

# subset column names
dt2 <- dt2 %>% select(caseid, v000, v005, v007, v006, v016, child, has_health_card, vaccine_date_recorded, is_date_recorded)

# pivot dataset wider
dt2 <- dt2 %>% 
  pivot_wider(names_from = vaccine_date_recorded, values_from = is_date_recorded)
  
#########################
##### 3. child vaccines dates 
#########################

# vaccine merge columns
vaxmergeCols <- c(idVars, "child", "vaccine")

# merge vaccine dates into one variable
dt3 <- vaxyearData %>%
  full_join(vaxmonthData, by = vaxmergeCols) %>%
  full_join(vaxdayData, by = vaxmergeCols)

# recode vaccine column to actual vaccine names 
# bcg, dpt, polio, mea (measles only), hepb, pent, pneu, rota, hepb, hib
dt3 <- dt3 %>% mutate(vaccine=recode(vaccine, 
                                                               `h2`="bcg",
                                                               `h3`="dpt1",
                                                               `h4`="pol1",
                                                               `h5`="dpt2",
                                                               `h6`="pol2",
                                                               `h7`="dpt3",
                                                               `h8`="pol3",
                                                               `h9`="mea1",
                                                               `h9a`="mea2",
                                                               `h0`="pol0",
                                                               `h50`="hepbbirth",
                                                               `h51`="pent1",
                                                               `h52`="pent2",
                                                               `h53`="pent3",
                                                               `h54`="pneu1",
                                                               `h55`="pneu2",
                                                               `h56`="pneu3",
                                                               `h57`="rota1",
                                                               `h58`="rota2",
                                                               `h59`="rota3",
                                                               `h60`="poln",
                                                               `h61`="hepb1",
                                                               `h62`="hepb2",
                                                               `h63`="hepb3",
                                                               `h64`="hib1",
                                                               `h65`="hib2",
                                                               `h66`="hib3"))
# calculate single vaccination date variable
dt3 <- dt3 %>% mutate(vaxdate := make_date(month=month, day=day, year=year))

# subset relevant columns
dt3 <- dt3 %>% select(caseid, v000, v005, v007, v006, v016, vaccine, child, vaxdate)

# pivot vaccine dates wider
dt3 <- dt3 %>% 
  pivot_wider(names_from = vaccine, values_from = vaxdate)

###############################################################
# merge three main datasets together
###############################################################

# re-code child variable in first datatable (dt1) in order to facilitate merge
dt1 <- dt1 %>% mutate(child=recode(child,
                                   `01`="1",
                                   `02`="2",
                                   `03`="3",
                                   `04`="4",
                                   `05`="5",
                                   `06`="6"))


# merge into new dataset called prepped_dhs_data
prepped_dhs_data <- dt1 %>% full_join(dt2, by = mergeCols) %>%
  full_join(dt3, by = mergeCols)

###############################################################
# calculate new variables
###############################################################

# date interview was conducted and date of birth (dob)
prepped_dhs_data <- prepped_dhs_data %>%
  mutate(intv_date := make_date(month=v006, day=v016, year=v007)) %>%
  mutate(dob := make_date(month=birth_month, day=birth_day, year=birth_year))
  
###############################################################
# subset rows
###############################################################

# drop rows that have NA for key columns
prepped_dhs_data <- prepped_dhs_data %>% drop_na(dob, sex_of_child)

# keep only info for the six most recent births
prepped_dhs_data <- filter(prepped_dhs_data, child %in% c("1", "2", "3", "4", "5", "6"))

# remove children that aren't residing with mom
prepped_dhs_data <- filter(prepped_dhs_data, child_resid!=4)

# remove children that weren't born in the last three years
prepped_dhs_data <- filter(prepped_dhs_data, birth_year >2017)

# keep only info for children alive at time of interview
prepped_dhs_data <- filter(prepped_dhs_data, is_child_alive==1)

###############################################################
# subset columns
###############################################################
# remove unnecessary columns
prepped_dhs_data <- prepped_dhs_data %>%
  select(-c(v007, v006, v016, h12, h32, h37, h15, h36, h40, h80, h31, birth_year, birth_month, birth_day))

# reorder columns to put demographic variables first
prepped_dhs_data <- prepped_dhs_data %>% relocate(intv_date, .after = v025) %>%
  relocate(dob, .after = child)

# fix white space in caseid variable
prepped_dhs_data$caseid <- gsub('\\s+', '', prepped_dhs_data$caseid)

###############################################################
# read in next year and country file and bind dataset together once confirmed what the final variable names will be--
# maybe create a codebook for dhs derived dataset which could be read in to make sure all names are consistent
###############################################################


###############################################################
# save as an r-object
###############################################################
saveRDS(prepped_dhs_data, file=outputFile2d)

# print final statement
print("Step 2d: Prepping DHS data now complete.")

