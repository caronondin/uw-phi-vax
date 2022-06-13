# ----------------------------------------------
# AUTHOR: Francisco Rios
# PURPOSE: Extract dhs data from stata files
# DATE: Last updated August 20 2021

extract_dhs_data <- function(dir, inFile, containing_folder, dhs_version, loc){
  
  ### TROUBLESHOOTING HELP 
  # Uncomment lines below to run tests
  
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # containing_folder = file_list$containing_folder[i]
  # dhs_version = file_list$data_source[i]
  # loc = file_list$location_name[i]
  
  # Load data
  if (dhs_version %in% c('dhs7', 'dhs6')){
    dhs_data = read_dta(file=paste0(dir, inFile))
    
  } else {
    stop('Not a valid file iteration value.')
  }
  
  ###############################################################
  # subset columns into id values and variables that need tidying
  ###############################################################
  # fix white space in caseid variable
  dhs_data$caseid <- gsub('\\s+', '', dhs_data$caseid) # unfortunately the spaces in between have meaning which might result in duplicate caseids
  
  idVars = c("caseid", "v009", "v010", "v011", "v000", "v005", "v007", "v006", "v016", "sstate") # id variables for merging (include mother's date of birth)
  
  # add check to make sure 
  if (sum(duplicated(dhs_data %>% select(all_of(idVars))))>0){
    stop("The id variables are not uniquely identifying all variables in the dataset.")
  }
  
  if (dhs_version=="dhs7"){
  demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v717", "v501", "v136", "v190", "v191", "v190a", "v191a", "v025") # demographic variables
  } else if (dhs_version=="dhs6"){
    demoVars = c("v012", "v106", "v151", "v155", "v201", "v716", "v717", "v501", "v136", "v190", "v191", "v025") # slighlty different demographic variables
  }
  
  dobyearVars = names(dhs_data)[grepl('b2_', names(dhs_data)) & !grepl('sh', names(dhs_data))] # child's year of birth
  dobmonthVars = names(dhs_data)[grepl('b1_', names(dhs_data)) & !grepl('sh', names(dhs_data))] # child's month of birth
  
  if (dhs_version=="dhs7"){
    dobdayVars = names(dhs_data)[grepl('b17_', names(dhs_data))] # child's day of birth only in dhs7
  }
  
  childsexVars = names(dhs_data)[grepl('b4', names(dhs_data))] # child's sex
  childlivingVars = names(dhs_data)[grepl('b5', names(dhs_data))] # child dead or living status
  childresidVars = names(dhs_data)[grepl('b9_', names(dhs_data))] # where the child is residing
  
  if (dhs_version=="dhs7"){
    vaxcardVars = names(dhs_data)[grepl('h1a', names(dhs_data))] # indicates whether child had vaccine card
    vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h9a_|h0_|h50_|h51_|h52_|h53_|h54_|h55_|h56_|h57_|h58_|h59_|h60_|h61_|h62_|h63_|h64_|h65_|h66_', names(dhs_data))]
    vaxyearVars = names(dhs_data)[grepl('h.*y_', names(dhs_data)) & !grepl('h12|h32|h33|h37|h40', names(dhs_data))] # year vaccines given
    vaxmonthVars = names(dhs_data)[grepl('h.*m_', names(dhs_data)) & !grepl('h12|h15|h31|h32|h33|h36|h37|h40|h80', names(dhs_data))] # months vaccine given
    vaxdayVars = names(dhs_data)[grepl('h.*d_', names(dhs_data)) & !grepl('h12|h15|h31|h32|h33|h36|h37|h40|h80', names(dhs_data))] # day vaccine given
  } else if (dhs_version=="dhs6"){
    vaxcardVars = names(dhs_data[grepl('h1_', names(dhs_data))])
    
    if (loc=="Nigeria"){
      vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h0_|shb1_|shb2_|shb3_|syf_', names(dhs_data))] # indicator for whether date is recorded
      vaxyearVars = names(dhs_data)[grepl('h2y_|h3y_|h4y_|h5y_|h6y_|h7y_|h8y_|h9y_|h0y_|shb1y_|shb2y_|shb3y_|syfy_', names(dhs_data))] # year vaccines given
      vaxmonthVars = names(dhs_data)[grepl('h2m_|h3m_|h4m_|h5m_|h6m_|h7m_|h8m_|h9m_|h0m_|shb1m_|shb2m_|shb3m_|syfm_', names(dhs_data))] # months vaccine given
      vaxdayVars = names(dhs_data)[grepl('h2d_|h3d_|h4d_|h5d_|h6d_|h7d_|h8d_|h9d_|h0d_|shb1d_|shb2d_|shb3d_|syfd_', names(dhs_data))] # day vaccine given
    } else {
      vaxcarddateVars = names(dhs_data)[grepl('h2_|h3_|h4_|h5_|h6_|h7_|h8_|h9_|h0_', names(dhs_data))] # indicator for whether date is recorded
      vaxyearVars = names(dhs_data)[grepl('h.*y_', names(dhs_data))] # year vaccines given
      vaxmonthVars = names(dhs_data)[grepl('h.*m_', names(dhs_data))] # months vaccine given
      vaxdayVars = names(dhs_data)[grepl('h.*d_', names(dhs_data))] # day vaccine given
    }
  }

  ######################
  # dob year variable
  ######################
  dobyearData <- dhs_data[, c(idVars, dobyearVars), with=F]
  
  dobyearData <- dobyearData %>%
    pivot_longer(
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
      names_to = c("variable", "child"),
      names_sep = "_",
      values_to = "birth_month",
      values_drop_na = FALSE
    )
  
  # drop extra variable column
  dobmonthData <- select(dobmonthData, -(variable))
  
  # only dhs7 has a child day of birth variable
  if (dhs_version=="dhs7"){
    
    ######################
    # dob day variable
    ######################
    dobdayData <- dhs_data[, c(idVars, dobdayVars), with=F]

    dobdayData <- dobdayData %>%
      pivot_longer(
        !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
        names_to = c("variable", "child"),
        names_sep = "_",
        values_to = "birth_day",
        values_drop_na = FALSE
      )

    # drop extra variable column
    dobdayData <- select(dobdayData, -(variable))
    
  }
  
  
  ######################
  # child sex data
  ######################
  childsexData <- dhs_data[, c(idVars, childsexVars), with=F]
  
  childsexData <- childsexData %>%
    pivot_longer(
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
      !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
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
  dt1 <- demoData %>% 
    full_join(childsexData, by = idVars) %>%
    full_join(childlivingData, by = mergeCols) %>%
    full_join(childresidData, by=mergeCols) %>% 
    full_join(dobyearData, by = mergeCols) %>% 
    full_join(dobmonthData,  by = mergeCols) 
  
  if (dhs_version=="dhs7"){
    dt1 <- dt1 %>% 
      full_join(dobdayData, by=mergeCols)
  }
  
  ######################### 
  ##### 2. vaccine health card variables
  #########################
  
  # merge the variables related to vaccine card coverage together
  dt2 <- vaxcardData %>% full_join(vaxcarddateData, by = mergeCols)
  
  # bcg, dpt, polio, mea (measles only), hepb, pent, pneu, rota, hepb, hib
  
  if (dhs_version=="dhs7"){
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
                                       `h0`="polbirth",
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
                                       `h60`="ipv",
                                       `h61`="hepb1",
                                       `h62`="hepb2",
                                       `h63`="hepb3",
                                       `h64`="hib1",
                                       `h65`="hib2",
                                       `h66`="hib3"))
  
  } else if (dhs_version=="dhs6"){
    if (loc=="Nigeria"){
      dt2 <- dt2 %>% mutate(vaccine=recode(vaccine,
                                           `h2`="bcg",
                                           `h3`="dpt1",
                                           `h4`="pol1",
                                           `h5`="dpt2",
                                           `h6`="pol2",
                                           `h7`="dpt3",
                                           `h8`="pol3",
                                           `h9`="mea1",
                                           `h0`="polbirth",
                                           `shb1`="hepb1",
                                           `shb2`="hepb2",
                                           `shb3`="hepb3",
                                           `syf`="yelfev"))
    } else {
      dt2 <- dt2 %>% mutate(vaccine=recode(vaccine, 
                                           `h2`="bcg",
                                           `h3`="dpt1",
                                           `h4`="pol1",
                                           `h5`="dpt2",
                                           `h6`="pol2",
                                           `h7`="dpt3",
                                           `h8`="pol3",
                                           `h9`="mea1",
                                           `h0`="polbirth"))      
    }
  }
  
  # revise vaccine_date_recorded to ensure it will be tidy variable name when pivoted wider
  dt2 <- dt2 %>% mutate(
    vaccine_date_recorded = paste(vaccine, "date_recorded", sep = "_")
  )
  
  # subset column names
  dt2 <- dt2 %>% select(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate, child, has_health_card, vaccine_date_recorded, is_date_recorded)
  
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
  
  if (dhs_version=="dhs7"){
    
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
                                       `h0`="polbirth",
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
                                       `h60`="ipv",
                                       `h61`="hepb1",
                                       `h62`="hepb2",
                                       `h63`="hepb3",
                                       `h64`="hib1",
                                       `h65`="hib2",
                                       `h66`="hib3"))
  } else if (dhs_version=="dhs6"){
    if (loc=="Nigeria"){
      dt3 <- dt3 %>% mutate(vaccine=recode(vaccine,
                                           `h2`="bcg",
                                           `h3`="dpt1",
                                           `h4`="pol1",
                                           `h5`="dpt2",
                                           `h6`="pol2",
                                           `h7`="dpt3",
                                           `h8`="pol3",
                                           `h9`="mea1",
                                           `h0`="polbirth",
                                           `shb1`="hepb1",
                                           `shb2`="hepb2",
                                           `shb3`="hepb3",
                                           `syf`="yelfev"
      ))} else {
    dt3 <- dt3 %>% mutate(vaccine=recode(vaccine, 
                                         `h2`="bcg",
                                         `h3`="dpt1",
                                         `h4`="pol1",
                                         `h5`="dpt2",
                                         `h6`="pol2",
                                         `h7`="dpt3",
                                         `h8`="pol3",
                                         `h9`="mea1",
                                         `h0`="polbirth"))
      }
  }
  
  # calculate single vaccination date variable
  dt3 <- dt3 %>% mutate(vaxdate := make_date(month=month, day=day, year=year))
  
  # subset relevant columns
  dt3 <- dt3 %>% select(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate, vaccine, child, vaxdate)
  
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
  
  # if dataset doesn't contain the birth year then add in a placeholder for middle of the month
  if ("birth_day"%in%names(prepped_dhs_data)==FALSE){
    prepped_dhs_data$birth_day <- 15
  }
  
  # date interview was conducted and date of birth (dob)
  prepped_dhs_data <- prepped_dhs_data %>%
    mutate(intv_date := make_date(month=v006, day=v016, year=v007)) %>%
    mutate(dob := make_date(month=birth_month, day=birth_day, year=birth_year))
  
  ###############################################################
  # subset rows
  ###############################################################
  
  # keep only info for the six most recent births
  prepped_dhs_data <- filter(prepped_dhs_data, child %in% c("1", "2", "3", "4", "5", "6"))
  
  # remove households without any children in them
  prepped_dhs_data <- filter(prepped_dhs_data, v201!=0)
  
  # keep only info for children alive at time of interview
  prepped_dhs_data <- filter(prepped_dhs_data, is_child_alive==1)

  # drop rows that have NA for key columns--indicating that row is not a actual data point
  check_na <- prepped_dhs_data %>% filter(is.na(sex_of_child) & is.na(is_child_alive) & is.na(child_resid))
  check_na_sum <- as.data.table(check_na)
  check_na_sum <- check_na_sum[,.(caseid, child, sex_of_child, is_child_alive, child_resid, has_health_card)]
  check_na_sum <- melt(check_na_sum, id.vars = c('caseid', 'child', 'sex_of_child', 'is_child_alive', 'child_resid'), value.name = "has_health_card")
  check_na_sum[, has_health_card:=as.numeric(has_health_card)]
  na_budget = check_na_sum[, sum(has_health_card, na.rm = TRUE)]
  if (na_budget!=0){
      stop("Some rows with  NA for all key variables still have health cards--review drop conditions before dropping NAs in key variables")
  }
  prepped_dhs_data <- prepped_dhs_data %>% filter(!is.na(sex_of_child) & !is.na(is_child_alive) & !is.na(child_resid))

  # re-code the dates of birth that take place after the date of interview to be one day before the interview
  # these are an artifact resulting from selecting the middle of the month for date of birth for surveys with missing day of the month
  prepped_dhs_data <- prepped_dhs_data %>%
    mutate(dob = case_when(intv_date<dob ~ intv_date-1,
                           TRUE ~ dob))
  
  # calculate the child's age in days
  prepped_dhs_data$age_in_days <- time_length(difftime(prepped_dhs_data$intv_date, prepped_dhs_data$dob), "days")
    
  # filter out children older than three--children older than three do not have vaccination data
  prepped_dhs_data <- filter(prepped_dhs_data, age_in_days<=1095)

# recode impausible/inconsistent dates to NA given that we cannot use them in this coded format
  # this includes years of birth recorded as 9997 or 9998
  if (dhs_version=="dhs7"){
  prepped_dhs_data <- prepped_dhs_data %>% 
    mutate(bcg = replace(bcg, bcg>"9000-01-01", NA)) %>%
    mutate(dpt1 = replace(dpt1, dpt1>"9000-01-01", NA)) %>%
    mutate(pol1 = replace(pol1, pol1>"9000-01-01", NA)) %>%
    mutate(dpt2 = replace(dpt2, dpt2>"9000-01-01", NA)) %>%
    mutate(dpt3 = replace(dpt3, dpt3>"9000-01-01", NA)) %>%
    mutate(pol2 = replace(pol2, pol2>"9000-01-01", NA)) %>%
    mutate(pol3 = replace(pol3, pol3>"9000-01-01", NA)) %>%
    mutate(mea1 = replace(mea1, mea1>"9000-01-01", NA)) %>%
    mutate(mea2 = replace(mea2, mea2>"9000-01-01", NA)) %>%
    mutate(polbirth = replace(polbirth, polbirth>"9000-01-01", NA)) %>%
    mutate(hepbbirth = replace(hepbbirth, hepbbirth>"9000-01-01", NA)) %>%
    mutate(pent1 = replace(pent1, pent1>"9000-01-01", NA)) %>%
    mutate(pent2 = replace(pent2, pent2>"9000-01-01", NA)) %>%
    mutate(pent3 = replace(pent3, pent3>"9000-01-01", NA)) %>%
    mutate(pneu1 = replace(pneu1, pneu1>"9000-01-01", NA)) %>%
    mutate(pneu2 = replace(pneu2, pneu2>"9000-01-01", NA)) %>%
    mutate(pneu3 = replace(pneu3, pneu3>"9000-01-01", NA)) %>%
    mutate(rota1 = replace(rota1, rota1>"9000-01-01", NA)) %>%
    mutate(rota2 = replace(rota2, rota2>"9000-01-01", NA)) %>%
    mutate(rota3 = replace(rota3, rota3>"9000-01-01", NA)) %>%
    mutate(ipv = replace(ipv, ipv>"9000-01-01", NA)) %>%
    mutate(hepb1 = replace(hepb1, hepb1>"9000-01-01", NA)) %>%
    mutate(hepb2 = replace(hepb2, hepb2>"9000-01-01", NA)) %>%
    mutate(hepb3 = replace(hepb3, hepb3>"9000-01-01", NA)) %>%
    mutate(hib1 = replace(hib1, hib1>"9000-01-01", NA)) %>%
    mutate(hib2 = replace(hib2, hib2>"9000-01-01", NA)) %>%
    mutate(hib3 = replace(hib3, hib3>"9000-01-01", NA))
  } else if (dhs_version=="dhs6"){
    if (loc=="Nigeria"){
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(bcg = replace(bcg, bcg>"9000-01-01", NA)) %>%
        mutate(dpt1 = replace(dpt1, dpt1>"9000-01-01", NA)) %>%
        mutate(pol1 = replace(pol1, pol1>"9000-01-01", NA)) %>%
        mutate(pol2 = replace(pol2, pol2>"9000-01-01", NA)) %>%
        mutate(dpt2 = replace(dpt2, dpt2>"9000-01-01", NA)) %>%
        mutate(dpt3 = replace(dpt3, dpt3>"9000-01-01", NA)) %>%
        mutate(pol3 = replace(pol3, pol3>"9000-01-01", NA)) %>%
        mutate(mea1 = replace(mea1, mea1>"9000-01-01", NA)) %>%
        mutate(polbirth = replace(polbirth, polbirth>"9000-01-01", NA)) %>%
        mutate(hepb1 = replace(hepb1, hepb1>"9000-01-01", NA)) %>%
        mutate(hepb2 = replace(hepb2, hepb2>"9000-01-01", NA)) %>%
        mutate(hepb3 = replace(hepb3, hepb3>"9000-01-01", NA)) %>%
        mutate(yelfev = replace(yelfev, yelfev>"9000-01-01", NA))
    } else {
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(bcg = replace(bcg, bcg>"9000-01-01", NA)) %>%
        mutate(dpt1 = replace(dpt1, dpt1>"9000-01-01", NA)) %>%
        mutate(pol1 = replace(pol1, pol1>"9000-01-01", NA)) %>%
        mutate(pol2 = replace(pol2, pol2>"9000-01-01", NA)) %>%
        mutate(dpt2 = replace(dpt2, dpt2>"9000-01-01", NA)) %>%
        mutate(dpt3 = replace(dpt3, dpt3>"9000-01-01", NA)) %>%
        mutate(pol3 = replace(pol3, pol3>"9000-01-01", NA)) %>%
        mutate(mea1 = replace(mea1, mea1>"9000-01-01", NA)) %>%
        mutate(polbirth = replace(polbirth, polbirth>"9000-01-01", NA))
    }
  }
  
  # recode vaccine dates listed as before the date of birth to the date of birth to account for our middle-of-month assumption for DHS6
  if (dhs_version=="dhs7"){
    prepped_dhs_data <- prepped_dhs_data %>% 
      mutate(bcg = replace(bcg, bcg<dob, NA)) %>%
      mutate(dpt1 = replace(dpt1, dpt1<dob, NA)) %>%
      mutate(pol1 = replace(pol1, pol1<dob, NA)) %>%
      mutate(dpt2 = replace(dpt2, dpt2<dob, NA)) %>%
      mutate(dpt3 = replace(dpt3, dpt3<dob, NA)) %>%
      mutate(pol2 = replace(pol2, pol2<dob, NA)) %>%
      mutate(pol3 = replace(pol3, pol3<dob, NA)) %>%
      mutate(mea1 = replace(mea1, mea1<dob, NA)) %>%
      mutate(mea2 = replace(mea2, mea2<dob, NA)) %>%
      mutate(polbirth = replace(polbirth, polbirth<dob, NA)) %>%
      mutate(hepbbirth = replace(hepbbirth, hepbbirth<dob, NA)) %>%
      mutate(pent1 = replace(pent1, pent1<dob, NA)) %>%
      mutate(pent2 = replace(pent2, pent2<dob, NA)) %>%
      mutate(pent3 = replace(pent3, pent3<dob, NA)) %>%
      mutate(pneu1 = replace(pneu1, pneu1<dob, NA)) %>%
      mutate(pneu2 = replace(pneu2, pneu2<dob, NA)) %>%
      mutate(pneu3 = replace(pneu3, pneu3<dob, NA)) %>%
      mutate(rota1 = replace(rota1, rota1<dob, NA)) %>%
      mutate(rota2 = replace(rota2, rota2<dob, NA)) %>%
      mutate(rota3 = replace(rota3, rota3<dob, NA)) %>%
      mutate(ipv = replace(ipv, ipv<dob, NA)) %>%
      mutate(hepb1 = replace(hepb1, hepb1<dob, NA)) %>%
      mutate(hepb2 = replace(hepb2, hepb2<dob, NA)) %>%
      mutate(hepb3 = replace(hepb3, hepb3<dob, NA)) %>%
      mutate(hib1 = replace(hib1, hib1<dob, NA)) %>%
      mutate(hib2 = replace(hib2, hib2<dob, NA)) %>%
      mutate(hib3 = replace(hib3, hib3<dob, NA))
  } else if (dhs_version=="dhs6"){
    if (loc=="Nigeria"){
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(
          bcg = case_when(bcg<dob & !is.na(bcg) ~ dob,
                          TRUE ~ bcg),
          dpt1 = case_when(dpt1<dob & !is.na(dpt1) ~ dob,
                           TRUE ~ dpt1),
          dpt2 = case_when(dpt2<dob & !is.na(dpt2) ~ dob,
                           TRUE ~ dpt2),
          dpt3 = case_when(dpt3<dob & !is.na(dpt3) ~ dob,
                           TRUE ~ dpt3),
          polbirth = case_when(polbirth<dob & !is.na(polbirth) ~ dob,
                           TRUE ~ polbirth),
          pol1 = case_when(pol1<dob & !is.na(pol1) ~ dob,
                           TRUE ~ pol1),
          pol2 = case_when(pol2<dob & !is.na(pol2) ~ dob,
                           TRUE ~ pol2),
          pol3 = case_when(pol3<dob & !is.na(pol3) ~ dob,
                           TRUE ~ pol3),
          mea1 = case_when(mea1<dob & !is.na(mea1) ~ dob,
                           TRUE ~ mea1),
          hepb1 = case_when(hepb1<dob & !is.na(hepb1) ~ dob,
                            TRUE ~ hepb1),
          hepb2 = case_when(hepb2<dob & !is.na(hepb2) ~ dob,
                            TRUE ~ hepb2),
          hepb3 = case_when(hepb3<dob & !is.na(hepb3) ~ dob,
                            TRUE ~ hepb3),
          yelfev = case_when(yelfev<dob & !is.na(yelfev) ~ dob,
                             TRUE ~ yelfev))
    } else {
      prepped_dhs_data <- prepped_dhs_data %>%
        mutate(
          bcg = case_when(bcg<dob & !is.na(bcg) ~ dob,
                          TRUE ~ bcg),
          dpt1 = case_when(dpt1<dob & !is.na(dpt1) ~ dob,
                           TRUE ~ dpt1),
          dpt2 = case_when(dpt2<dob & !is.na(dpt2) ~ dob,
                           TRUE ~ dpt2),
          dpt3 = case_when(dpt3<dob & !is.na(dpt3) ~ dob,
                           TRUE ~ dpt3),
          polbirth = case_when(polbirth<dob & !is.na(polbirth) ~ dob,
                           TRUE ~ polbirth),
          pol1 = case_when(pol1<dob & !is.na(pol1) ~ dob,
                           TRUE ~ pol1),
          pol2 = case_when(pol2<dob & !is.na(pol2) ~ dob,
                           TRUE ~ pol2),
          pol3 = case_when(pol3<dob & !is.na(pol3) ~ dob,
                           TRUE ~ pol3),
          mea1 = case_when(mea1<dob & !is.na(mea1) ~ dob,
                           TRUE ~ mea1))
    }
  }
  
  ###############################################################
  # subset columns
  ###############################################################
  # remove unnecessary columns
  if (dhs_version=="dhs7"){
    prepped_dhs_data <- prepped_dhs_data %>%
      select(-c(v007, v006, v016, 
                birth_day, birth_year, birth_month))
  } else if (dhs_version=="dhs6"){
    prepped_dhs_data <- prepped_dhs_data %>%
      select(-c(v007, v006, v016, birth_day, birth_year, birth_month))
  }
  
  # reorder columns to put demographic variables first
  prepped_dhs_data <- prepped_dhs_data %>% relocate(intv_date, .after = v025) %>%
    relocate(dob, .after = child)
  
  #Check column names, and that you have at least some valid data for the file.
  if (nrow(prepped_dhs_data)==0){
    stop(paste0("All data dropped for ", inFile))
  }

  # return the prepped data set
  return(prepped_dhs_data)
}
