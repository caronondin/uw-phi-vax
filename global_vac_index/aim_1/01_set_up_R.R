# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified September 20, 2021

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
# library(GGally)
library(ggrepel)
library(haven)
library(table1)
library(tidyr)
# library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(survival)
# library(cmprsk)
# library(survminer)
library(vtable)

# Define important variables -----

# set shared team Google drive and code repo dynamically
if (Sys.info()[2]=='10 x64'){
    g_drive  <- 'G:/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/'
    code_dir <- 'C:/Users/frc2/Documents/uw-phi-vax/'
  } else if (Sys.info()[2]=='Server x64'){
    g_drive  <- 'G:/Shared with Me/Merck Vaccine Improvement Index Project/'
    code_dir <- 'H:/uw-phi-vax/'
  } else {
    g_drive  <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/'
    code_dir <- '~/Documents/uw-phi-vax/'
    }

setwd(code_dir) # set the working directory to wherever code is stored
raw_data_dir <- paste0(g_drive,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(g_drive,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(g_drive,"Data/documentation/codebooks/") # location of codebooks for interpreting data
resDir <- paste0(g_drive, "Results/") # location of  any result outputs
visDir <- paste0(g_drive,"Visualizations/") # location where visualizations are saved

# Define important files referenced in multiple sheets
outputFile02 <- paste0(prepped_data_dir, "01_vaccine_trends.RDS")
outputFile03 <- paste0(prepped_data_dir, "02_sdi.RDS")
outputFile05 <- paste0(prepped_data_dir, "03_raw_extracted_dhs.RDS")
outputFile06 <- paste0(prepped_data_dir, "04_prepped_dhs_for_mov.RDS")
outputFile08 <- paste0(prepped_data_dir, "05_disease_trends.RDS")
# outputFile09 <- paste0(prepped_data_dir, "06_merged_data_for_visuals.RDS")

# Source shared functions -----
source(paste0(code_dir, "functions/prep_vax_trend_data.R"))
source(paste0(code_dir, "functions/prep_dx_trend_data.R"))
source(paste0(code_dir, "functions/strip_chars.R"), encoding = "UTF-8")
source(paste0(code_dir, "functions/extract_dhs_data.R"), encoding = "UTF-8")

# Set Boolean switches -----
prep_vax_trends  = FALSE  # this reads in and preps data on vaccination trends
prep_sdi         = FALSE # this reads in and preps SDI location values 

id_low_sdi       = FALSE # identifies highest performing countries in the low SDI category
id_med_sdi       = FALSE # identifies highest performing countries in the medium SDI category
id_high_sdi      = FALSE # identifies highest performing countries in the high SDI category

run_extract_dhs  = FALSE # this reads in DHS data and extracts relevant variables
prep_dhs_data    = FALSE # reads in extracted DHS data and prepares for further analyses

prep_dx_trends   = FALSE # this reads in GBD data on vaccine-preventable diseases

merge_files      = FALSE # merges SDI, vaccine coverage trends and disease trends by location


# Source scripts in prep pipeline -----

# ---------------------------------------
# 1. Prep vaccination trends data 
# ---------------------------------------
if(prep_vax_trends == TRUE){
  source('./02_prep_vacc_trend_data.R')
}

# ---------------------------------------
# 2. Prep SDI data 
# ---------------------------------------
if(prep_sdi == TRUE){
  source('./03_prep_sdi_data.R')
}

# ---------------------------------------
# 3. Identify outlier countries
# ---------------------------------------
if (id_low_sdi == TRUE){
  source('./04_id_countries_low_sdi.R')
}

# if(id_med_sdi == TRUE){
#   source('.archive/08b_id_countries_med_sdi.R')
# }
# 
# if(id_high_sdi == TRUE){
#   source('.archive/08c_id_countries_high_sdi.R')
# }


# ---------------------------------------
# 4. Extract DHS data
# ---------------------------------------
if (run_extract_dhs == TRUE){
  source('./05_run_extract_dhs_data.R')
}

# ---------------------------------------
# 5. Prep DHS data for analyses
# ---------------------------------------
if (prep_dhs_data == TRUE){
  source('./06_prep_dhs_data.R')
}

# ---------------------------------------
# 6. Prep disease trends data
# ---------------------------------------
if(prep_dx_trends == TRUE){
  source('./08_prep_disease_data.R')
}

# ---------------------------------------
# 7. Merge major data sets together
# --------------------------------------- 
if(merge_files == TRUE){
  source('./09_merge_data_for_visuals.R')
}

# ---------------------------------------
# 8. Exploratory graphs and visuals for aim 1 
# ---------------------------------------





