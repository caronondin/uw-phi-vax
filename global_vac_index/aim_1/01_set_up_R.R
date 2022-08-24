# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified September 20, 2021

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
library(ggrepel)
library(haven)
library(table1)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(survival)
library(vtable)
library(stringr)
library(kableExtra)

# library(GGally)
# library(plyr)
# library(cmprsk)
# library(survminer)
# Define important variables -----

# set shared team Google drive and code repo dynamically
if (Sys.info()[2]=='10 x64'){
    g_drive  <- 'C:/Users/frc2/UW/og_phi_global_vaccination_improvement_project - General/'
    code_dir <- 'C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/'
  } else if (Sys.info()[2]=='Server x64'){
    g_drive  <- 'G:/Shared with Me/Merck Vaccine Improvement Index Project/'
    code_dir <- 'H:/uw-phi-vax/global_vac_index/'
  } else {
    g_drive  <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/'
    code_dir <- '~/Documents/uw-phi-vax/global_vac_index/'
    }

setwd(code_dir) # set the working directory to wherever code is stored
raw_data_dir <- paste0(g_drive,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(g_drive,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(g_drive,"Data/documentation/codebooks/") # location of codebooks for interpreting data
resDir <- paste0(g_drive, "Results/") # location of  any result outputs
visDir <- paste0(g_drive,"Visualizations/") # location where visualizations are saved

# Define important files referenced in multiple sheets
outputFile02 <- paste0(prepped_data_dir, "aim_1/01_vaccine_trends.RDS")
outputFile03 <- paste0(prepped_data_dir, "aim_1/02_sdi.RDS")
outputFile05 <- paste0(prepped_data_dir, "aim_1/03_raw_extracted_dhs.RDS")
outputFile06 <- paste0(prepped_data_dir, "aim_1/04_prepped_dhs_for_mov.RDS")
outputFile08 <- paste0(prepped_data_dir, "aim_1/05_disease_trends.RDS")
outputFile09 <- paste0(prepped_data_dir, "aim_1/06_merged_data_for_visuals.RDS")

# Source shared functions -----
source(paste0(code_dir, "functions/prep_vax_trend_data.R"))
source(paste0(code_dir, "functions/prep_dx_trend_data.R"))
source(paste0(code_dir, "functions/strip_chars.R"), encoding = "UTF-8")
source(paste0(code_dir, "functions/extract_dhs_data.R"), encoding = "UTF-8")