# Author: Francisco Rios and Caroline Nondin
# Purpose: Set up R for prepping UW PHI Vaccination Data for Liberia Dataset 
# Date: Last modified November 15th, 2022

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
library(labelled)
library(naniar)
library(survminer)

# library(GGally)
# library(plyr)
# library(cmprsk)
# library(survminer)
# Define important variables -----

# set shared team Google drive and code repo dynamically
g_drive  <- '/Users/cnondin/Library/CloudStorage/OneDrive-SharedLibraries-UW/og_phi_global_vaccination_improvement_project - General/'
code_dir <- 'C:/Documents/PHI TA job/aim 1/git_repo/uw-phi-vax/global_vac_index/'

#setwd(code_dir) # set the working directory to wherever code is stored
#^CAROLINE: might have to work on this, so far there is no working directory 
raw_data_dir <- paste0(g_drive,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(g_drive,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(g_drive,"Data/documentation/codebooks/aim_1/") # location of codebooks for interpreting data
#^CAROLINE: added aim_1 folder 
resDir <- paste0(g_drive, "Results/") # location of  any result outputs
visDir <- paste0(g_drive,"Visualizations/") # location where visualizations are saved

# Define important files referenced in multiple sheets
outputFile02 <- paste0(prepped_data_dir, "aim_1/01_vaccine_trends_Lib.RDS")
outputFile03 <- paste0(prepped_data_dir, "aim_1/02_sdi_Lib.RDS")
outputFile05 <- paste0(prepped_data_dir, "aim_1/03_raw_extracted_dhs_Lib.RDS")
outputFile05a <- paste0(prepped_data_dir, "aim_1/05a_prepped_dhs_full_Lib.RDS") #CAROLINE: added an extra file to save all variables in prepped dataset
outputFile06 <- paste0(prepped_data_dir, "aim_1/04_prepped_dhs_for_mov_Lib.RDS")
outputFile08 <- paste0(prepped_data_dir, "aim_1/05_disease_trends_Lib.RDS")
outputFile09 <- paste0(prepped_data_dir, "aim_1/06_merged_data_for_visuals_Lib.RDS")
