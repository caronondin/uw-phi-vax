# Author: Francisco Rios 
# Purpose: set up file for aim 3 analyses
# Date: Last modified July 14, 2022

# Load required packages -----
library(data.table)
library(MASS)
library(ggplot2)
library(readxl)
library(tidyverse)

# Define important variables -----
set.seed(500)

# set shared team drive and code repo dynamically
if (Sys.info()[6]=="frc2"){
  if (Sys.info()[2]=='10 x64'){
  file_folder  <- 'C:/Users/frc2/UW/og_phi_global_vaccination_improvement_project - General/'
  code_dir <- 'C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/'
} else if (Sys.info()[2]=='Server x64'){
  file_folder  <- 'G:/Shared with Me/Merck Vaccine Improvement Index Project/'
  code_dir <- 'H:/uw-phi-vax/global_vac_index/'
} else {
  file_folder  <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/'
  code_dir <- '~/Documents/uw-phi-vax/'
} 
}


setwd(code_dir) # set the working directory to wherever code is stored
raw_data_dir <- paste0(file_folder,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(file_folder,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(file_folder,"Data/documentation/codebooks/aim_2/") # location of codebooks for interpreting data
resDir <- paste0(file_folder, "Results/") # location of  any result outputs
visDir <- paste0(file_folder,"Visualizations/") # location where visualizations are saved
