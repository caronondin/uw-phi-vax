# Author: Francisco Rios 
# Purpose: Set up R for prepping UW PHI Vaccination Data
# Date: Last modified September 20, 2021

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
library(tidyverse)

# Define important variables -----
set.seed(500)

# set shared team Google drive and code repo dynamically
if (Sys.info()[2]=='10 x64'){
  g_drive  <- 'G:/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/'
  code_dir <- 'C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/'
} else if (Sys.info()[2]=='Server x64'){
  g_drive  <- 'G:/Shared with Me/Merck Vaccine Improvement Index Project/'
  code_dir <- 'H:/uw-phi-vax/global_vac_index/'
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
