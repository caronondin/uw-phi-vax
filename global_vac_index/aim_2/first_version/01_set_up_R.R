# Author: Francisco Rios 
# Purpose: Set up R for Aim 2 of project: constructing Vaccine Improvement Index
# Date: Last modified July 12 2022

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
library(tidyverse)

# Define important variables -----
set.seed(500)

# set shared team drive and repo automatically based on user
if (Sys.info()[7]=="frc2"){
  if (Sys.info()[2]=='10 x64'){
    file_drive  <- 'C:/Users/frc2/UW/og_phi_global_vaccination_improvement_project - General/'
    code_dir <- 'C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/'
  } else if (Sys.info()[2]=='Server x64'){
    # file_drive  <- 'G:/Shared with Me/Merck Vaccine Improvement Index Project/'
    stop("File drive needs to be updated for use on CSDE Server")
    code_dir <- 'H:/uw-phi-vax/global_vac_index/'
  }
} else if (Sys.info()[7]==""){
    # update this section for new users attempting to access files
  }

setwd(code_dir) # set the working directory to wherever code is stored
raw_data_dir <- paste0(file_drive,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(file_drive,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(file_drive,"Data/documentation/codebooks/aim_2/") # location of codebooks for interpreting data
resDir <- paste0(file_drive, "Results/") # location of  any result outputs
visDir <- paste0(file_drive,"Visualizations/") # location where visualizations are saved
