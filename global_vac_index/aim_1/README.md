# uw-phi-vax
Repository to store code used in the Vaccine Improvement Project at the UW Population Health Initiative

Last updated by Francisco Rios Casas (frc2@uw.edu) on September 20, 2021

## Main files included in this repo:
  
 * "1_set_up_R.R": lists important packages, file paths, functions and order of scripts to carry out analyses.
  
 * "2a_prep_vaccination_trend_data.R": reads in vaccination coverage data, appends file name, and saves in prepped data folder.
  
 * "2b_prep_sdi_data.R": reads in SDI data from past 20 years, merges onto national/subnational GBD hierarchy and also reshapes data to merge onto vaccination coverage.
  
  * "2c_prep_disease_data.R": reads in all GBD files (one per vaccine-preventable disease) to append them together and save on the prepped data file.
  
  * "2d_run_extract_dhs_data.R": reads in DHS surveys from selected countries, and extracts relevant variables for MOV analysis (vaccination dates, demographic and household information).
  
  * "3_merge_data.R": merges SDI values onto vaccination coverage data. To be expanded to further prepare data for further analyses or visualizations.
  
  * "4_visualizations_aim1.R": [DRAFT]: creates sample visualizations to explore trends in vaccine coverage. To be expanded with additional examples. 
  
  * "5a_id_countries_low_sdi.R": creates tables and charts to identify countries with most improved trends in vaccine coverage in low-SDI group. 
  
  * "6a_prep_dhs_data.R": re-codes, labels, and reformats variables as appropriate. Also calculates new variables such as indicator for missed opportunity for vaccination.
  
  * "6b_mov_analyses.R": creates tables and figures for missed opportunities and survival curves for time until vaccination.

## Files included in the "Functions" folder:
   This folder contains functions that are called in other scripts to extract or prep data.
   
   * "extract_dhs_data.R": extracts relevant DHS data according to year and location of the DHS survey. References list_of_data_used. 
   * "prep_dx_trend_data.R": extracts vaccine-preventable disease data.  References list_of_data_used. 
   * "prep_vax_trend_data.R": extracts vaccination coverage  data. References list_of_data_used. 
   * "strip_chars.R": removes special characters from datasets to faciliate merge, such as among location names. 

## Files included in the "Archive" folder:
  This folder contains files that are no longer used or not part of the main data prep or analysis.


  

