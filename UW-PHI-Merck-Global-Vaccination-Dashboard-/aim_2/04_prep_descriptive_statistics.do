***File to create descriptive statistics for DPP Paper
***Francisco Rios Casas
***July 14, 2022

*** Load data file
use "G:\Misc_Projects\dpp_paper\data\prepped_data\02_prepped_data_for_analysis.dta"

*** install modules
ssc install mdesc

*** apply survey weights
svyset [pw = ewt1234]