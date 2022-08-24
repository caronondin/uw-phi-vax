# create an r -script output of validation model and results

# clear environment
rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_3/01_set_up_R.R"))

rmarkdown::render(input=paste0(code_dir, "aim_3/04_counterfactual_analyses_measles.R"),
                  output_format = "html_document",
                  output_file = "03_index_validation_output",
                  output_dir = paste0(resDir, "aim_3/"),
)
