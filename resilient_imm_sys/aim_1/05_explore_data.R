## Create Visuals of State-level Vaccination Data
# Author: Francisco Rios 
# Purpose: Load modeled estimates and create visualizations
# Date: Last modified January 21, 2022
rm(list=ls())
# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

# Load modeled estimates
data <- readRDS(paste0(prepped_data_dir, "02_estimated_vaccine_coverage.RDS"))

# format and label data
label(data$INCPOV1) <- "POVERTY STATUS"
INCPOVlevels=c(1,2,3,4)
INCPOVlabels=c("ABOVE POVERTY, > $75K", "ABOVE POVERTY, <= $75K", "BELOW POVERTY", "UNKNOWN INCOME")
data$INCPOV1 <- factor(data$INCPOV1, levels=INCPOVlevels, labels=INCPOVlabels)

label(data$RACEETHK_R) <- "RACE/ETHNICITY OF CHILD"
RACEETHKRlevels=c(1,2,3,4)
RACEETHKRlabels=c("NON-HISPANIC WHITE ONLY", "HISPANIC", "NON-HISPANIC BLACK ONLY", "OTHER + MULTIPLE RACE")
data$RACEETHK_R <- factor(data$RACEETHK_R, levels=RACEETHKRlevels, labels=RACEETHKRlabels)

# create graphics
# create series of box plots
vaccines <- unique(data$VACCINE)

# names of files that will be created
outputFile05_01 <- paste0(visDir, "01_vaccination_trends.PDF")
outputFile05_02 <- paste0(visDir, "02_state_level_vaccination_trends.PDF")

# Save file
print(paste('Saving:', outputFile05_01)) 
pdf(outputFile05_01, height = 8.5, width = 11)

for (i in 1:length(vaccines)) {
  g <- ggplot(data %>% filter(VACCINE==vaccines[i]), aes(YEAR, PredictedProb, group=factor(ESTIAP))) + 
    geom_line(aes(color=ESTIAP), show.legend = FALSE) +
    facet_grid(vars(INCPOV1), vars(RACEETHK_R)) +
    labs(title = paste0(unique(vaccines[i]), " vaccination trends between 2007 and 2020"),
         subtitle = "Stratified according to race/ethnicity and family income") +
    ylab('Predicted probability of being vaccinated') +
    xlab('Year') +
    theme_minimal()
  print(g)
}

dev.off()

# code the locations for plotting
ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
                "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
                "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
data$ESTIAP <- factor(data$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)

# make vector of all the locations
lctns <- unique(data$ESTIAP)
labelTable <- as.data.table(unique(data %>% select(ESTIAP, VACCINE)))

tsPlots = lapply(seq(length(lctns)), function(g) {
  l = unique(labelTable[ESTIAP == lctns[[g]]]$ESTIAP)
  ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = VACCINE)) + 
    geom_ribbon(aes(ymin = LL,
                    ymax = UL, fill = VACCINE), alpha = 0.2) +
    geom_line(size = 1, alpha = .8) + 
    facet_wrap(~INCOME) +
    labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
         subtitle = "Stratified according to race/ethnicity and family income") +
    theme_minimal() +
    facet_grid(vars(INCPOV1), vars(RACEETHK_R))
})

# Save file
print(paste('Saving:', outputFile05_02)) 
pdf(outputFile05_02, height = 8.5, width = 11)
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}
dev.off()