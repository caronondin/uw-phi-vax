# create demographic output state level (from NIS)

# source previous files and directory
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

# load other packes
library(table1)

# load data
nis_data <- readRDS(paste0(prepped_data_dir, "01_complete_nis_data.RDS")) %>%
  filter(YEAR==2007 | YEAR==2019)

# format data for plotting
nis_data$YEAR <- factor(nis_data$YEAR, levels=c(2007, 2019))

EDUC1_levels=c(1,2,3,4,77,99)
EDUC1_labels=c("< 12 YEARS", "12 YEARS", "> 12 YEARS, NON-COLLEGE GRAD",
               "COLLEGE GRAD", "DON'T KNOW", "REFUSED")

INCPOVlevels=c(1,2,3,4)
INCPOVlabels=c("ABOVE POVERTY, > $75K", "ABOVE POVERTY, <= $75K",
               "BELOW POVERTY", "UNKNOWN")

MOBILlevels=c(1,2,77,99)
MOBILlabels=c("MOVED FROM DIFFERENT STATE", "DID NOT MOVE FROM DIFFERENT STATE",
              "DON'T KNOW", "REFUSED")

LANGUAGElevels=c(1,2,3)
LANGUAGElabels=c("ENGLISH", "SPANISH", "OTHER")

INSURANCElevels=c(1,2,3,4)
INSURANCElabels=c("PRIVATE ONLY", "ANY MEDICAID", "OTHER INSURANCE", "UNINSURED")

RACEETHK_Rlevels=c(1, 2, 3, 4)
RACEETHK_Rlabels=c("White", "Hispanic", "Black", "Other")

ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
                "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
                "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")


nis_data$EDUC1 <- factor(nis_data$EDUC1, levels=EDUC1_levels, labels=EDUC1_labels)
nis_data$INCPOV1 <- factor(nis_data$INCPOV1, levels=INCPOVlevels, labels=INCPOVlabels)
nis_data$MOBIL <- factor(nis_data$MOBIL, levels=MOBILlevels, labels=MOBILlabels)
nis_data$LANGUAGE <- factor(nis_data$LANGUAGE, levels=LANGUAGElevels, labels=LANGUAGElabels)
nis_data$xxINSURANCE <- factor(nis_data$INSURANCE, levels=INSURANCElevels, labels=INSURANCElabels)
nis_data$RACEETHK_R <- factor(nis_data$RACEETHK_R, levels=RACEETHK_Rlevels, labels=RACEETHK_Rlabels)
nis_data$ESTIAP <- factor(nis_data$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)

# create function that loops through every state

# create function that loops though only states in each location
locations <- unique(nis_data$ESTIAP)
demographics = lapply(locations, function(x){
  table1( ~ EDUC1 + INCPOV1 + MOBIL + LANGUAGE + INSURANCE + RACEETHK_R + INCPOV1 | YEAR, data = nis_data %>% filter(ESTIAP==x), topclass="Rtable1-grid")
})

library(htmltools)
for (i in 1:length(locations)){
  name <- locations[i]
  save_html(demographics[[i]], file = paste0(visDir, "demographics/", name, "_demo_table.html"))
}

# 
# i <- 1
# for(i in 1:56){
#   name <- locations[i]
#   pdf(file=paste0(visDir, "07_demographics/", name, "demo_table.pdf"))
#   print(demographics[[i]])
#   dev.off()
# }
# 
# for(i in 1:3){
#   save_html
#   pdf(paste("c:/", i, ".pdf", sep=""))
#   plot(cbind(iris[1], iris[i]))
#   dev.off()
# }
# pdf(file=paste0(visDir, "demo_tables.PDF"))
# for (i in 1:length(locations)){
#   print(locations[i])
#   demographics[[i]]
# }
# dev.off()
# 
# 
# 
