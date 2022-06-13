# Author: Francisco Rios 
# Purpose: final data prep and visualize results
# Date: Last modified January 26, 2022

# PENDING IDEAS
# Add survey weights
# try calculating relative ratios
# check for statistical significance

rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_set_up_R.R"))

data <- readRDS(paste0(prepped_data_dir, "03_estimates_vaccine_coverage_2007-2019.RDS"))

# final variable prep
data$VACCINE <- tolower(data$VACCINE)
data$VACCINE <- gsub(" ", "", data$VACCINE)

data <- data %>% 
  mutate(RACEETHK_R = case_when(RACEETHK_R==1 ~ "white",
                                RACEETHK_R==2 ~ "hispa",
                                RACEETHK_R==3 ~ "black",
                                RACEETHK_R==4 ~ "other"),
         INCPOV1 = case_when(INCPOV1==1 ~ "high",
                             INCPOV1==2 ~ "med", 
                             INCPOV1==3 ~ "low",
                             INCPOV1==4 ~ "miss"))

# code the locations for plotting
ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
                "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
                "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
data$ESTIAP <- factor(data$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)

# re-shape the data to facilitate calculation
dt2 <- pivot_wider(data, id_cols = c(ESTIAP, RACEETHK_R, INCPOV1, VACCINE), names_from = c(YEAR), values_from = PredictedProb)

# calculate the change in the time periods of interest for each vaccine
dt2$change <- ((dt2$`2019`-dt2$`2007`)/dt2$`2007`)*100

# reshape data for additional calculations
dt3 <- pivot_wider(dt2, id_cols = c(ESTIAP, INCPOV1, VACCINE), names_from = RACEETHK_R, values_from = c(change))

dt3$eii_hispa <- (dt3$hispa - dt3$white)
dt3$eii_black <- (dt3$black - dt3$white)
dt3$eii_other <- (dt3$other - dt3$white)

# Reshape data for additional calculations
dt4 <- pivot_wider(dt3, id_cols = c(ESTIAP), names_from = c(INCPOV1, VACCINE), values_from = c(eii_hispa, eii_black, eii_other))

# create function that deletes outliers
is_outlier <- function(x){
  lower_bound <- quantile(x, 0.05)
  upper_bound <- quantile(x, 0.95)
  ifelse((x < lower_bound | x > upper_bound), NA, x)
}

numVars <- names(dt4)[2:37]
for (v in numVars) {
  dt4 <- as.data.table(dt4)
  dt4[, (v):=is_outlier(get(v))]
}

# calculate the quantile groups for each column
# summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply( dt4[,2:37] , 2 , quantile , probs = quants , na.rm = TRUE))
quantiles$quant <- NA
quantiles$quant[1] <- "first_q"
quantiles$quant[2] <- "third_q"

quantiles <- pivot_longer(quantiles, cols = starts_with("eii"), names_to = c("group"))
quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value" )
quantiles <- quantiles %>% separate(group, into = c("variable", "race", "income", "vaccine"))
quantiles <- quantiles %>% select(-c(variable))

# reshape dataset
dt5 <- dt4 %>%
  pivot_longer(
    !c(ESTIAP),
    names_to = c("group"),
    values_to = "eii",
    values_drop_na = FALSE) %>% 
  separate(group, into = c("variable", "race", "income", "vaccine")) %>%
  select(-c(variable))

# merge the dt_groups data to the quantiles cut off points
dt6 <- merge(dt5, quantiles, by=c("race", "income", "vaccine"))

# calculate the group for each location, race, income and vaccine
dt6 <- dt6 %>% 
  mutate(equitable_improvement = case_when(eii > 0 ~ TRUE,
                                           TRUE ~ FALSE),
         category = case_when(eii >= first_q & eii <= third_q ~ "medium",
                     eii >= third_q ~ "high",
                     eii <= first_q ~ "low",
                     is.na(eii) ~ "outlier"))

# # merge dt2 and dt6 to create dataset with both percent change, no outliers, and values
plot_data <- dt2 %>% full_join(dt6, by=c("ESTIAP", "RACEETHK_R"="race", "INCPOV1"="income", "VACCINE"="vaccine"))

plot_data <- plot_data %>% mutate(
  category = case_when(
    RACEETHK_R=="white" & is.na(category)~"reference",
    TRUE ~ category ))

# factor the race, and income variables
plot_data$RACEETHK_R <- factor(plot_data$RACEETHK_R, levels=c("white", "hispa", "black", "other"), labels = c("White", "Hispanic", "Black", "Other"))
plot_data$INCPOV1 <- factor(plot_data$INCPOV1, levels=c("low", "med", "high", "miss"), labels = c("Low", "Medium", "High", "Unknown"))
plot_data$category <- factor(plot_data$category, levels = c("reference", "low", "medium", "high", "outlier"), labels = c("Reference", "Worse", "Average", "Better", "Outlier"))
plot_data$VACCINE <- factor(plot_data$VACCINE, levels = c("dtp", "hepb", "mmr"), labels = c("DTP", "HEP B", "MMR"))

# Plot all values for each location
locations <- unique(plot_data$ESTIAP)

rasterPlots = lapply(locations, function(g) {
  dt_subset <- plot_data %>% filter(ESTIAP==g)
  frames <- plot_data %>% filter(ESTIAP==g) %>% filter(eii>0)
  ggplot(dt_subset, aes(x=INCPOV1, y = RACEETHK_R)) +
    geom_tile(aes(fill = category), colour = "grey50") +
    geom_text(aes(label = round(change, 2))) + 
    facet_wrap(~VACCINE) +
    geom_tile(data=frames, size=1, fill=NA, colour="black") +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() +  
    labs(title=paste('Percent change in', g, "vaccination coverage"), y = 'Race/ethnicity of children', x = 'Family income', 
              subtitle=paste0('between 2007 and 2019'), caption = "Black outline is example of equitable improvement") +
    scale_y_discrete(limits=rev)
})

# Save file
outputFile07a <- paste0(visDir, "03_detailed_table_state_level_changes.PDF")
print(paste('Saving:', outputFile07a)) 
pdf(outputFile07a, height = 5.5, width = 9)
for(i in seq(length(rasterPlots))) { 
  print(rasterPlots[[i]])
}
dev.off()

# Create groupings based on how states compared to each other but also on which were most likely to show equitable improvement
dt7 <- plot_data %>%
  filter(category!="outlier") %>%
  filter(category!="reference") %>%
  group_by(ESTIAP) %>%
  mutate(freq.high = sum(category=="high"), freq.med = sum(category=="medium"), freq.low = sum(category=="low"), freq.out = sum(category=="outlier")) %>%
  ungroup %>%
  group_by(ESTIAP, category) %>%
  tally %>%
  mutate(pct=n/sum(n))

dt8 <- pivot_wider(dt7, id_cols = ESTIAP, names_from=category, values_from = pct)

high_group <- dt8 %>% filter(high >= .5) %>% select(ESTIAP, high, medium, low)
med_group <- dt8 %>% filter(medium >= .5) %>% select(ESTIAP, high, medium, low)
low_group <- dt8 %>% filter(low >= .5) %>% select(ESTIAP, high, medium, low)

# format the data file for plotting
dt7$category <- factor(dt7$category, levels = c("low", "medium", "high"), labels = c("Worse", "Average", "Better"))

# format the data file for plotting
data$RACEETHK_R <- factor(data$RACEETHK_R, levels=c("white", "hispa", "black", "other"), labels = c("White", "Hispanic", "Black", "Other"))
data$INCPOV1 <- factor(data$INCPOV1, levels=c("low", "med", "high", "miss"), labels = c("Low", "Medium", "High", "Unknown"))
data$VACCINE <- factor(data$VACCINE, levels = c("dtp", "hepb", "mmr"), labels = c("DTP", "HEP B", "MMR"))

# Plot time series for each location in the "high performing group" ------
high_locations <- unique(high_group$ESTIAP)

barPlotshigh =
    ggplot(dt7 %>% filter(ESTIAP %in% high_locations), aes(fill=category, x=ESTIAP, y=n, label=round(pct,2))) +
    geom_bar(position="fill", stat="identity") +
    geom_text(size =3, position = position_fill(vjust = .5)) +
    coord_flip() +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() + 
  labs(title=paste0('States/areas with high performance'), x="State/area", y="percent")

rasterPlotshigh = lapply(high_locations, function(g) {
  dt_subset <- plot_data %>% filter(ESTIAP==g)
  frames <- plot_data %>% filter(ESTIAP==g) %>% filter(eii>0)
  ggplot(dt_subset, aes(x=INCPOV1, y = RACEETHK_R)) +
    geom_tile(aes(fill = category), colour = "grey50") +
    geom_text(aes(label = round(change, 2))) + 
    facet_wrap(~VACCINE) +
    geom_tile(data=frames, size=1, fill=NA, colour="black") +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() +  
    labs(title=paste('Percent change in', g, "vaccination coverage"), y = 'Race/ethnicity of children', x = 'Family income', 
         subtitle=paste0('between 2007 and 2019'), caption = "Black outline is example of equitable improvement") +
    scale_y_discrete(limits=rev)
})

tsPlotshigh = lapply(high_locations, function(g) {
  data_subset <- data %>% filter(ESTIAP==g)
  ggplot(data_subset, aes(y = PredictedProb, x = YEAR, color = RACEETHK_R)) + 
    geom_line(size = 1, alpha = .8) + 
    labs(title = paste('Vaccine coverage in', g), y = 'Predicted probability of being vaccinated', x = 'Year',
         subtitle = "Stratified according to race/ethnicity and family income") +
    theme_minimal() +
    scale_x_continuous(breaks=c(2007, 2019)) +
    facet_grid(vars(INCPOV1), vars(VACCINE))
})

# Plot time series for each location in the "average-performing group" ----
med_locations <- unique(med_group$ESTIAP)

barPlotsmed = ggplot(dt7 %>% filter(ESTIAP %in% med_locations), aes(fill=category, x=ESTIAP, y=n, label=round(pct,2))) +
    geom_bar(position="fill", stat="identity") +
    geom_text(size =3, position = position_fill(vjust = .5)) +
    coord_flip() +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() + 
    labs(title=paste0('States/areas with average performance'), x="State/area", y="percent")

tsPlotsmed = lapply(med_locations, function(g) {
  data_subset <- data %>% filter(ESTIAP==g)
  ggplot(data_subset, aes(y = PredictedProb, x = YEAR, color = RACEETHK_R)) + 
    geom_line(size = 1, alpha = .8) + 
    labs(title = paste('Vaccine coverage in', g), y = 'Predicted probability of being vaccinated', x = 'Year',
         subtitle = "Stratified according to race/ethnicity and family income") +
    theme_minimal() +
    scale_x_continuous(breaks=c(2007, 2019)) +
    facet_grid(vars(INCPOV1), vars(VACCINE))
})

rasterPlotsmed = lapply(med_locations, function(g) {
  dt_subset <- plot_data %>% filter(ESTIAP==g)
  frames <- plot_data %>% filter(ESTIAP==g) %>% filter(eii>0)
  ggplot(dt_subset, aes(x=INCPOV1, y = RACEETHK_R)) +
    geom_tile(aes(fill = category), colour = "grey50") +
    geom_text(aes(label = round(change, 2))) + 
    facet_wrap(~VACCINE) +
    geom_tile(data=frames, size=1, fill=NA, colour="black") +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() +  
    labs(title=paste('Percent change in', g, "vaccination coverage"), y = 'Race/ethnicity of children', x = 'Family income', 
         subtitle=paste0('between 2007 and 2019'), caption = "Black outline is example of equitable improvement") +
    scale_y_discrete(limits=rev)
})

# Plot time series for each location in the "low-performing group"
low_locations <- unique(low_group$ESTIAP)

barPlotslow = ggplot(dt7 %>% filter(ESTIAP %in% low_locations), aes(fill=category, x=ESTIAP, y=n, label=round(pct,2))) +
    geom_bar(position="fill", stat="identity") +
    geom_text(size =3, position = position_fill(vjust = .5)) +
    coord_flip() +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() + 
    labs(title=paste0('States/areas with low performance'), x="State/area", y="percent")

rasterPlotslow = lapply(low_locations, function(g) {
  dt_subset <- plot_data %>% filter(ESTIAP==g)
  frames <- plot_data %>% filter(ESTIAP==g) %>% filter(eii>0)
  ggplot(dt_subset, aes(x=INCPOV1, y = RACEETHK_R)) +
    geom_tile(aes(fill = category), colour = "grey50") +
    geom_text(aes(label = round(change, 2))) + 
    facet_wrap(~VACCINE) +
    geom_tile(data=frames, size=1, fill=NA, colour="black") +
    scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
    theme_minimal() +  
    labs(title=paste('Percent change in', g, "vaccination coverage"), y = 'Race/ethnicity of children', x = 'Family income', 
         subtitle=paste0('between 2007 and 2019'), caption = "Black outline is example of equitable improvement") +
    scale_y_discrete(limits=rev)
})

tsPlotslow = lapply(low_locations, function(g) {
  data_subset <- data %>% filter(ESTIAP==g)
  ggplot(data_subset, aes(y = PredictedProb, x = YEAR, color = RACEETHK_R)) + 
    geom_line(size = 1, alpha = .8) + 
    labs(title = paste('Vaccine coverage in', g), y = 'Predicted probability of being vaccinated', x = 'Year',
         subtitle = "Stratified according to race/ethnicity and family income") +
    theme_minimal() +
    scale_x_continuous(breaks=c(2007, 2019)) +
    facet_grid(vars(INCPOV1), vars(VACCINE))
})

# save pdf files for high, medium, low locations
outputFile07_b <- paste0(visDir, "04_graphics_for_high_performing_areas.PDF")
outputFile07_c <- paste0(visDir, "05_graphics_for_typical_performing_areas.PDF")
outputFile07_d <- paste0(visDir, "06_graphics_for_low_performing_areas.PDF")

# Save files
pdf(outputFile07_b, height = 5.5, width = 9)
print(barPlotshigh)
for(i in seq(length(high_locations))) { 
  print(rasterPlotshigh[[i]])
  print(tsPlotshigh[[i]])
}
dev.off()

pdf(outputFile07_c, height = 5.5, width = 9)
print(barPlotsmed)
for(i in seq(length(med_locations))) { 
  print(rasterPlotsmed[[i]])
  print(tsPlotsmed[[i]])
}
dev.off()

pdf(outputFile07_d, height = 5.5, width = 9)
print(barPlotslow)
for(i in seq(length(low_locations))) { 
  print(rasterPlotslow[[i]])
  print(tsPlotslow[[i]])
}
dev.off()


    
  # facet_wrap(~VACCINE) +
  # geom_tile(aes(fill = rank)) + 
  

# 
# 
#    +
# 
#   geom_tile(data=frames, size=1, fill=NA, colour="black")

# reshape dt3 to look like dt2 but include the eii, then the eii could be used to shaed in the difference

# +
#   geom_tile(data=median, size=1, fill=NA, colour="black") +
#   labs(title=paste('Percent change in countries that saw above-average improvement'), y = 'Location', x = 'Vaccine', 
#        subtitle=paste0('between 2014 and 2019, among low-SDI countries'))
# medium_group <- dt8 %>% filter(medium >= 50) %>% select(ESTIAP, medium)
# low_group <- dt8 %>% filter(low >= 50) %>% select(ESTIAP, low)

# high_group <- dt8 %>% filter(high >= 50) %>% select(ESTIAP, high)
# high_group <- dt8[with(dt8, order(-high)), ] %>% select (ESTIAP)
# order.med <- dt8[with(dt8, order(-medium)), ] %>% select (ESTIAP)
# order.low <- dt8[with(dt8, order(-low)), ] %>% select (ESTIAP)
# 
# order.high <- dt8[with(dt8, order(-high)), ] %>% select (ESTIAP)
# order.med <- dt8[with(dt8, order(-medium)), ] %>% select (ESTIAP)
# order.low <- dt8[with(dt8, order(-low)), ] %>% select (ESTIAP)
# 
# dthigh$ESTIAP <- factor(dthigh$ESTIAP, levels = order.high$ESTIAP)
# dtmed$ESTIAP <- factor(dtmed$ESTIAP, levels = order.med$ESTIAP)
# dtlow$ESTIAP <- factor(dtlow$ESTIAP, levels = order.low$ESTIAP)
# dthigh$category <- factor(dthigh$category, levels = c("low", "medium", "high"))
# dtmed$category <- factor(dtmed$category, levels = c("medium", "low", "high"))
# dtlow$category <- factor(dtlow$category, levels = c("high", "medium", "low"))
# 
# 
# # count how many states were most likely to exceed the change within racial group and ethnicity and vaccine
# ggplot(plot_data %>% filter(ESTIAP %in% high_group), aes(fill=category, x=ESTIAP, y=n)) + 
#   geom_bar(position="fill", stat="identity") + coord_flip() +
#   # scale_y_reverse() +
#   scale_x_discrete(limits=rev) +
#   scale_fill_manual(values = c("#DADAEB", "#9E9AC8", "#6A51A3")) # low, med, high
# 
# ggplot(dtmed, aes(fill=category, x=ESTIAP, y=n)) + 
#   geom_bar(position="fill", stat="identity") + coord_flip() +
#   scale_y_reverse() +
#   scale_x_discrete(limits=rev) +
#   scale_fill_manual(values = c("#9E9AC8", "#DADAEB", "#6A51A3")) # medium, low, high
# 
# ggplot(dtlow, aes(fill=category, x=ESTIAP, y=n)) + 
#   geom_bar(position="fill", stat="identity") + coord_flip() +
#   scale_y_reverse() +
#   scale_x_discrete(limits=rev) + theme_minimal() +
#   scale_fill_manual(values = c("#DADAEB", "#9E9AC8", "#6A51A3")) # medium, low, high

# change the fill colors so they are consistent across graphs
# label the location names


# code the locations for plotting
# ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
# ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
#                 "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
#                 "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
# data$ESTIAP <- factor(data$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)
# 
# # make vector of all the locations
# lctns <- unique(data$ESTIAP)
# labelTable <- as.data.table(unique(data %>% select(ESTIAP, VACCINE)))
# 
# tsPlots = lapply(seq(length(lctns)), function(g) {
#   l = unique(labelTable[ESTIAP == lctns[[g]]]$ESTIAP)
#   ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = RACEETHK_R)) + 
#     # geom_ribbon(aes(ymin = LL,
#                     # ymax = UL, fill = VACCINE), alpha = 0.2) +
#     geom_line(size = 1, alpha = .8) + 
#     # facet_wrap(~INCOME) +
#     labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
#          subtitle = "Stratified according to race/ethnicity and family income") +
#     theme_minimal() +
#     facet_grid(vars(INCPOV1), vars(VACCINE))
# })



# plotdt6 <- 
# test <- dt6 %>% 
#   group_by(ESTIAP) %>% 
#   mutate(freq.high = sum(category=="high"), freq.med = sum(category=="medium"), freq.low = sum(category=="low")) %>% 
#            ungroup %>%
#   # Order by frequency of "high" category
#   arrange(desc(freq.high, freq.med, freq.low)) %>%
#   # Set ESTIAP factor order based on the sorting we just created
#   mutate(ESTIAP = factor(ESTIAP, levels=unique(ESTIAP))) %>%
#   group_by(ESTIAP, category) %>% 
#     tally %>%
#     mutate(pct=n/sum(n))
# 
# test$category <- factor(test$category, levels = c("high", "medium", "low")) 
# test$label <- test$ESTIAP
# 
# # label the locations
# labellevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
# labellabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
#                 "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
#                 "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
# test$label <- factor(test$label, levels=labellevels, labels=labellabels)
# ggplot(test, aes(fill=category, y=n, x=ESTIAP)) + 
#   geom_bar(position="fill", stat="identity") +
#   coord_flip() +
#   # scale_x_discrete(labels=test$label)
# 
# ggplot(test, aes(x = ESTIAP, y = pct, fill = category)) + 
#   # geom_col() + coord_flip() +


# %>% 
#   ungroup %>%
#   # Order by frequency of "Swelling 1"
#   arrange(desc(freq.high)) %>% 
#   # Set ESTIAP factor order based on the sorting we just created
#   mutate(ESTIAP = factor(ESTIAP, levels=unique(ESTIAP))) %>% 
#   # Get percents for each bar segment
#   group_by(ESTIAP, category) %>% 
#   tally %>% 
#   mutate(pct=n/sum(n))

# dt7 <-
#   plotdt6 %>% 
#   filter(category=="high") %>% 
#   arrange(desc(pct))

# df2$Genotype <- factor(df2$Genotype, levels=df3$Genotype)

# ggplot(plot_data, aes(x=ESTIAP, y = eii, color=vaccine)) +
#   geom_bar(position="dodge", stat="identity") +
#   facet_grid(vars(income), vars(race))


  
# ggplot(data=dt6, aes(x=category, y=category)) + geom_bar(stat="fill")

# reshape the dt4 to facilitate the merge
# dt5 <- dt4 %>% 
# # create new columns that will help group each variable
# dt4$group_hispa_high_dtp <- NA_character_
# dt4$group_hispa_med_dtp <- NA_character_
# dt4$group_hispa_low_dtp <- NA_character_
# dt4$group_hispa_miss_dtp <- NA_character_
# dt4$group_hispa_high_mmr <- NA_character_
# dt4$group_hispa_med_mmr <- NA_character_
# dt4$group_hispa_low_mmr <- NA_character_
# dt4$group_hispa_miss_mmr <- NA_character_
# dt4$group_hispa_high_hepb <- NA_character_
# dt4$group_hispa_med_hepb <- NA_character_
# dt4$group_hispa_low_hepb <- NA_character_
# dt4$group_hispa_miss_hepb <- NA_character_
# 
# dt4$group_black_high_dtp <- NA_character_
# dt4$group_black_med_dtp <- NA_character_
# dt4$group_black_low_dtp <- NA_character_
# dt4$group_black_miss_dtp <- NA_character_
# dt4$group_black_high_mmr <- NA_character_
# dt4$group_black_med_mmr <- NA_character_
# dt4$group_black_low_mmr <- NA_character_
# dt4$group_black_miss_mmr <- NA_character_
# dt4$group_black_high_hepb <- NA_character_
# dt4$group_black_med_hepb <- NA_character_
# dt4$group_black_low_hepb <- NA_character_
# dt4$group_black_miss_hepb <- NA_character_
# 
# dt4$group_other_high_dtp <- NA_character_
# dt4$group_other_med_dtp <- NA_character_
# dt4$group_other_low_dtp <- NA_character_
# dt4$group_other_miss_dtp <- NA_character_
# dt4$group_other_high_mmr <- NA_character_
# dt4$group_other_med_mmr <- NA_character_
# dt4$group_other_low_mmr <- NA_character_
# dt4$group_other_miss_mmr <- NA_character_
# dt4$group_other_high_hepb <- NA_character_
# dt4$group_other_med_hepb <- NA_character_
# dt4$group_other_low_hepb <- NA_character_
# dt4$group_other_miss_hepb <- NA_character_

# quantiles$new_variable <- quantiles$group
# quantiles$new_variable <- gsub("eii", "group", quantiles$new_variable)
# quantiles <- as.data.table(quantiles)

# dt4 %>% mutate(
#   
# )

# create a loop that will group each variable
# groupVars <- names(dt4)[38:73]
# 
# i <- 1
# for (i in 1:nrow(dt4)){
#   v <- numVars[i]
#   g <- groupVars[i]
#   dt4 %>% select(g, v) %>% filter(v==quantiles[group==v]) <- 
#   dt4[i, get(g)] <- ifelse(dt4[i ,get(v)] < quantiles[group==v]$first_q, "low", ifelse(dt4[i, get(v)] > quantiles[group==v]$third_q, "high", ifelse(is.na(dt4[i ,get(v)]), NA_character_, "medium")))
#   rm(g)
# }
# # g <- groupVars[1]
# for (r in 1:nrow(dt4)){
#   for (c in 38:73){
#     dt4[r,c] <- ifelse(dt4[,get(v)][r] < quantiles[group==v]$first_q, "low", ifelse(dt4[,get(v)][r] > quantiles[group==v]$third_q, "high", ifelse(is.na(dt4[,get(v)][r]), NA_character_, "medium")))
#   }
# }
# for (v in newVars){
#   data[is.na(get(v)), (v):=0]
# }

# 
# # separate the data into two separate files
# dt_groups <- dt4 %>% select(c(ESTIAP), starts_with("group"))
# dt_eii <- dt4 %>% select(c(ESTIAP), starts_with("eii"))

# # reshape each dataset
# dt_groups <- dt_groups %>%
#   pivot_longer(
#     !c(ESTIAP),
#     names_to = c("group"),
#     values_to = "category",
#     values_drop_na = FALSE) %>% 
#   separate(group, into = c("variable", "race", "income", "vaccine"))

# dt_eii <- dt_eii %>%
#   pivot_longer(
#     !c(ESTIAP),
#     names_to = c("group"),
#     values_to = "eii",
#     values_drop_na = FALSE) %>% 
#   separate(group, into = c("variable", "race", "income", "vaccine"))

# drop unnecessary columns
# dt_groups <- dt_groups %>% select(-c(variable))
# dt_eii <- dt_eii %>% select(-c(variable))

# merge two datasets into one
# dt5 <- dt_eii %>% inner_join(dt_groups, by=c("ESTIAP", "race", "income", "vaccine"))


# create visuals and tables to explore the data

# format the location variable
# ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
# ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
#                 "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
#                 "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
# dt5$ESTIAP <- factor(dt5$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)
# dt5[category=="high", .N, by = .(ESTIAP, vaccine)]
# dt5[category=="medium", .N, by = .(ESTIAP, vaccine)]
# dt5[category=="low", .N, by = .(ESTIAP, vaccine)]

# subset data to high-performing locations
# plot.data <- dt5 %>%
#   group_by(ESTIAP, VACCINE) %>%
#   dplyr::summarize(.N))


# ggplot(data = dt5, aes(x = outlier, y = N)) +
#   geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
#   coord_flip() +
#   geom_text(aes(label=N), hjust=1.6, color="white") +
#   theme_minimal(base_size = 12) + 
#   labs(title = paste('Countries that exceeded median improvement in vaccine coverage'), 
#        y = 'Number of vaccines that saw greater-than-expected improvement', 
#        x = 'Location', 
#        subtitle = paste0('between 2014 and 2019, among low-SDI countries'))
# 
# dt5 <- as.data.table(dt5)
# 
# high.per.data <- dt5 %>% filter(category=="high")
# 
# ggplot(dt6, aes(x=income, , color=ESTIAP)) +
#   geom_point(alpha=0.5) +
#   
#   geom_tile(aes(fill = eii)) +
#   geom_text(aes(label = round(eii, 2))) +
#   scale_fill_gradient(low="white", high= "steelblue") +
#   facet_grid(vars(income), vars(race))
#   
# 
# # create raster plot of locations
# j <- ggplot(dt3, aes(vaccine_name, location_name)) +
#   geom_tile(aes(fill = percent_change)) + 
#   geom_text(aes(label = round(percent_change, 2))) +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   geom_tile(data=median, size=1, fill=NA, colour="black") +
#   labs(title=paste('Percent change in countries that saw above-average improvement'), y = 'Location', x = 'Vaccine', 
#        subtitle=paste0('between 2014 and 2019, among low-SDI countries'))
# create tables described in DAP

# dobyearData <- dobyearData %>%
#   pivot_longer(
#     !c(caseid, v009, v010, v011, v000, v005, v007, v006, v016, sstate),
#     names_to = c("variable", "child"),
#     names_sep = "_",
#     values_to = "birth_year",
#     values_drop_na = FALSE
#   )

# reshape the data


# 
# dt4[, get(g)] <- ifelse(dt4[,get(v)][1]<quantiles[group==v]$first_q, "low", ifelse(dt4[,get(v)][1]>quantiles[group==v]$third_q, "high", "medium"))
# for (v in numVars){
#   ifelse()
#   #   min = norm_cut$min[i]
#   #   max = norm_cut$max[i]
#   #   v = norm_cut$variable[i]
#   #   final_data[, (v):=(get(v)-min)/(max-min)]
#   dt4[, (v):=get(v):=(get(v)-min)/(max)]
# }
  # create loop to calculate the groups for each s
# i <- 1
# for (i in 1:length(normVars)) {
#   min = norm_cut$min[i]
#   max = norm_cut$max[i]
#   v = norm_cut$variable[i]
#   final_data[, (v):=(get(v)-min)/(max-min)]
# }
# calculate the three quintile groups for each vaccine and for each income group
# quantile_table_hispanic <- dt4 %>% 
#   group_by(INCPOV1, VACCINE) %>%
#   dplyr::summarize(quant25 = quantile(eii_2, na.rm=TRUE, probs = .25),
#                    quant75 = quantile(eii_2, na.rm=TRUE, probs = .75))
# quantile_table_black <- dt4 %>% 
#   group_by(INCPOV1, VACCINE) %>%
#   dplyr::summarize(quant25 = quantile(eii_3, na.rm=TRUE, probs = .25),
#                    quant75 = quantile(eii_3, na.rm=TRUE, probs = .75))
# quantile_table_other <- dt4 %>% 
#   group_by(INCPOV1, VACCINE) %>%
#   dplyr::summarize(quant25 = quantile(eii_4, na.rm=TRUE, probs = .25),
#                    quant75 = quantile(eii_4, na.rm=TRUE, probs = .75))

# select columns of interest
# dt4 <- dt4 %>% select(ESTIAP, INCPOV1, VACCINE, eii_2, eii_3, eii_4)

# pivot dataset longer
# dt5 <- pivot_longer(dt4, cols = c(eii_2, eii_3, eii_4), names_to = "RACEETHK_R")





# calculate risk ratios
# data_final$dtp_rr_2 <- (data_final$)

# eth_vac_cov_final$eii_dtp_1 <- (eth_vac_cov_final$change_dtp_1 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
# eth_vac_cov_final$eii_dtp_3 <- (eth_vac_cov_final$change_dtp_3 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
# eth_vac_cov_final$eii_dtp_4 <- (eth_vac_cov_final$change_dtp_4 - eth_vac_cov_final$change_dtp_2)/eth_vac_cov_final$change_dtp_2*100
# 
# eth_vac_cov_final$eii_mmr_1 <- (eth_vac_cov_final$change_mmr_1 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
# eth_vac_cov_final$eii_mmr_3 <- (eth_vac_cov_final$change_mmr_3 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
# eth_vac_cov_final$eii_mmr_4 <- (eth_vac_cov_final$change_mmr_4 - eth_vac_cov_final$change_mmr_2)/eth_vac_cov_final$change_mmr_2*100
# 
# eth_vac_cov_final$eii_hep_1 <- (eth_vac_cov_final$change_hep_1 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
# eth_vac_cov_final$eii_hep_3 <- (eth_vac_cov_final$change_hep_3 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
# eth_vac_cov_final$eii_hep_4 <- (eth_vac_cov_final$change_hep_4 - eth_vac_cov_final$change_hep_2)/eth_vac_cov_final$change_hep_2*100
# # for each vaccine and racial/ethnic group calculate the equitable improvement index: which is the relative difference between the two values
# data_wide$dtp_2007

# remove outliers
# from each of the values of interest remove the outliers using the following formula

# create a function to spot outliers in data
# is_outlier <- function(x) {
#   return(x > quantile(x, 0.75) + 1.5 * IQR(x))
# }



# # calculate the three quintile groups for each vaccine and for each income group
# data_final %>% 
#   group_by(INCPOV1) %>%
#   dplyr::summarize(quant25 = quantile(eii_dtp_2, probs = .25),
#             quant75 = quantile(eii_dtp_2, probs = .75))
# 
# data_final <- as.data.table(data_final)
# data_final[, lapply(.SD, quantile, prob = c(0.25, 0.75), na.rm=TRUE), .SDcols = c('eii_dtp_2')]
# # change income to a non-factored variable
# out <- dt.data[ ,lapply(.SD, quantile, prob = c(.05, .25, .5, .75, .95), na.rm = TRUE),  .SDcols = grep("V", names(dt.data), value = TRUE)]
# data_final %>% 
#   mutate(eii_dtp_2_test = 
#            case_when(is_outlier(eii_dtp_2)==TRUE ~ NA,
#                      is_outlier(eii_dtp_2)==FALSE ~ eii_dtp_2))
# 
# data_final[,outlier:=ifelse(is_outlier(eii_dtp_2), eii_dtp, as.double(NA))]
# dt[,outlier:=ifelse(is_outlier(percent_change), location_name, as.double(NA)), by = .(vaccine_name),
#    TRUE]
# 

# create tables identified in data analysis plan

# 1: Quartile identification according to equitable
# 2
# 3
# 4

# save figures and tables


# vaccines <- unique(data$VACCINE)
# 
# for (i in 1:length(vaccines)) {
#   g <- ggplot(data %>% filter(VACCINE==vaccines[i]), aes(YEAR, PredictedProb, group=factor(ESTIAP))) + 
#     geom_line(aes(color=ESTIAP), show.legend = FALSE) +
#     facet_grid(vars(INCPOV1), vars(RACEETHK_R)) +
#     labs(title = paste0(unique(vaccines[i]), " vaccination trends between 2007 and 2019"),
#          subtitle = "Stratified according to race/ethnicity and family income") +
#     ylab('Predicted probability of being vaccinated') +
#     xlab('Year') +
#     theme_minimal()
#   print(g)
# }
# 
# # make vector of all the locations
# lctns <- unique(data$ESTIAP)
# labelTable <- as.data.table(unique(data %>% select(ESTIAP, VACCINE)))
# 
# tsPlots = lapply(seq(length(lctns)), function(g) {
#   l = unique(labelTable[ESTIAP == lctns[[g]]]$ESTIAP)
#   ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = VACCINE)) + 
#     geom_ribbon(aes(ymin = LL,
#                     ymax = UL, fill = VACCINE), alpha = 0.2) +
#     geom_line(size = 1, alpha = .8) + 
#     facet_wrap(~INCOME) +
#     labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
#          subtitle = "Stratified according to race/ethnicity and family income") +
#     theme_minimal() +
#     facet_grid(vars(INCPOV1), vars(RACEETHK_R))
# })
# 
# # make vector of all the locations
# lctns <- unique(data$ESTIAP)
# labelTable <- as.data.table(unique(data %>% select(ESTIAP, VACCINE)))
# 
# tsPlots = lapply(seq(length(lctns)), function(g) {
#   l = unique(labelTable[ESTIAP == lctns[[g]]]$ESTIAP)
#   ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = VACCINE)) + 
#     geom_ribbon(aes(ymin = LL,
#                     ymax = UL, fill = VACCINE), alpha = 0.2) +
#     geom_line(size = 1, alpha = .8) + 
#     facet_wrap(~INCOME) +
#     labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
#          subtitle = "Stratified according to race/ethnicity and family income") +
#     theme_minimal() +
#     facet_grid(vars(INCPOV1), vars(RACEETHK_R))
# })
# 
# # Save file
# print(paste('Saving:', outputFile05_02)) 
# pdf(outputFile07, height = 8.5, width = 11)
# for(i in seq(length(tsPlots))) { 
#   print(tsPlots[[i]])
# }
# dev.off()


# create tables identified in data analysis plan

# 1: Quartile identification according to equitable
# 2
# 3
# 4

# save figures and tables

# Calculate equitable immunization improvement
# data_final$eii_dtp_2 <- ((data_final$dtp_change_2 - data_final$dtp_change_1)/data_final$dtp_change_1)*100
# data_final$eii_dtp_3 <- ((data_final$dtp_change_3 - data_final$dtp_change_1)/data_final$dtp_change_1)*100
# data_final$eii_dtp_4 <- ((data_final$dtp_change_4 - data_final$dtp_change_1)/data_final$dtp_change_1)*100
# 
# data_final$eii_mmr_2 <- ((data_final$mmr_change_2 - data_final$mmr_change_1)/data_final$mmr_change_1)*100
# data_final$eii_mmr_3 <- ((data_final$mmr_change_3 - data_final$mmr_change_1)/data_final$mmr_change_1)*100
# data_final$eii_mmr_4 <- ((data_final$mmr_change_4 - data_final$mmr_change_1)/data_final$mmr_change_1)*100
# 
# data_final$eii_hep_2 <- ((data_final$hep_change_2 - data_final$hep_change_1)/data_final$hep_change_1)*100
# data_final$eii_hep_3 <- ((data_final$hep_change_3 - data_final$hep_change_1)/data_final$hep_change_1)*100
# data_final$eii_hep_4 <- ((data_final$hep_change_4 - data_final$hep_change_1)/data_final$hep_change_1)*100
# data_wide$dtp_change <- ((data_wide$dtp_2019-data_wide$dtp_2007)/data_wide$dtp_2007)*100
# data_wide$mmr_change <- ((data_wide$mmr_2019-data_wide$mmr_2007)/data_wide$mmr_2007)*100
# data_wide$hep_change <- ((data_wide$hepb_2019-data_wide$hepb_2007)/data_wide$hepb_2007)*100

# data_final$eii_2
# data_final$eii_dtp_2 <- (data_final$dtp_change_2 / data_final$dtp_change_1) #/data_final$dtp_change_1*100
# data_final$eii_dtp_3 <- (data_final$dtp_change_3 / data_final$dtp_change_1) #/data_final$dtp_change_1*100
# data_final$eii_dtp_4 <- (data_final$dtp_change_4 / data_final$dtp_change_1) #/data_final$dtp_change_1*100
# 
# data_final$eii_mmr_2 <- (data_final$mmr_change_2 / data_final$mmr_change_1) #/data_final$mmr_change_1*100
# data_final$eii_mmr_3 <- (data_final$mmr_change_3 / data_final$mmr_change_1) #/data_final$mmr_change_1*100
# data_final$eii_mmr_4 <- (data_final$mmr_change_4 / data_final$mmr_change_1) #/data_final$mmr_change_1*100
# 
# data_final$eii_hep_2 <- (data_final$hep_change_2 / data_final$hep_change_1) #/data_final$hep_change_1*100
# data_final$eii_hep_3 <- (data_final$hep_change_3 / data_final$hep_change_1) #/data_final$hep_change_1*100
# data_final$eii_hep_4 <- (data_final$hep_change_4 / data_final$hep_change_1) #/data_final$hep_change_1*100

# data_final$eii_2 <- (data_final$`2` / data_final$`1`)
# data_final$eii_3 <- (data_final$`3` / data_final$`1`)
# data_final$eii_4 <- (data_final$`4` / data_final$`1`)

# plotdt6 <- dt6 %>% group_by(ESTIAP, category) %>%
#   # tally()

# levels(plotdt6$ESTIAP)


# create figures to compare states
# ggplot(plotdt6, aes(fill=category, y =n, x=ESTIAP)) + geom_bar(position="fill", stat="identity")




# df2$Genotype <- factor(df2$Genotype, levels=df3$Genotype)

# Calculate equitable immunization improvement
# dt3$eii_hispa <- ((dt3$hispa - dt3$white)/dt3$white)*100
# dt3$eii_black <- ((dt3$black - dt3$white)/dt3$white)*100
# dt3$eii_other <- ((dt3$other - dt3$white)/dt3$white)*100

# dt4 <- dt3 %>% select(ESTIAP, INCPOV1, VACCINE, eii_hispa, eii_black, eii_other)
# dt4 <- dt3 %>% select(ESTIAP, INCPOV1, VACCINE, eii_hispa, eii_black, eii_black)


# ggplot(dt3, aes(x=eii_hispa)

# drop locations that are outliers
# dt6 <- dt6 %>% filter(!is.na(eii))

# dt7 <- dt6 %>% 
#   group_by(ESTIAP) %>% 
#   mutate(freq.high = sum(category=="high"), freq.med = sum(category=="medium"), freq.low = sum(category=="low")) %>% 
#   ungroup %>% 
#   group_by(ESTIAP, category) %>% 
#   tally %>%
#   mutate(pct=n/sum(n))

# dt7$category <- factor(dt7$category, levels = c("low", "medium", "high"))
# dt7$pct <- dt7$pct*100
# 
# # format the location variable
# ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
# ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
#                 "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
#                 "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
# dt7$ESTIAP <- factor(dt7$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)
# 
# # 
# # dt7 <- dt7 %>%
# #   group_by(ESTIAP) %>%
# #   mutate(label_y = cumsum(n) - 0.5 * n)
# 
# ggplot(dt7 %>% filter(ESTIAP %in% high_group$ESTIAP), aes(fill=category, x=ESTIAP, y=n, label=pct)) + 
#   geom_bar(position="fill", stat="identity") +
#   geom_text(size =3, position = position_fill(vjust = .5)) + 
#   coord_flip() + 
#   theme_minimal()
# 
# ggplot(dt7 %>% filter(ESTIAP %in% low_group$ESTIAP), aes(fill=category, x=ESTIAP, y=n, label=pct)) + 
#   geom_bar(position="fill", stat="identity") +
#   geom_text(size =3, position = position_fill(vjust = .5)) + 
#   coord_flip() + 
#   theme_minimal()
# 
# ggplot(dt7 %>% filter(ESTIAP %in% medium_group$ESTIAP), aes(fill=category, x=ESTIAP, y=n, label=pct)) + 
#   geom_bar(position="fill", stat="identity") +
#   geom_text(size =3, position = position_fill(vjust = .5)) + 
#   coord_flip() + 
#   theme_minimal()


# ggplot
# +
#   # geom_col() +
#   coord_flip() +
#   geom_text(aes(label=pct), position = position_stack()) +
#   theme_minimal()


# scale_y_reverse() +
# scale_x_discrete(limits=rev)

# dthigh <- dt6 %>% 
#   group_by(ESTIAP) %>% 
#   mutate(freq.high = sum(category=="high"), freq.med = sum(category=="medium"), freq.low = sum(category=="low")) %>% 
#   ungroup %>% 
#   group_by(ESTIAP, category) %>% 
#   tally %>%
#   mutate(pct=n/sum(n))
# 
# dtlow <- dthigh
# dtmed <- dthigh


# this is a promising figure work on this next
# high_group <- plot_data[with(dt8, order(-high)), ] %>% select (ESTIAP)
# order.med <- dt8[with(dt8, order(-medium)), ] %>% select (ESTIAP)
# order.low <- dt8[with(dt8, order(-low)), ] %>% select (ESTIAP)
