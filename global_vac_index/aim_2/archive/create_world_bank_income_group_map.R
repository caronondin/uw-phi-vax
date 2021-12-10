# Author: Francisco Rios Casas
# Purpose: Prep list of high-income countries per world bank for use in analyses
# Date: December 07, 2021

# List of world bank income groups
wb_list <- read_xlsx(paste0(codebook_directory, "CLASS.xlsx"))

# Save columns of interest
wb_list <- wb_list[,c(1:2, 4)]

# rename columns
names(wb_list)[1] <- "country"
names(wb_list)[2] <- "iso_code"
names(wb_list)[3] <- "income_group"

# reshape data to tidy data
wb_list$high_income_group <- 0
wb_list$low_income_group <- 0
wb_list$lmi_group <- 0
wb_list$umi_group <- 0

wb_list$high_income_group[which(wb_list$income_group=="High income")] <-1
wb_list$low_income_group[which(wb_list$income_group=="Low income")] <-1
wb_list$lmi_group[which(wb_list$income_group=="Lower middle income")] <-1
wb_list$umi_group[which(wb_list$income_group=="Upper middle income")] <-1

# Remove blank rows
wb_list <- wb_list %>% filter(!is.na(income_group))

# load location map
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# merge onto standardized names in location map
wb_list <- wb_list %>% left_join(location_map, by="iso_code")

# Keep certain variables
wb_list <- wb_list %>% select(location, iso_code, iso_num_code, gbd_location_id, 
                              income_group, high_income_group, low_income_group,
                              lmi_group, umi_group) %>%
  filter(!is.na(location))

# Save final data
write.csv(wb_list, file=paste0(codebook_directory, "world_bank_income_groups.csv"))
saveRDS(wb_list, file=paste0(codebook_directory, "world_bank_income_groups.RDS"))
