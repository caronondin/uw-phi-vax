# Author: Francisco Rios Casas
# Purpose: Load and prep UN immigrant rate (net immigrants minus emmigrants)
# Date: November 09, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[6], "/", file_list$data_source[6], "/", file_list$file_name[6])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 1)

# rename columns of data
name_row <- 12
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Remove unnecessary columns and rows
dt1 <- dt1[-c(1:12),-c(1, 2, 4)]

# Subset rows
# Remove the unnecessary location divisions
dt1 <- dt1 %>% filter(Type=="Country/Area")

# assign correct names to columns
colnames(dt1)[1] <- "country"
colnames(dt1)[2] <- "iso_num_code"
colnames(dt1)[4] <- "parent_iso_num_code"

# count how many characters are in the ISO num code column
# dt1$nchar <- nchar(dt1$iso_num_code)

#24, 72, 12, 51, 31, 48, 4, 50, 64, 96, 28, 44, 52, 84, 32, 68, 76, 36, 90, 8, 70, 40, 56
# Recode the ISO values that were incorrectly read
dt1$iso_num_code[which(dt1$iso_num_code=="24")] <- "024"
dt1$iso_num_code[which(dt1$iso_num_code=="12")] <- "012"
dt1$iso_num_code[which(dt1$iso_num_code=="72")] <- "012"
dt1$iso_num_code[which(dt1$iso_num_code=="96")] <- "096"
dt1$iso_num_code[which(dt1$iso_num_code=="4")] <-  "004"
dt1$iso_num_code[which(dt1$iso_num_code=="50")] <- "050"
dt1$iso_num_code[which(dt1$iso_num_code=="64")] <- "064"
dt1$iso_num_code[which(dt1$iso_num_code=="51")] <- "051"
dt1$iso_num_code[which(dt1$iso_num_code=="31")] <- "031"
dt1$iso_num_code[which(dt1$iso_num_code=="48")] <- "048"

dt1$iso_num_code[which(dt1$iso_num_code=="8")] <-  "008"
dt1$iso_num_code[which(dt1$iso_num_code=="20")] <- "020"
dt1$iso_num_code[which(dt1$iso_num_code=="70")] <- "070"
dt1$iso_num_code[which(dt1$iso_num_code=="40")] <- "040"
dt1$iso_num_code[which(dt1$iso_num_code=="56")] <- "056"
dt1$iso_num_code[which(dt1$iso_num_code=="28")] <- "028"
dt1$iso_num_code[which(dt1$iso_num_code=="44")] <- "044"
dt1$iso_num_code[which(dt1$iso_num_code=="52")] <- "052"
dt1$iso_num_code[which(dt1$iso_num_code=="92")] <- "092"
dt1$iso_num_code[which(dt1$iso_num_code=="84")] <- "084"

dt1$iso_num_code[which(dt1$iso_num_code=="32")] <- "032"
dt1$iso_num_code[which(dt1$iso_num_code=="68")] <- "068"
dt1$iso_num_code[which(dt1$iso_num_code=="76")] <- "076"
dt1$iso_num_code[which(dt1$iso_num_code=="60")] <- "060"
dt1$iso_num_code[which(dt1$iso_num_code=="36")] <- "036"
dt1$iso_num_code[which(dt1$iso_num_code=="90")] <- "090"
dt1$iso_num_code[which(dt1$iso_num_code=="16")] <- "016"

# Reshape the data
dt2 <- data.table(dt1)

prepped_migr_rate_dataset <- melt(dt2, id.vars=c("country", "iso_num_code", "Type", "parent_iso_num_code"),
                                  value.name = "mig_rate", 
                                  variable.name = "year_range")

# subset columns
prepped_migr_rate_dataset <- prepped_migr_rate_dataset %>% select(country, iso_num_code, year_range, mig_rate)

# Reformat variable
prepped_migr_rate_dataset$mig_rate <- as.numeric(prepped_migr_rate_dataset$mig_rate)

# Load location codebook to standardize names
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge location map onto the data
prepped_migr_rate_dataset <- prepped_migr_rate_dataset %>%
  inner_join(location_map, by="iso_num_code")

# Keep columns of interest
prepped_migr_rate_dataset <- prepped_migr_rate_dataset %>% select(location, year_range, gbd_location_id, iso_code, iso_num_code, mig_rate)

# Save file in the prepped data folder
saveRDS(prepped_migr_rate_dataset, file = paste0(prepped_data_dir, "aim_2/06_prepped_un_net_migr_rate_data.RDS"))
