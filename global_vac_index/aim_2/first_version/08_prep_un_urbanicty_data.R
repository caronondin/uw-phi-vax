# Author: Francisco Rios Casas
# Purpose: Load and prep UN data on proportion of population living in urban areas
# Date: July 12, 2022

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[7], "/", file_list$data_source[7], "/", file_list$file_name[7])

# Read data sheet
dt1 <- read_xls(file_path, sheet = 1)

# rename columns of data
name_row <- 12
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Remove unnecessary columns and rows
dt1 <- dt1[-c(1:12),-c(1, 3)]

# assign correct names to columns
colnames(dt1)[1] <- "country"
colnames(dt1)[2] <- "iso_num_code"

# change value format for the year 1950 to numeric to match other year values
dt1$`1950` <- as.numeric(dt1$`1950`)

# count how many characters are in the ISO num code column
# dt1$nchar <- nchar(dt1$iso_num_code)
# 24, 12, 72, 4, 50, 64, 96, 51, 31, 48, 8, 20, 70, 
# 40, 56, 28, 44, 52, 92, 84, 32, 68, 76, 60, 36, 90, 16


# Recode the ISO values that were incorrectly read
dt1$iso_num_code[which(dt1$iso_num_code=="24")] <- "024"
dt1$iso_num_code[which(dt1$iso_num_code=="12")] <- "012"
dt1$iso_num_code[which(dt1$iso_num_code=="72")] <- "072"
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
prepped_urban_dataset <- melt(dt2, id.vars=c("country", "iso_num_code"),
                                  value.name = "perc_urban", 
                                  variable.name = "year_factor")

# Extract year variable from the data set
prepped_urban_dataset$year <- sub('.*(\\d{4}).*', '\\1', prepped_urban_dataset$year_factor)

# Change format of data
prepped_urban_dataset$year <- as.numeric(prepped_urban_dataset$year)

# Load location codebook to standardize names
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge location map onto the data
prepped_urban_dataset <- prepped_urban_dataset %>%
  inner_join(location_map, by="iso_num_code")

# Keep columns of interest
prepped_urban_dataset <- prepped_urban_dataset %>% 
  select(location, year, gbd_location_id, iso_code, iso_num_code, perc_urban) %>%
  filter(year >=1990 & year <=2020)

# Divide urban dataset by 100
prepped_urban_dataset$perc_urban <- prepped_urban_dataset$perc_urban/100

# Save file in the prepped data folder
saveRDS(prepped_urban_dataset, file = paste0(prepped_data_dir, "aim_2/07_prepped_un_perc_urban_data.RDS"))
