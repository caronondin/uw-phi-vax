# Author: Francisco Rios Casas
# Purpose: Load and prep UN skilled attendant data
# Date: July 12, 2022

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[4], "/", file_list$data_source[4], "/", file_list$file_name[4])

# Load data file
dt1 <- read.csv(file_path)

# subset columns
loc_col <- 2
year_col <- 5
val_col <- 6

# rename
names(dt1)[loc_col] <- "geographic_area"
names(dt1)[year_col] <- "time_period"
names(dt1)[val_col] <- "perc_skill_attend"

# keep only relevant columns
total_subset <- c(loc_col, year_col, val_col)
dt1 <- dt1[, total_subset]

# split variable columns
dt1 <- as_tibble(dt1)
dt2 <- dt1 %>% separate(geographic_area, c("iso_code", "geographic_area"), sep = ":")
dt2$geographic_area <- str_trim(dt2$geographic_area, side="left")

# clean years that have month variable
# Extract four consecutive numeric digits from the year variable
dt2$year <- sub('.*(\\d{4}).*', '\\1', dt2$time_period)

# Merge location map to standardize names of countries
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

dt2 <- dt2 %>% 
  right_join(location_map, by='iso_code')

# Keep only columns of interest
dt2 <- dt2 %>% select(location, year, gbd_location_id, iso_code, iso_num_code, perc_skill_attend)

# Reformat columns
dt2$year <- as.numeric(dt2$year)

# There is a second data source with additional location information
# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path2 <- paste0(raw_data_dir, file_list$data_type[14], "/", file_list$data_source[14], "/", file_list$containing_folder[14], "/", file_list$file_name[14])

# Load data file
headers <- read.csv(file_path2, skip = 4, header = F, nrows = 1, as.is = T)
dt3 <- read.csv(file_path2, skip = 5, header = F)
colnames(dt3) <- headers

# drop extra columns
dt3 <- dt3[,-c(3, 4, 66)]

# reshape data
dt3 <- pivot_longer(dt3, c(3:63), names_to = "year", values_to ="perc_skill_attend")

# rename
names(dt3)[1] <- "geographic_area"
names(dt3)[2] <- "iso_code"
names(dt3)[3] <- "year"

# Merge location map to standardize names of countries
# location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

dt3 <- dt3 %>% 
  right_join(location_map, by='iso_code')

# Keep only columns of interest
dt3 <- dt3 %>% select(location, year, gbd_location_id, iso_code, iso_num_code, perc_skill_attend)

# remove the missing values
dt3 <- dt3 %>% filter(!is.na(perc_skill_attend))

# Reformat columns
dt3$year <- as.numeric(dt3$year)

# check if there are any values in the dt3 dataset missing from the other one
missing_locs_subset <- unique(dt2 %>% filter(is.na(perc_skill_attend)) %>% select(location))
missing.locs <- missing_locs_subset$location

# find missing values and locations
dt3 <- dt3 %>% filter(location %in% missing.locs)

# bind additional locations to full dataset
birth_attendant_data <- bind_rows(dt2, dt3)

# drop locations without any values
birth_attendant_data <- birth_attendant_data %>% filter(!is.na(perc_skill_attend))

# save the file on the prepped data folder
saveRDS(birth_attendant_data, file = paste0(prepped_data_dir, "aim_2/05_prepped_un_birth_attendant_data.RDS"))
