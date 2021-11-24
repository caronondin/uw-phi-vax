# Author: Francisco Rios Casas
# Purpose: Load and prep UN skilled attendant data
# Date: November 08, 2021

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
names(dt1)[val_col] <- "perc_skil_attend"

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

birth_attendant_data <- dt2 %>% 
  left_join(location_map, by='iso_code')

# Keep only columns of interest
birth_attendant_data <- birth_attendant_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, perc_skil_attend)

# Reformat columns
birth_attendant_data$year <- as.numeric(birth_attendant_data$year)

# save the file on the prepped data folder
saveRDS(birth_attendant_data, file = paste0(prepped_data_dir, "aim_2/04_prepped_un_birth_attendant_data.RDS"))
