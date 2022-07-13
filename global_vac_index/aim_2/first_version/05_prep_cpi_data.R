# Author: Francisco Rios Casas
# Purpose: Load and prep Corruptions Perceptions Index
# Date: July 12, 2022

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[3], "/", file_list$data_source[3], "/", file_list$containing_folder[3], "/", file_list$file_name[3])

# Load data file
dt1 <- data.table(read_xlsx(file_path, sheet = 2))

# Rename data headers
name_row <- 2
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Identify columns of interest
loc_col <- grepl("Country", names)
iso_col <- grepl("ISO3", names)
cpi_col <- grepl("CPI ", names)

# assign correct names to columns
colnames(dt1)[loc_col] <- "country"
colnames(dt1)[iso_col] <- "iso_code"

# keep columns of interest
loc_dt <- dt1[, loc_col, with=FALSE]
iso_dt <- dt1[, iso_col, with=FALSE]
cpi_dt <- dt1[, cpi_col, with=FALSE]

# merge columns together
dt2 <- cbind(loc_dt, iso_dt, cpi_dt)

# remove blank rows
dt2 <- dt2[!(is.na(country) & is.na(iso_code))]
dt2 <- dt2[-1,]

# melt data long
cpi_dataset = melt(dt2, id.vars = c('country', 'iso_code'), 
                      value.name = "cpi", 
                   variable.name = "year_factor")

# Extract four consecutive numeric digits from the year_factor variable
cpi_dataset$year <- sub('.*(\\d{4}).*', '\\1', cpi_dataset$year_factor)

# save year and CPI as numeric
cpi_dataset$year <- as.numeric(cpi_dataset$year)
cpi_dataset$cpi <- as.numeric(cpi_dataset$cpi)

# Load the location name code book
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge to ensure only national-level data is saved
full_cpi_dataset <- cpi_dataset %>% 
  inner_join(location_map, by='iso_code')

# Select columns of interest
full_cpi_dataset <- full_cpi_dataset %>% select(location, year, gbd_location_id, iso_code, iso_num_code, cpi)

# save the file on the prepped data folder
saveRDS(full_cpi_dataset, file = paste0(prepped_data_dir, "aim_2/04_prepped_cpi_data.RDS"))
