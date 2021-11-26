# Author: Francisco Rios Casas
# Purpose: Load and prep IHME DAH data files
# Date: October 29, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables" & data_source=="ihme")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, "/", file_list$data_type[1], "/", file_list$data_source[1], "/", file_list$containing_folder[1], "/", file_list$file_name[1])

# Load data file
dt1 <- read_csv(file_path, show_col_types = FALSE)

# Drop certain rows to avoid double counting
dt1 <- dt1 %>% filter(elim_ch==0)

# Subset columns
dt1 <- dt1 %>% select(year, source, recipient_isocode, recipient_country, gbd_location_id, nch_cnv_dah_20)

# replace missing values with NA
dt1$nch_cnv_dah_20[dt1$nch_cnv_dah_20=="-"] <- NA

# Transform column from character to numeric
dt1$nch_cnv_dah_20 <- as.numeric(dt1$nch_cnv_dah_20)

# sum donations across all sources
dt1 <- as.data.table(dt1)
dt1 <- dt1[, .(nch_cnv_dah_20=sum(nch_cnv_dah_20, na.rm = T)), by=c('year', 'recipient_isocode', 'recipient_country', 'gbd_location_id')]

# Load the location name codebook
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge location names onto dataset
dah_dataset <- dt1 %>% inner_join(location_map, by=c('recipient_isocode'='iso_code', 'gbd_location_id'))

# Change the name of the variables to make sure they are standardized
setnames(dah_dataset, 
         old = c("recipient_isocode"),
         new = c("iso_code"))

# Subset final columns
dah_dataset <- dah_dataset %>% select(location, year, gbd_location_id, iso_code, iso_num_code, nch_cnv_dah_20)

# Save data
saveRDS(dah_dataset, file=paste0(prepped_data_dir, "aim_2/01_prepped_ihme_dah_data.RDS"))
