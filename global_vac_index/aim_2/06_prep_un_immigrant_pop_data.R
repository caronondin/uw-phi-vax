# Author: Francisco Rios Casas
# Purpose: Load and prep UN skilled attendant data
# Date: November 09, 2021

# Read in list of files to prep
file_list <- read_excel(paste0(g_drive, "data/list_of_data_used.xlsx")) %>% 
  filter(data_type=="index_variables")

# Set file path that will indicate which file to prep
file_path <- paste0(raw_data_dir, file_list$data_type[5], "/", file_list$data_source[5], "/", file_list$file_name[5])

# Read data sheet
dt1 <- read_xlsx(file_path, sheet = 4)

# Remove unnecessary columns
dt1 <- dt1[-c(1:8),-c(1, 13:26)]

name_row <- 1
names <- unlist(dt1[name_row,])
names(dt1) <- names

# Remove blank first rows
dt1 <- dt1[-1,]

# Remove unnecessary columns
dt1 <- dt1 %>% select(1, 3, 5:11)

# Assign correct names to columns
colnames(dt1)[1] <- "country"
colnames(dt1)[2] <- "iso_num_code"

# count how many characters are in the ISO num code column
# dt1$nchar <- nchar(dt1$iso_num_code)

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

# Reshape data layout
dt2 <- data.table(dt1)
imm_pop_dataset <- melt(dt2, id.vars=c("country", "iso_num_code"),
                     value.name = "imm_pop_perc",
                     variable.name = "year_factor")

# Create variable to replace year
imm_pop_dataset$year <- sub('.*(\\d{4}).*', '\\1', imm_pop_dataset$year_factor)

# replace "missing" code with NA
imm_pop_dataset$imm_pop_perc[ imm_pop_dataset$imm_pop_perc == ".." ] <- NA

# Reformat numeric columns
imm_pop_dataset$imm_pop_perc <- as.numeric(imm_pop_dataset$imm_pop_perc)
imm_pop_dataset$year <- as.numeric(imm_pop_dataset$year)

# Load location codebook to standardize names
location_map <- readRDS(paste0(codebook_directory, "location_iso_codes_final_mapping.RDS"))

# Merge location map onto the data
imm_pop_dataset <- imm_pop_dataset %>%
  inner_join(location_map, by="iso_num_code")

# Select columns of interest
imm_pop_dataset <- imm_pop_dataset %>% select(location, year, gbd_location_id, iso_code, iso_num_code, imm_pop_perc)

# expand dataset with all years to complete the series
# years_of_data <- (1990:2018)
# years_of_data <- (1990:2020)
# years <- matrix(nrow = 204, ncol = 31)
# colnames(years) <- years_of_data
# years <- data.table(years)
# 
# frame <- cbind(location_map, years)
# frame <- as.data.table(frame)
# 
# frame_long <- melt(frame, id.vars=c('gbd_location_id', 'location', 'iso_code', 'iso_num_code'), variable.factor = FALSE)
# frame_long$year <- as.numeric(frame_long$variable)
# 
# # Remove blank value from frame
# frame_long <- frame_long[,-c(5,6)]
# 
# # Use the frame to map existing data
# expanded_imm_pop_dataset <- frame_long %>% left_join(imm_pop_dataset, by = c("location", "iso_code", "iso_num_code", "gbd_location_id", "year"))
# 
# # extrapolate where necessary using GLM (better would be to use multiple imputation)
# numVars <- "imm_pop_perc"
# i=1
# for(v in numVars) {
#   for(h in unique(expanded_imm_pop_dataset$location)) { 
#     i=i+1
#     if (!any(is.na(expanded_imm_pop_dataset[location==h][[v]]))) next
#     if (!any(!is.na(expanded_imm_pop_dataset[location==h][[v]]))) next
#     form = as.formula(paste0(v,'~year'))
#     lmFit = glm(form, expanded_imm_pop_dataset[location==h], family='poisson')
#     expanded_imm_pop_dataset[location==h, tmp:=exp(predict(lmFit, newdata=expanded_imm_pop_dataset[location==h]))]
#     lim = max(expanded_imm_pop_dataset[location==h][[v]], na.rm=T)+sd(expanded_imm_pop_dataset[location==h][[v]], na.rm=T)
#     expanded_imm_pop_dataset[location==h & tmp>lim, tmp:=lim]
#     ggplot(expanded_imm_pop_dataset[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red')
#     expanded_imm_pop_dataset[location==h & is.na(get(v)), (v):=tmp]
#     pct_complete = floor(i/(length(numVars)*length(unique(expanded_imm_pop_dataset$location)))*100)
#     cat(paste0('\r', pct_complete, '% Complete'))
#     flush.console() 
#   }
# }
# expanded_imm_pop_dataset$tmp = NULL

# Save file in the prepped data folder
saveRDS(imm_pop_dataset, file = paste0(prepped_data_dir, "aim_2/05_prepped_un_immigrant_data.RDS"))
