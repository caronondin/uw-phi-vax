# Purpose: Create a model the predicts vaccination coverage based on vaccine improvement index
# Author: Francisco Rios Casas
# Date: Dec 22 2021

# clear environment
# rm(list=ls())

# load index dataset
index_data <- readRDS(paste0(prepped_data_dir, "aim_2/10_index_results.RDS"))

# load vaccination coverage dataset
vax_data <- readRDS(paste0(prepped_data_dir, "aim_1/01_vaccine_trends.RDS"))

# reshape the vaccination data
vax_data <- pivot_wider(vax_data,
            id_cols=c("location_id", "location_name", "year_id"),
            names_from = "vaccine_name",
            values_from = c(prop_val, prop_upper, prop_lower))

# merge together files for each location and year
full_data <- index_data %>% left_join(vax_data, by=c("gbd_location_id"="location_id", "year"="year_id", "location"="location_name"))

# subset columns
full_data <- full_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, result, prop_val_MCV1, prop_val_DTP1, prop_val_DTP3)

# drop the values from 2019
data <- full_data
data[year==2019, prop_val_MCV1:=NA]

# Use values to extrapolate per country
v <- "prop_val_MCV1"
h <- "Afghanistan"
form = as.formula(paste0(v,'~result'))
lmFit = glm(form, data[location==h], family='poisson')
data[location==h,tmp:=exp(predict(lmFit, newdata=data[location==h]))]
lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
data[location==h & tmp>lim, tmp:=lim]
ggplot(data[location==h], aes_string(y=v, x='result')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title=paste0(h))

numVars <- names("prop_val_MCV1")
i=1
pltlist <- list()
for(v in numVars) {
  for(h in unique(data$location)) {
    
    if (!any(is.na(data[location==h][[v]]))) next
    if (!any(!is.na(data[location==h][[v]]))) next
    form = as.formula(paste0(v,'~year'))
    lmFit = glm(form, data[location==h], family='binomial')
    data[location==h, tmp:=exp(predict(lmFit, newdata=data[location==h]))]
    lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
    data[location==h & tmp>lim, tmp:=lim]
    pltlist[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title = paste0(h))
    data[location==h & is.na(get(v)), (v):=tmp]
    i=i+1
    pct_complete = floor(i/(length(numVars)*length(unique(data$location)))*100)
    cat(paste0('\r', pct_complete, '% Complete'))
    flush.console()
  }
}

data$tmp = NULL

# plot relationship between index result and values of vaccine coverage
ggplot(data[], aes_string(y='prop_val_MCV1', x='result')) + geom_point()
ggplot(data, aes_string(y='prop_val_DTP1', x='result')) + geom_point()
ggplot(data[location=="Afghanistan"], aes_string(y='prop_val_DTP3', x='result')) + geom_point()
