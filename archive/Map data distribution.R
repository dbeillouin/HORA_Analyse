## Script to map data points and make whittaker graph
## Damien Beillouin, Sarah Jones
## 21 march 2023 last updated 25 Nov 2024

required.packages <- c("raster","terra","maptools","rasterVis","dplyr","data.table", "rnaturalearth", "vegan","ncdf4","mapproj","measurements","Ternary")
for(i in required.packages){
  if(i %in% installed.packages()==FALSE)
    install.packages(i)
}

## Load Packages
library(readxl)
library(openxlsx)
library(raster)
library(magrittr)
library(dplyr) # for data manipulation
library(foreign) # for reading dbf files
library(rnaturalearth)
library(terra)
#library(gdalUtilities)
library(stringr)
library(sf) 
library(mapproj)
library(magrittr)
library(googlesheets4)  # Interface with Google Sheets API v4
library(readxl)
library(tidyverse)
library(measurements)
library(Ternary)
library(usethis) # for git configuration

data.path <- "C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/"
#setwd("C:/Users/sjones/OneDrive - CGIAR/Documents/HORA_Analyse")

# import files 
kew <- read_xlsx("FoodPlants_SOTWPF2020.xlsx",sheet=1)

d <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")
#d <- read_excel("DB_HORA_20240927.xlsx",sheet = "Articles caractérisés")

d_typology <- read.csv("TAB_FINALE_TRANSFERT_SARAH_20241008.csv") 

list_species <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=390873745', sheet = "list_speciesDB")

unsd <- read.csv("UNSD — Methodology.csv",header=TRUE, sep = ";") # download here: https://unstats.un.org/unsd/methodology/m49/overview/

# format country names to match UNSD standard and join datasets
d <- d %>% 
  mutate(country = ifelse(country == "USA","United States of America",
                          ifelse(country == "Bengladesh","Bangladesh",
                                 ifelse(country == "Bolivia", "Bolivia (Plurinational State of)", 
                                        ifelse(country == "Vietnam", "Viet Nam",
                                               ifelse(country == "Venezuela", "Venezuela (Bolivarian Republic of)",
                                                      ifelse(country %in% c("England","United Kingdom"), "United Kingdom of Great Britain and Northern Ireland",
                                                             ifelse(country == "Virgin Islands", "United States Virgin Islands",
                                                                    ifelse(country == "Turkey", "Türkiye",
                                                                           ifelse(country == "Tanzania", "United Republic of Tanzania",
                                                                                  ifelse(country == "Ivory Coast", "Côte d’Ivoire",
                                                                                         ifelse(country == "at Pallekelle", "Sri Lanka",
                                                                                                ifelse(country == "Hawaii", "United States of America",
                                                                                                       ifelse(country == "Maroc", "Morocco",
                                                                                                              ifelse(country == "Bulgary", "Bulgaria",
                                                                                                                     ifelse(country == "Iran", "Iran (Islamic Republic of)",
                                                                                                                            ifelse(country == "Czech Republic", "Czechia",
                                                                                                                                   ifelse(country == "Brazil;Argentina;Paraguay", "Brazil",
                                                                                                                                          ifelse(country == "Russia", "Russian Federation",country))))))))))))))))))) %>%
  mutate(country = ifelse(source_id == 7430,"Benin",
                          ifelse(source_id == 4619,"China",
                                 ifelse(country =="Quebec","Canada",
                                        ifelse(country=="DR Congo","Democratic Republic of the Congo",country)))))

#sort(unique(d$country))

d <- d %>%   left_join(unsd %>% select(Region.Name,Sub.region.Name, Country.or.Area, ISO.alpha3.Code),by=c("country"="Country.or.Area")) #%>%rename("Region"="Region.Name") 

d <- d %>% 
  mutate(`farm size` = as.character(`farm size`),
         `Duration of study` = as.character(`Duration of study`),
         Intervention_reclass = as.character(Intervention_reclass),
         soil = as.character(soil),
         `tree age treatment` = as.character(`tree age treatment`),
         `fertiliser N treatment` = as.character(`fertiliser N treatment`),
         `Tree age control` = as.character(`Tree age control`),
         `fertiliser N control` = as.character(`fertiliser N control`),
         `tillage control` = as.character(`tillage control`),
         `raw density` = as.character(`raw density`),
         `row N treatment` = as.character(`row N treatment`),
         `row N control` = as.character(`row N control`))

# Clean the intervention classes
sort(unique(d$Intervention_reclass))
d <- d %>%
  mutate(Intervention_reclass = ifelse(Intervention_reclass %in% c("Alley-cropping","Alley cropping","alley cropping","alley-cropping","Alley cropping /complex mutli-strata agroforestry","Alley-cropping, complex multi-strata systems","Alley-cropping/ complex multi-strata systems","Alley cropping, multistrata","Alley cropping/ complex multi-strata agroforestry", "alley-cropping/hedgerows"),"Alley cropping",
                                       ifelse(Intervention_reclass %in% c("multistata",  "multi strata","multistrata", "complex multi-strata-agroforestry systems", "Complex multistrata systems","multi-strata","multi-strata?","complex multi-strata agroforestry","complex multi-strata system","Complex multi-strata agroforestry"),"Multi-strata systems",
                                              ifelse(Intervention_reclass %in% c("Parkland","parkland?","parkland"),"Parkland",
                                                     ifelse(Intervention_reclass %in% c("Herdgerows","complex multi-strata-agroforestry systems/hedgerows"),"Hedgerows",
                                                            ifelse(Intervention_reclass %in% c("fallows","Alley cropping, fallows"), "Fallows",
                                                                   ifelse(Intervention_reclass %in% c("Shaded systems","shaded", "?","NA"),"Other/unknown",Intervention_reclass)))))))

unique(d$Intervention_reclass)

#temp <- d %>% filter(Intervention_reclass == "shaded")

# add typology to main dataset

d <- d %>% rename(New.ID = `New ID`)

# remove white space in ID numbers from both files
d <- d %>% 
  mutate(New.ID = gsub(" ", "", New.ID, fixed = TRUE),
         numéro = gsub(" ", "", numéro, fixed = TRUE))

d_typology <- d_typology %>% 
  mutate(New.ID = gsub(" ", "", New.ID, fixed = TRUE),
         `numéro` = gsub(" ", "", numéro, fixed = TRUE))

table(d_typology$Categorie_C, d_typology$Categorie_C2)
table(d_typology$Concatenation)
sort(unique(d_typology$Concatenation))
sort(unique(d_typology$Categorie))

# correct numero column
#d_typology <- d_typology %>% mutate(numéro = ifelse(New.ID=="2170",sub(".*\\.", "", numéro),numéro))

# There are 5312 obs in the d dataset, and only 4513 in the d_typology. 
# Use right join because d_typology has filtered out experiments with zea mays 
d <- as.data.frame(d) %>% right_join(
  d_typology %>% as.data.frame() %>% select(-c("X", "farm.type","Design","Scale","country", "longitude","latitude","Intervention_reclass")),
  by=c("New.ID","numéro"))

#check <- d %>% filter(is.na(New.ID))
#check <- d %>% select(New.ID, numéro, Number_Total, Number_Woody, `species treatment`, `species control`, NB_sp, NB_spC) %>% filter(is.na(NB_sp))

d <- d %>% mutate(Intervention_by_composition = Categorie) # CHANGE THIS?

d <- d %>%
  mutate(intervention_by_complexity = case_when(Number_Total==2 & Intervention_reclass %in% c("Alley cropping","Hedgerows") ~ "Very simple",
                                                Number_Total==2 & !(Intervention_reclass %in% c("Alley cropping","Hedgerows")) ~ "Simple",
                                                Number_Total==3 & (Intervention_reclass %in% c("Alley cropping","Hedgerows")) ~ "Moderately complex",
                                                Number_Total==3 & !(Intervention_reclass %in% c("Alley cropping","Hedgerows")) ~ "Complex",
                                                Number_Total>3~"Very complex",.default="Other"))

table(d$intervention_by_complexity)

check <- d %>% filter(intervention_by_complexity =="Other" ) %>% select(New.ID,latitude,longitude, Number_Total,NB_sp,  `Intervention in our classification`,Intervention_reclass,Intervention_by_composition,intervention_by_complexity)

check2 <- d_typology %>% filter(New.ID == "1959")
check <- d %>% filter(is.na(Intervention_by_composition)) %>% unique()

#write.csv(unlist(check),"check_no_intervention_found.csv",row.names=FALSE)

# convert lat and long to decimal degrees
# from either decimal degrees with a comma instead of point separation (format 42,654563°N)
# or degrees, minutes, seconds (format 34°23'45,750W or 34°23’45,750W or 34°23′45,750W)
# We use the measurement package to convert the second type.

d <-  d %>% mutate(latitude = ifelse(latitude=="NA",NA,latitude),
                  longitude = ifelse(longitude=="NA",NA,longitude))

#d$latitude[i]
#CODE[i]
#grepl("'|′",d$latitude[i])

CODE <- grepl("'|′|’",d$latitude) 

for(i in 1:length(d$latitude)){
  
  if(CODE[i] == TRUE){
    
    d$latitude[i] <-gsub("°"," ",d$latitude[i]) 
    d$latitude[i] <-gsub("'"," ",d$latitude[i])
    d$latitude[i] <-gsub("′"," ",d$latitude[i])
    d$latitude[i] <-gsub(",",".",d$latitude[i])
    
    d$longitude[i] <-gsub("°"," ",d$longitude[i])
    d$longitude[i] <-gsub("'"," ",d$longitude[i])
    d$longitude[i] <-gsub("′"," ",d$longitude[i])
    d$longitude[i] <-gsub(",",".",d$longitude[i])
    
    # Format Lat S and Long W
    
    d$latitude[i] <- if (grepl("S",d$latitude)[i] == TRUE) {paste0("-",d$latitude[i])}else {d$latitude[i]} 
    d$longitude[i]<- if (grepl("W",d$longitude[i]) == TRUE) {paste0("-",d$longitude[i])}else {d$longitude[i]}
    
    # Format all lat and long
    
    d$latitude[i] <-gsub("N","",d$latitude[i])
    d$latitude[i] <-gsub("S","",d$latitude[i])
    d$longitude[i] <-gsub("E","",d$longitude[i])
    d$longitude[i] <-gsub("W","",d$longitude[i])
    
    # Convert to decimal degrees
    
    d$latitude[i] <- measurements::conv_unit(d$latitude[i], from = "deg_min_sec", to = "dec_deg")
    d$longitude[i] <- measurements::conv_unit(d$longitude[i], from = "deg_min_sec", to = "dec_deg")
  }
  
  else {
    d$latitude[i] <-gsub("°","",d$latitude[i])
    d$latitude[i] <-gsub(",",".",d$latitude[i])
    
    d$longitude[i] <-gsub("°","",d$longitude[i])
    d$longitude[i] <-gsub(",",".",d$longitude[i])
    
    # Format Lat S and Long W
    
    d$latitude[i] <- if (grepl("S",d$latitude[i]) == TRUE) {paste0("-",d$latitude[i])}else {d$latitude[i]}
    d$longitude[i]<- if (grepl("W",d$longitude[i]) == TRUE) {paste0("-",d$longitude[i])}else {d$longitude[i]}
    
    # Convert to decimal degrees
    
    d$latitude[i] <-gsub("N","",d$latitude[i])
    d$latitude[i] <-gsub("S","",d$latitude[i])
    d$longitude[i] <-gsub("E","",d$longitude[i])
    d$longitude[i] <-gsub("W","",d$longitude[i])
  }
}

# Fill in missing lat-longs using country centroids

# get a data.frame with country centroids
#centroids <- read.csv("https://github.com/gavinr/world-countries-centroids/blob/master/dist/countries.csv") # this is not working
#centroids <- read.csv(curl("https://github.com/gavinr/world-countries-centroids/blob/master/dist/countries.csv")) # this is not working
centroids <- read.csv("centroids.csv")  # downloaded manually

centroids <- centroids %>% rename(lon_centroid = longitude,
                                  lat_centroid = latitude) %>%
  mutate(country_unsd = case_when(COUNTRY =="United Kingdom"~ "United Kingdom of Great Britain and Northern Ireland",
                                  COUNTRY == "Tanzania"~"United Republic of Tanzania",
                                  COUNTRY == "Vietnam"~"Viet Nam",
                                  COUNTRY == "Bolivia"~ "Bolivia (Plurinational State of)",
                                  COUNTRY == "United States"~"United States of America",
                                  COUNTRY == "Venezuela"~ "Venezuela (Bolivarian Republic of)",
                                  COUNTRY == "Turkey"~ "Türkiye",
                                  COUNTRY == "Côte d'Ivoire"~ "Côte d’Ivoire",
                                  COUNTRY == "Congo DRC" ~ "Democratic Republic of the Congo",
                                  COUNTRY == "US Virgin Islands" ~ "United States Virgin Islands",
                                  COUNTRY == "Iran" ~ "Iran (Islamic Republic of)",
                                  COUNTRY == "Czech Republic" ~ "Czechia",
                                  .default=COUNTRY))

d_map <- d %>% left_join(centroids %>% rename(country_centroids = COUNTRY), by=c("country"= "country_unsd")) 

#check <- d_map %>% filter(is.na(latitude)) %>% select(country, latitude, longitude, lat_centroid,lon_centroid) %>% unique()
#check <- d_map %>% filter(is.na(lon_centroid)) %>% select(country, latitude, longitude, lat_centroid,lon_centroid) %>% unique()
#unique(check %>% filter(is.na(lat_centroid)) %>% select(country))
#sort(unique(centroids$COUNTRY))

d_map <- d_map %>%
  #mutate(latitude = unlist(latitude)) %>%
  mutate(lat_map= ifelse(is.na(latitude) | latitude %in% c("A"),lat_centroid,latitude),
         lon_map = ifelse(is.na(longitude)| longitude %in% c("A"),lon_centroid,longitude)) %>%
  mutate_at(c("lat_map","lon_map"),as.numeric)

#check <- d_map %>% select(New.ID, latitude,longitude,lat_map, lon_map) %>% filter(lat_map > 90 | is.na(lat_map) | is.na (lon_map)) %>% unique()

#write.csv(check,"data_missing_latlon.csv",row.names=FALSE)
#names(d_map)
d_map <- d_map %>%
  mutate(intervention_by_richness = case_when(Number_Total <5 ~as.character(Number_Total),
                                              Number_Total <11~"6-10",
                                              Number_Total <21~"11-20",
                                              Number_Total >20~"21-44",
                                              .default=as.character(Number_Total)))
d_map <- d_map %>% 
  select(c(New.ID,country,location, intervention_by_complexity, intervention_by_richness, Intervention_by_composition, Number_Total, lat_map,lon_map)) %>%
  group_by(New.ID, lat_map, lon_map) %>%
  mutate(n_experiments = n()) %>% 
  ungroup() %>%
  group_by(New.ID,intervention_by_complexity, lat_map,lon_map) %>%
  mutate(n_id_complexity = n()) %>%
  ungroup() %>%
  group_by(New.ID,intervention_by_richness, lat_map,lon_map) %>%
  mutate(n_id_richness = n()) %>%
  ungroup() %>% 
  group_by(New.ID,Intervention_by_composition, lat_map,lon_map) %>%
  mutate(n_id_composition = n()) %>%
  ungroup() %>% unique()

#d_map %>% filter(is.na(lat_map)) %>% select(country) %>% unique()

write.csv(d_map,"data_map.csv",row.names=FALSE)
write.table(d_map,"data_map.txt")

#### Map number of experiments per region ####

#library(installr)
#install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE)
library(raster)
library(rasterVis)
library(terra)
library(mapproj)
library(shadowtext)
library(forcats)
library(mapdata)
library(ggspatial)
library(scales)
library(viridis)
library(sf)
library(tmap)
library(RColorBrewer)
library(exactextractr)

#Set parameters for mapping
crs_wgs84 <-  "+proj=longlat +datum=WGS84 +no_defs"
crs_moll <-  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"
crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
col.terrain6 <- c("#fffdd0","#EEB99F","#EAB64E","#E6E600", "#63C600","forestgreen")
col.intervention <- c(viridis_pal()(7))
#show_col(viridis_pal()(12))

unique(d_map$intervention_by_complexity)
unique(d_map$intervention_by_richness)
unique(d_map$Intervention_by_composition)
hist(d_map$Number_Total)

# import data
#tree_ag_moll <- raster("D:/00_Data/ICRAF/tc_ag_2010_moll.tif") 
#tree_ag <- raster("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/Zomer/tc_ag_2010.tif")
#tree_ag <- rast("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/Chapman/Chapman_crop_mosaic_1km_gte10.tif")
#tree_ag_moll <- project(tree_ag, crs_moll)
#plot(tree_ag)
#plot(tree_ag_moll)
#tree_ag_moll[tree_ag_moll == 0] <- NA
#writeRaster(tree_ag_moll,"C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/Chapman/Chapman_crop_mosaic_1km_gte10_moll.tif")
#tree_ag <- tree_ag_moll
tree_ag <- rast("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/Chapman/Chapman_crop_mosaic_1km_gte10_moll.tif")

#lulc <- rast("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/ESA/esa_2020_cropland.tif")
#plot(lulc_moll)
#lulc_moll <- terra::project(lulc, crs_moll)
#writeRaster(lulc_moll,"C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/ESA/esa_2020_cropland_moll.tif")
#lulc <- lulc_moll
#lulc <- rast("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/ESA/esa_2020_cropland_moll.tif")
lulc <- rast("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/ESA/esa_2020_cropland_moll_arc_1km.tif")

ecoreg <- read_sf("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/Ecoregions/Ecoregions2017_biome.shp")
ecoreg <- st_transform(ecoreg, crs(tree_ag))

#install.packages("rnaturalearthdata")
world_ne <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_ne <- st_transform(world_ne, crs(tree_ag))
  
# covert our data to shapefile
#d_map <- d_map %>% filter(!is.na(lat_map)) %>% filter(!is.na(lon_map))
d_map_sf <- st_as_sf(d_map,coords=c("lon_map","lat_map"),crs=crs_wgs84)
d_map_sf <- d_map_sf %>%
  rename(n_rich = Number_Total,
         n_id_comp = n_id_complexity,
         n_id_rich = n_id_richness,
         int_comp = intervention_by_complexity,
         int_rich = intervention_by_richness,
         int_cton = Intervention_by_composition) %>%
  mutate(int_comp = factor(int_comp,levels= unique(int_comp[order(n_rich)]))) %>%
  mutate(int_rich = factor(int_rich,levels= unique(int_rich[order(n_rich)]))) %>%
  mutate(int_cton = factor(int_cton,levels= unique(int_cton[order(n_rich)])))

plot(d_map_sf)
# export shapefile to use in ArcMap
st_write(d_map_sf,"data_map.shp",append=FALSE)

# get proportion of experiments per region
#install.packages("exactextractr")

continents <- world_ne %>%
  group_by(region_un) %>%
  summarize(geometry = st_union(geometry))

subregions <- world_ne %>%
  group_by(subregion) %>%
  summarize(geometry = st_union(geometry))

countries <- world_ne %>%
  group_by(geounit) %>%
  summarize(geometry = st_union(geometry))

# Extract the valid (non-NA) and >0 areas on tree_ag raster for each continent
# And calculate the proportion of each continent represented by these pixels
tree_ag_non_na_areas <- exact_extract(tree_ag, continents, function(values, coverage_fraction) {
  # Calculate the proportion of valid (non-NA) cells
  sum((!is.na(values) & values>0) * coverage_fraction)
})

write.csv(data.frame(tree_ag_non_na_areas),"proportions_tree_ag.csv",row.names=FALSE)

# Extract the valid (non-NA) areas on tree_ag rasters for the entire world (therefore representing all agricultural land) - use this for Zomer data
#agland_areas <- exact_extract(tree_ag, st_union(st_geometry(world_ne)), function(values, #coverage_fraction) {
#  sum(!is.na(values)  * coverage_fraction)
#})

# Extract the cropland areas for each continent
agland_areas <- exact_extract(lulc, continents, function(values, coverage_fraction) {
  # Calculate the proportion of valid (non-NA) cells
  sum((!is.na(values) & values>0) * coverage_fraction)#*res(lulc)[1]*res(lulc)[2]
})

proportions_global <- tree_ag_non_na_areas / agland_areas

write.csv(data.frame(proportions_global),"proportions_tree_ag_global.csv",row.names=FALSE)

# Calculate the total area of each continent
continent_areas <- st_area(continents) # returns areas in square meters by default
# Convert to square kilometers if needed
continent_areas_km2 <- as.numeric(continent_areas) / 1e6  # km²

proportions_per_cont <- tree_ag_non_na_areas / continent_areas_km2

# Add the results to the dataframe
continents$tree_ag_proportion_per_cont <- proportions_per_cont
continents$tree_ag_proportions_global <- proportions_global

# Compute proportion of our experiments in each continent and subregion
d_map_sf <- st_transform(d_map_sf, crs(continents))
d_map_sf_w_continents <- st_join(d_map_sf, continents) %>%
  mutate(continent = ifelse(country == "Italy","Europe",
                            ifelse(country == "Canada","North America",
                                   ifelse(country =="Brazil","South America",continent)))) # region_un

#continents <- continents %>% mutate(continent  = region_un)

d_map_sf_w_subregions <- st_join(d_map_sf, subregions) %>%
  mutate(subregion = ifelse(country == "Italy","Western Europe",
                            ifelse(country == "Canada","Northern America",
                                   ifelse(country =="Brazil","South America",subregion)))) 

# Count the number of points in each continent
d_map_continent_counts <- data.frame(d_map_sf_w_continents)  %>%
  select(New.ID, continent, geometry, n_experiments) %>% unique() %>%
  group_by(continent)  %>%
  summarize(n_experiments_cont = sum(n_experiments))

d_map_continent_counts$n_experiments_cont_proportion_global <- d_map_continent_counts$n_experiments_cont / nrow(d)

# Count the number of points in each sub-region
d_map_subregion_counts <- data.frame(d_map_sf_w_subregions)  %>%
  select(New.ID, subregion, geometry, n_experiments) %>% unique() %>%
  group_by(subregion)  %>%
  summarize(n_experiments_subregion = sum(n_experiments))

d_map_subregion_counts$n_experiments_subregion_proportion_global <- d_map_subregion_counts$n_experiments_subregion / nrow(d)

d_map_subregion_counts <- d_map_subregion_counts %>% 
  mutate(subregion_grouped = case_when(subregion %in% c("Eastern Europe","Northern Europe", "Southern Europe","Western Europe")~"Europe", 
                                       subregion %in% c("Caribbean","Central America" , "South America") ~"South & Central America",
                                       subregion %in% c("Central Asia","Western Asia")~"Other Asia",
                                       subregion %in% c("Southern Africa","Eastern Africa","Western Africa","Middle Africa") ~ "Sub-Saharan Africa", 
                                       subregion %in% c("Australia and New Zealand") ~ "Oceania",
                                       .default=subregion)) %>%
  group_by(subregion_grouped)  %>%
  mutate(n_experiments_subregion_grouped = sum(n_experiments_subregion))

d_map_subregion_counts <- d_map_subregion_counts %>% left_join(d_map_subregion_counts %>% select(subregion_grouped, n_experiments_subregion_grouped) %>% unique() %>% group_by() %>% mutate(n_experiments_total = sum(n_experiments_subregion_grouped)) %>% mutate(subregion_grouped_proportion_global = n_experiments_subregion_grouped/n_experiments_total) %>% ungroup() %>% select(-n_experiments_total))

# Append dataset
continent_prop <- continents %>% select(continent,tree_ag_proportion_per_cont) %>% as.data.frame() %>% select(-geometry)
continent_prop <- continents %>% select(continent,tree_ag_proportion_per_cont,tree_ag_proportions_global) %>% as.data.frame() %>% select(-geometry)
continent_prop <- continent_prop %>% left_join(
  d_map_continent_counts %>% select(continent, n_experiments_cont, n_experiments_cont_proportion_global),by="continent")

#continent_prop <- continent_prop %>% select(-geometry)

# Get count and share of points per intervention in each continent and subregion
d_map_continent_int_counts <- data.frame(d_map_sf_w_continents)  %>%
  select(New.ID, continent, geometry, int_cton, n_id_composition) %>% unique() %>%
  group_by(continent, int_cton) %>%
  summarize(hora_n_per_int_cton_per_continent = sum(n_id_composition))

d_map_continent_int_counts <- left_join(d_map_continent_int_counts,
    data.frame(d_map_continent_counts)) %>%
  mutate(hora_n_int_cton_proportion_continent = hora_n_per_int_cton_per_continent/n_experiments_cont)

d_map_subregion_int_counts <- data.frame(d_map_sf_w_subregions)  %>%
  select(New.ID, subregion, geometry, int_cton, n_id_composition) %>% unique() %>%
  group_by(subregion, int_cton) %>%
  summarize(hora_n_per_int_cton_per_subregion = sum(n_id_composition))

d_map_subregion_int_counts <- left_join(d_map_subregion_int_counts,
                                        data.frame(d_map_subregion_counts)) %>%
  mutate(hora_n_int_cton_proportion_subregion = hora_n_per_int_cton_per_subregion/n_experiments_subregion) %>%
  group_by(subregion_grouped, int_cton) %>%
  mutate(hora_n_per_int_cton_per_subregion_grouped = sum(hora_n_per_int_cton_per_subregion)) %>% ungroup() %>%
  mutate(hora_n_per_int_cton_per_subregion_grouped = hora_n_per_int_cton_per_subregion_grouped/n_experiments_subregion_grouped) %>% 
  select(subregion_grouped,int_cton, n_experiments_subregion_grouped, subregion_grouped_proportion_global,hora_n_per_int_cton_per_subregion_grouped ) %>% unique()

d_map_subregion_int_counts <- d_map_subregion_int_counts %>% 
  mutate(subregion_grouped_label = paste0(subregion_grouped, " (", n_experiments_subregion_grouped,")"))

# Append dataset
continent_prop <- continent_prop %>% left_join(data.frame(d_map_continent_int_counts) %>% select(continent,int_cton, hora_n_per_int_cton_per_continent, hora_n_int_cton_proportion_continent), by="continent") 

names(continent_prop)
continent_prop <- continent_prop %>% 
  mutate_at(vars(n_experiments_cont, n_experiments_cont_proportion_global,hora_n_per_int_cton_per_continent, hora_n_int_cton_proportion_continent), ~ifelse(is.na(.),0,.))
#glimpse(continent_prop)

sum(continent_prop$hora_n_per_int_cton_per_continent, na.rm=TRUE)

#write.csv(continent_prop,"data_distribution_per_unregion.csv")
write.csv(continent_prop,"data_distribution_per_continent.csv")

##### Make figures #######

# Scatter plot showing % land with trees and % experiments per continent
g_scatter <- ggplot(continent_prop %>% filter(!continent %in% c("Seven seas (open ocean)","Antarctica")), aes(x=n_experiments_cont_proportion_global*100, y=tree_ag_proportions_global*100, colour=continent, size=n_experiments_cont))+
  geom_point()+
  geom_text(aes(label=continent,colour=continent),size=3.2,nudge_x=4, nudge_y=3,show.legend=FALSE)+
  coord_cartesian(expand=c(0))+
  labs(x="Share of experiments per region (%)", 
       y="Share of cropland with tree cover (%)")+
  scale_size_continuous(breaks=c(50,500,1500), range=c(2,8),"Number of experiments per region")+
  scale_color_manual(values=terrain.colors(7),"Region")+
  scale_x_continuous(limits=c(0,45))+
  scale_y_continuous(limits=c(0,50))+
  theme_classic()+
  theme(legend.text = element_text(size=10),
        legend.position="top",legend.direction = "horizontal",
        legend.title = element_text(size=10,face="bold",hjust=0.5),
        legend.title.position="top")+
  guides(size=guide_legend(override.aes=list(colour="grey80")),
         colour="none")

g_scatter

# Bar chart showing proportions per intervention per continent

col.int <- c("1"= "lightblue","2"="blue3","3"="lightgreen",
             "4"="forestgreen","5"="darkgreen","6-10"="violet",
             "11-20"="purple", "21-44"="purple4")

sort(unique(d_map$Intervention_by_composition))

col.int <- c("(HNE), HE, WE" = "purple4" , "(HNE), WE"="lightblue", "(HNE), WNE, WE"="lightgreen",   "WNE" ="forestgreen", "HNE, WNE" ="violet", "HE"  ="purple2", "Autre"= "steelblue")

col.int <- c("(HNE), HE, WE" = "lightgreen" , "(HNE), WE"="lightblue", "(HNE), WNE, WE" =  "purple4",   "(HNE), WNE, HE, WE" ="forestgreen","(HNE), WNE, HE"  ="purple2")

g_prop <- ggplot(continent_prop %>% filter(!continent %in% c("Seven seas (open ocean)","Antarctica")), aes(y=reorder(continent,desc(continent)),x=hora_n_int_cton_proportion_continent*100, fill=int_cton))+
  geom_col(position=position_stack())+
  labs(x="Proportion of studies (%)",y="")+
  scale_fill_manual(values=col.int,name="")+
  theme_classic()+
  theme(legend.position="top",legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.title = element_text(size=10))+
  guides(fill=guide_legend(reverse=TRUE,nrow=2))
g_prop

#ggsave("data_proportions_barchart.png",width=3,height=2.5,units="in")
tiff("data_proportions_int_region_barchart_unregion.tif",width=350,height=310,units="px")
g_prop
dev.off()

png("data_proportions_int_region_barchart_unregion.png",width=490,height=310,units="px")
g_prop
dev.off()

g_prop <- ggplot(d_map_subregion_int_counts, aes(y=reorder(str_wrap(subregion_grouped_label, 15),n_experiments_subregion_grouped),x=hora_n_per_int_cton_per_subregion_grouped*100, fill=int_cton))+
  geom_col(position=position_stack())+
  labs(x="Proportion of studies (%)",y="")+
  scale_fill_manual(values=col.int,name="")+
  theme_classic()+
  theme(legend.position="top",legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.title = element_text(size=10))+
  guides(fill=guide_legend(reverse=TRUE,nrow=2))
g_prop

#ggsave("data_proportions_barchart.png",width=3,height=2.5,units="in")
tiff("data_proportions_int_region_barchart_subregion.tif",width=350,height=310,units="px")
g_prop
dev.off()

png("data_proportions_int_region_barchart_subregion.png",width=490,height=310,units="px")
g_prop
dev.off()


# Make map
library(reshape2)
library(stars)
library(cowplot)

# Plot with ggplot but raster plotting takes a long time

#tree_ag_df <- as.data.frame(tree_ag, xy = TRUE) # memory limit exceeded

g_map <- ggplot() + 
  #geom_raster(data=tree_ag_df, aes(x=x, y=y, fill=value), alpha=0.8) +
  #geom_stars(data=tree_ag, downsample = 10) +
  geom_sf(data = world_ne %>% filter(!continent %in% c("Antarctica")),fill="white")+
  geom_sf(data = d_map_sf,
           aes(colour=int_cton, size=n_id_composition),alpha=0.7, 
           linewidth = 0.1)+ #size=n_experiments), 

  coord_sf(default_crs =sf::st_crs(4326))+
  #coord_map("mollweide",xlim=c(-180,180)) # azequalarea
  #coord_fixed(1.3)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  #scale_fill_gradientn(colours= rev(terrain.colors(10)), name='Tree cover (%)')+ 
  scale_colour_manual(values=col.int)+
  scale_size_continuous(range=c(1,7)) # breaks=c(1,5,10,40,76),
g_map

#ggsave("Map of interventions.tiff",width=8,height=4)
legend <- get_legend(g_map+theme(legend.position="right",
                             legend.direction="vertical",
                             legend.background=element_blank())+
                       #guides(col=guide_legend(nrow=7,override.aes = list(alpha = 1,size=5),title="Agroforestry system",size=5,title.theme = element_text(size = 10,face = "bold",colour = "black",angle = 0)),
                              guides(col="none",
                              size=guide_legend(nrow=5,
                                               override.aes = list(shape=21,fill="white",colour="black"),
                                               title="Number of experiments",
                                               title.theme = element_text(size = 10,face = "bold",colour = "black",angle = 0))))
                           
p1 <- plot_grid(g_map,legend, labels = c('a.', ''), label_size = 12, ncol=2,rel_widths = c(0.8, 0.2))
p2 <- plot_grid(g_prop,g_scatter, labels = c("b.","c."),label_size = 12, ncol= 2, rel_widths = c(0.5,0.5))
p2

tiff("Map of interventions_unregions.tiff",width=8,height=5,res=300,units="in")
p1
dev.off()

tiff("Map of charts_unregions.tiff",width=8,height=4,res=300,units="in")
p2
dev.off()

tiff("Map of interventions and charts below_subregions.tiff",width=8,height=8,res=300,units="in")
plot_grid(p1,p2,labels=c('',''), label_size=12, ncol=1, rel_heights=c(0.5,0.5))
dev.off()

png("Map of interventions_unregions.png",width=8,height=5,res=300,units="in")
p1
dev.off()

png("Map of charts_unregions.png",width=8,height=4,res=300,units="in")
p2
dev.off()

png("Map of interventions and charts below_subregions.png",width=8,height=8,res=300,units="in")
plot_grid(p1,p2,labels=c('',''), label_size=12, ncol=1, rel_heights=c(0.5,0.5))
dev.off()


# Descriptive stats for the paper
addmargins(table(d$Region.Name, d$Intervention_by_composition))
addmargins(table(d$Region.Name))
check <- data.frame(addmargins(table(d$Region.Name))) %>%
  mutate(Freq_prop = Freq/nrow(d)*100)
check <- data.frame(addmargins(table(d[which(d$Region.Name =="Americas"),]$country))) %>%
  mutate(Freq_prop = Freq/nrow(d)*100) %>%
  mutate(Freq_prop_local = Freq/max(Freq)*100)
sort(unique(d$Sub.region.Name))
check <- data.frame(addmargins(table(d[which(d$Sub.region.Name =="Northern America"),]$country))) %>%
  mutate(Freq_prop = Freq/nrow(d)*100) %>%
  mutate(Freq_prop_local = Freq/max(Freq)*100)
check <- data.frame(addmargins(table(d[which(d$Sub.region.Name =="Latin America and the Caribbean"),]$country))) %>%
  mutate(Freq_prop = Freq/nrow(d)*100) %>%
  mutate(Freq_prop_local = Freq/max(Freq)*100)

check <- continent_prop %>% select(continent, tree_ag_proportions_global, n_experiments_cont_proportion_global)
cor.test(check$tree_ag_proportions_global, check$n_experiments_cont_proportion_global)
#r = 0.35,  p = 0.041, at regional level

#### Proportion per biome ####

# Spatial join to add ecoregion information to experiments
d_map_w_biomes <- st_join(d_map_sf, ecoreg, join = st_intersects)

#check <- d_map_w_biomes %>% filter(BIOME_NAME == "Deserts & Xeric Shrublands"  )
#table( d_map_w_biomes$country, d_map_w_biomes$BIOME_NAME)
#check <- data.frame(d_map_w_biomes) %>% select(New.ID, BIOME_NAME, country) %>% unique()
#write.csv(check, "d_with_biome.csv",row.names=FALSE)
#check <- check %>% right_join(d)
#check <- check %>% filter(BIOME_NAME == "Deserts & Xeric Shrublands" )
#check <- check %>% left_join(d_typology)
#ggplot(check, aes(x=Intervention_reclass))+geom_bar()
#plot(ecoreg['BIOME_NAME'])

biome_prop <- d_map_w_biomes %>%
  select(New.ID,geometry, n_experiments, BIOME_NAME) %>% unique() %>%
  group_by(BIOME_NAME) %>%
  summarise(n_per_biome = sum(n_experiments))

sum(biome_prop$n_per_biome)

biome_prop <- biome_prop %>%
  mutate(Freq = n_per_biome/sum(biome_prop$n_per_biome)*100) 

biome_prop <- biome_prop %>% mutate(biome_label = paste0(BIOME_NAME," (",n_per_biome,")"))

g_biome <- ggplot(biome_prop %>% filter(!is.na(BIOME_NAME)), aes(y=reorder(str_wrap(biome_label, 30),Freq),x=Freq))+
  geom_col(fill="grey50")+
  labs(x="Proportion of experiments (%)",y="")+
  theme_classic()+
  theme(legend.position="top",legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.title = element_text(size=10))+
  guides(fill=guide_legend(reverse=TRUE,nrow=2))
g_biome

#ggsave("data_proportions_barchart.png",width=3,height=2.5,units="in")
tiff("data_biome_barchart.tif",width=7,height=5,res=300,units="in")
g_biome
dev.off()

png("data_biome_barchart.png",width=7,height=5,units="in",res=300)
g_biome
dev.off()


# Calculate share of cropland with trees, per biome
tree_ag_non_na_areas_biomes <- exact_extract(tree_ag, ecoreg, function(values, coverage_fraction) {
  # Calculate the proportion of valid (non-NA) cells
  sum((!is.na(values) & values>0) * coverage_fraction)
})

agland_areas_biomes <- exact_extract(lulc, ecoreg, function(values, coverage_fraction) {
  # Calculate the proportion of valid (non-NA) cells
  sum((!is.na(values) & values>0) * coverage_fraction)#*res(lulc)[1]*res(lulc)[2]
})

#proportions_global <- tree_ag_non_na_areas_biomes / agland_areas_biomes

# Calculate the total area of each biome
biome_areas <- st_area(ecoreg) # returns areas in square meters by default
# Convert to square kilometers if needed
biome_areas_km2 <- as.numeric(biome_areas) / 1e6  # km²

proportions_per_biome <- tree_ag_non_na_areas_biomes/ biome_areas


#### Proportion per country to run correlation analysis ####
# This killed my computer and needs to be run on the server

tree_ag_non_na_areas_countries <- exact_extract(tree_ag, countries, function(values, coverage_fraction) {
  # Calculate the proportion of valid (non-NA) cells
  sum((!is.na(values) & values>0) * coverage_fraction)
})

agland_areas_countries <- exact_extract(lulc, countries, function(values, coverage_fraction) {
  # Calculate the proportion of valid (non-NA) cells
  sum((!is.na(values) & values>0) * coverage_fraction)#*res(lulc)[1]*res(lulc)[2]
})

proportions_global <- tree_ag_non_na_areas / agland_areas

# Calculate the total area of each country
countries_areas <- st_area(countries) # returns areas in square meters by default
# Convert to square kilometers if needed
countries_areas_km2 <- as.numeric(countries_areas) / 1e6  # km²

proportions_per_country <- tree_ag_non_na_areas_countries / countries_areas


### Whitetaker - this part needs updating ####

library(raster)
library(sp)

P1 <- raster('wc2.1_10m_prec_01.tif')
T1 <- raster('wc2.1_10m_tavg_01.tif')

points <- spsample(as(P1@extent, 'SpatialPolygons'),n=100, type="random") 
values <- raster::extract(P1,points)

TEST<- DB_0 %>% dplyr::select(c('New.ID', 'longitude','latitude'))
TEST<-TEST %>% dplyr::filter(!is.na(longitude),
                             !is.na(latitude))
TEST1<-sf::st_as_sf( TEST, coords = c( "longitude", "latitude" ) )
values <- raster::extract(P1,TEST1)
DATA_P <- cbind.data.frame(TEST,values)
names(DATA_P)[4]<-"precipitation"

values <- raster::extract(T1,TEST1)
DATA_T <- cbind.data.frame(TEST,values)
names(DATA_T)[4]<-"temperature"

DATA<-left_join(DATA_T,DATA_P)

#devtools::install_github("valentinitnelav/plotbiomes")
#library(plotbiomes)
whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = DATA, 
             aes(x = temperature, 
                 y = precipitation), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()

library("Ternary")

data(holdridge, holdridgeLifeZonesUp, package = "Ternary")

# Suppress plot margins
par(mar = c(0, 0, 0, 0))

# Create blank Holdridge plot
HoldridgePlot(hex.labels = holdridgeLifeZonesUp)
HoldridgeBelts()

# Plot data, shaded by latitude; darker = equitorial.
HoldridgePoints(holdridge$PET, holdridge$Precipitation,
                col = hcl.colors(91)[abs(holdridge$Latitude) + 1],
                lwd = 2)

df <- cbind.data.frame(coordinates(points),values)
plot(P1[[1]])
plot(points,add=T)

r <- getData("worldclim",var="bio",res=10)
w = getData('worldclim', var='tmin', res=0.5, lon=5, lat=45)

ggplot(world2) +
  geom_sf(aes(fill = no_articles)) +                 # on dessine le fond de carte
  # scale_fill_manual(values=c('#edf8b1',"#7fcdbb",'#41b6c4','#2b8cbe','#225ea8','#253494','#3f007d'), na.value= "white")+   # on colorie en fonction du nb de PS
  guides(fill = guide_legend(reverse = F)) +
  labs(fill = 'number of experiments'                    # on ajuste les titres, lÃ©gendes, ....
       ,color = '.'
       ,title = ''
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = 'black')  # on ajuste les param graphiques
        ,plot.title = element_text(size = 22)
        ,plot.subtitle = element_text(size = 15)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'white')
        ,plot.background = element_rect(fill = 'white')
        ,legend.position = c(.08,.06)
        ,legend.background = element_blank()
        ,legend.key = element_blank())+
  theme(panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  #annotation_scale(location = "bl", width_hint = 0.2) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #  pad_x = unit(0.05, "in"), pad_y = unit(0.5, "in"),
  #   style = north_arrow_fancy_orienteering) +
  theme(legend.position="bottom")

### Removed code ####

# import lookup table of country-regions
#databasis_region <- read_excel("./databasis_region.xlsx")

#write.csv(country,'country_region.csv')
#DB1_country <- data.frame(left_join(DB_0,databasis_region,by=c('country'='geounit')))

occ_region <- data.frame(table(d_id_unique$Region))
occ_geounit <- data.frame(table(d_id_unique$country))

names(occ_geounit)[1] <- 'country'
names(occ_geounit)[2] <- 'no_articles'
occ_geounit$no_articles2 <- cut(occ_geounit$no_articles,breaks=c(as.numeric(0),as.numeric(1),as.numeric(5),as.numeric(10),as.numeric(20),as.numeric(30),as.numeric(50),as.numeric(70)))

world$geounit <-tolower(world$geounit)
occ_geounit$country <- tolower(occ_geounit$country)

# we change the names of the geounit to make them match between the 2 tables

world$geounit<-plyr::revalue(world$geounit,c("united kingdom"="uk","united states of america"="usa"))
setdiff(world$geounit,occ_geounit$country)

# merging map background and data

T0world2<-merge(world,occ_geounit, all=TRUE)
world2 <- T0world2 #%>% select(geounit,no_articles)


# plot with tmap (takes ages)
tm_shape(tree_ag) + tm_raster(palette = "-RdYlBu", title = "Raster Value") +
  tm_layout(main.title = "Raster Layer Map") +
  tm_shape(world_ne) +  tm_borders("black") + 
  tm_shape(d_map_sf) +  tm_dots(col = "red", size = 0.1)

# plot with RasterVis (not working)
levelplot(tree_ag, margin = FALSE, col.regions = terrain.colors(100)) +
  layer(world_ne, col = "black")# +  # Add country boundaries
layer(d_map_sf, col = "red", pch = 19, cex = 1)  # Add point data

# Plot with terra

d_map_sf_moll <- project(vect(d_map_sf),crs_moll)
st_write(d_map_sf_moll,"data_map_moll.shp")
world_ne_moll <- project(vect(world_ne),crs_moll)
st_write(world_ne_moll,paste0(data.path,"naturalearth/ne_moll.shp"))

terra::plot(tree_ag, col = brewer.pal(5,"Greens"), main = "",axes=FALSE,box=FALSE,clip=FALSE,legend=FALSE)
# Add country boundaries
plot(world_ne, add = TRUE, border = "grey80",col=NA,lwd=0.5)
# Add points
points(d_map_sf, col = viridis(7)[factor(d_map_sf$intervention_by_richness,levels=c(unique(d_map_sf$intervention_by_richness[order(d_map_sf$Number_Total)])))], cex = c("1"=0.3,"2"=0.4,"3"=0.6,"4"=0.8,"5"=1, "6-10"=1.2,"11-20"=1.4,"21-44"=1.6)[d_map_sf$intervention_by_richness], pch = 19,alpha=0.5)

legend(x=-80,y=-100, legend = sort(unique(d_map_sf$intervention_by_richness)), col =  viridis(7), pch = 19, title = "Agroforestry plant richness",ncol=4, bty="n",cex=0.8,xjust=0.5,yjust=0.5)

legend(x=50,y=-50, legend = !!! ,col = brewer.pal(5,"Greens"), title = "% tree cover on agricultural land",ncol=1, bty="n",cex=0.8,xjust=0.5,yjust=0.5)

dev.off()
