## Script to map data points and make whittaker graph
## Damien Beillouin, Sarah Jones
## 21 march 2023 last updated 6 March 2024

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

#use_git_config(user.name= "Sarah Jones",user.email = "s.jones@cgiar.org")

data.path <- "C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/"
#setwd("C:/Users/sjones/OneDrive - CGIAR/Documents/HORA_Analyse")

# import files 
kew <- read_xlsx(paste0(data.path,"FoodPlants_SOTWPF2020.xlsx"),sheet=1)

d <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")
d <- read_excel("DB_HORA_20240927.xlsx",sheet = "Articles caractérisés")

d_typology <- read.csv("TAB_FINALE.HORA_20240306.csv") 

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

d <- d %>%   left_join(unsd,by=c("country"="Country.or.Area")) #%>%rename("Region"="Region.Name") 

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

names(d)
names(d_typology)

d <- d %>% mutate(New.ID = as.character(`New ID`),
                  numéro = as.character(numéro))

# remove white space in ID numbers from both files
d <- d %>% 
  mutate(New.ID = gsub(" ", "", New.ID, fixed = TRUE),
         numéro = gsub(" ", "", numéro, fixed = TRUE))

d_typology <- d_typology %>% 
  mutate(New.ID = gsub(" ", "", New.ID, fixed = TRUE),
         `numéro` = gsub(" ", "", numéro, fixed = TRUE))

d_typology <- d_typology %>% mutate(Intervention_by_composition = ifelse(Categorie =="(NH_Herb), NH_Wood, H_Herb","Classic",
                                                       ifelse(Categorie =="(NH_Herb), H_Herb, H_Wood","Exclusive",
                                                              ifelse(Categorie =="(NH_Herb), H_Wood","Woody exclusive",
                                                                     ifelse(Categorie =="(NH_Herb), NH_Wood, H_Herb, H_Wood","Complex",
                                                                            ifelse(Categorie =="(NH_Herb), NH_Wood, H_Wood","Woody classic","check"))))))


d <- as.data.frame(d) %>% left_join(
  d_typology %>% as.data.frame() %>% select(-c("X", "farm.type","Design","Scale")), by=c("New.ID"="New.ID","numéro"="numéro")) %>% filter(!(is.na(New.ID)))

d <- d %>%
  mutate(intervention_by_complexity = case_when(Number_Total==2 & Intervention_reclass %in% c("Alley cropping","Hedgerows") ~ "Very simple",
                                                Number_Total==2 & !(Intervention_reclass %in% c("Alley cropping","Hedgerows")) ~ "Simple",
                                                Number_Total==3 & (Intervention_reclass %in% c("Alley cropping","Hedgerows")) ~ "Moderately complex",
                                                Number_Total==3 & !(Intervention_reclass %in% c("Alley cropping","Hedgerows")) ~ "Complex",
                                                Number_Total>3~"Very complex",.default="Other"))

table(d$intervention_by_complexity)
table(d$intervention_by_complexity)

check <- d %>% filter(intervention_by_complexity =="Other" ) %>% select(`New ID`,latitude,longitude, Number_Total,NB_sp,NB_strates,  `Intervention in our classification`,Intervention_reclass,Intervention_by_composition,intervention_by_complexity)

check2 <- d_typology %>% filter(New.ID == "1959")

check <- d %>% filter(is.na(Intervention_by_composition)) %>% unique()

write.csv(check,"check_no_intervention_found.csv",row.names=FALSE)

d <- d %>% filter(!is.na(NB_sp))

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
centroids <- read.csv("https://github.com/gavinr/world-countries-centroids/blob/master/dist/countries.csv") # this is not working
centroids <- read.csv(curl("https://github.com/gavinr/world-countries-centroids/blob/master/dist/countries.csv")) # this is not working
centroids <- read.csv("centroids.csv")  # downloaded manually

centroids <- centroids %>% rename(lon_centroid = longitude,
                                  lat_centroid = latitude) %>%
  mutate(country_unsd = ifelse(COUNTRY =="United Kingdom", "United Kingdom of Great Britain and Northern Ireland",
                               ifelse(COUNTRY == "Tanzania","United Republic of Tanzania",
                                      ifelse(COUNTRY == "Vietnam","Viet Nam",
                                                    ifelse(COUNTRY == "Bolivia", "Bolivia (Plurinational State of)",
                                                           ifelse(COUNTRY == "United States", "United States of America",
                                                                  ifelse(COUNTRY == "Venezuela", "Venezuela (Bolivarian Republic of)",
                                                                         ifelse(COUNTRY == "Turkey", "Türkiye",
                                                                                                     ifelse(COUNTRY == "Côte d'Ivoire", "Côte d’Ivoire",COUNTRY)))))))))

d <- d %>% left_join(centroids, by=c("country"= "country_unsd")) 
check <- d %>% filter(is.na(latitude)) %>% select(country, latitude, longitude, lat_centroid,lon_centroid)
unique(check %>% filter(is.na(lat_centroid)) %>% select(country))

d <- d %>%
  mutate(lat_map= ifelse(is.na(latitude),lat_centroid,latitude),
         lon_map = ifelse(is.na(longitude),lon_centroid,longitude)) %>%
  mutate_at(c("latitude","longitude", "lat_map","lon_map"),as.numeric)

d <- d %>%
  mutate(intervention_by_richness = case_when(Number_Total <5 ~as.character(Number_Total),
                                              Number_Total <11~"6-10",
                                              Number_Total <21~"11-20",
                                              Number_Total >20~"21-44",
                                              .default=as.character(Number_Total)))
d_map <- d %>% 
  select(c(source_id, intervention_by_complexity, intervention_by_richness, Number_Total, lat_map,lon_map)) %>%
  group_by(source_id, intervention_by_complexity, lat_map,lon_map) %>%
  mutate(n_id_complexity = n()) %>%
  ungroup() %>%
  group_by(source_id, intervention_by_richness, lat_map,lon_map) %>%
  mutate(n_id_richness = n()) %>%
  ungroup() %>% unique()
#filter(!(is.na(latitude))&!(is.na(longitude)))

write.csv(d_map,"data_map.csv",row.names=FALSE)

#### Map number of experiments per region ####
# Note this code wasn't used to make final figure for time and aesthetic reasons 

library(installr)
install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE)
library(raster)
library(rasterVis)
library(mapproj)
library(shadowtext)
library(forcats)
library(rgeos)
library(mapdata)
library(ggspatial)
library(scales)
library(viridis)

#Set parameters for mapping
crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
col.terrain6 <- c("#fffdd0","#EEB99F","#EAB64E","#E6E600", "#63C600","forestgreen")
col.intervention <- c(viridis_pal()(7))
#show_col(viridis_pal()(12))

unique(d_map$intervention_by_complexity)
unique(d_map$intervention_by_richness)


# import data
tree_ag <- raster("D:/00_Data/ICRAF/tc_ag_2010_moll.tif") 
tree_ag <- raster("C:/Users/sjones/OneDrive - CGIAR/00_Data_copy_20240615/Zomer/tc_ag_2010.tif")
#tree_ag_df <- as.data.frame(tree_ag, xy = T)
plot(tree_ag)
tree_ag
tree_ag_df <- as.data.frame(tree_ag, xy = TRUE, na.rm = TRUE)

#install.packages("rnaturalearthdata")
world_ne <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

library(sf)
library(tmap)
library(rasterVis)
library(RColorBrewer)

# covert our data to shapefile
d_map <- d_map %>% filter(!is.na(lat_map)) %>% filter(!is.na(lon_map))
d_map_sf <- st_as_sf(d_map,coords=c("lon_map","lat_map"))
d_map_sf <- d_map_sf %>%
  mutate(intervention_by_complexity = factor(intervention_by_complexity,levels= unique(intervention_by_complexity[order(Number_Total)]))) %>%
  mutate(intervention_by_richness = factor(intervention_by_richness,levels= unique(intervention_by_richness[order(Number_Total)])))

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
crs_wgs84 <-  "+proj=longlat +datum=WGS84 +no_defs"
crs_moll <-  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

#tree_ag_moll <-  project(rast(tree_ag), crs_moll)
#writeRaster(tree_ag_moll,paste0(data.path,"Zomer/tree_ag_moll.tif"))
tree_ag_moll <- raster(paste0(data.path,"Zomer/tree_ag_moll.tif"))
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

# Using base mapping
#tiff(paste0(output,"Fig_1.tif"),height=4,width=7.4,units="in",res=900,compression="lzw")

par(mar = c(2, 1,1,1), oma=c(2,1,1,1), mfrow = c(1,1),adj=0) 
raster::plot(tree_ag, axes=FALSE, ann=F,box=F,legend=T,legend.width=0.5, legend.shrink=0.75,horizontal=TRUE,
             legend.args=list(text="Tree cover on agricultural land (%)",side=3,cex=0.8),
             col=col.terrain6, xpd=TRUE)
plot(world, add=TRUE, lwd=0.3)
plot(map_data,add=TRUE,col=col.intervention)
title(main="",font.main=2)


# Or with ggplot but world plotting takes a long time
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)
world <- ggplot2::map_data("world",wrap=c(-180,180))%>%filter(region != "Antarctica")

g <- ggplot() + 
  geom_raster(data=tree_ag_df, aes(x=x, y=y, fill=tc_ag_2010_moll), alpha=0.8) +
  geom_polygon(data = world,
               aes(x=long, y = lat, group = group), 
           fill="white",color = "black", 
           linewidth = 0.1) +
  #coord_sf(default_crs =sf::st_crs(4326))+
  coord_map("mollweide",xlim=c(-180,180))+ # azequalarea
  #coord_fixed(1.3)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "None") +
  geom_point(data = arrange(d_map,desc(n_experiments)), 
             aes(x=longitude,y=latitude,colour=Intervention_reclass,size=n_experiments),
             alpha = 0.7,
             shape=20)+
  scale_fill_gradientn(colours= rev(terrain.colors(10)), name='Tree cover (%)')+ 
  scale_colour_viridis_d(7)+
  scale_size_continuous(breaks=c(1,5,10,40,76),range=c(1,7))

g


library(cowplot)
legend <- get_legend(g+theme(legend.position="right",
                             legend.direction="vertical",
                             legend.background=element_blank())+
                       guides(col=guide_legend(nrow=6,
                                               override.aes = list(alpha = 1,size=5),
                                               title="System",size=5,
                                               title.theme = element_text(size = 11,face = "bold",colour = "black",angle = 0)),
                              size=guide_legend(nrow=1,
                                               override.aes = list(shape=21,fill="white",colour="black"),
                                               title="Number of experiments",
                                               title.theme = element_text(size = 11,face = "bold",colour = "black",angle = 0))))
                           
plot(legend)
#g_bar_interventions <- system.file("extdata", "g_bar_interventions.png", package = "cowplot")

tiff("Map of interventions.tiff",width=8,height=4,res=300,units="in")

plot_grid(g, legend, labels = c('', ''), label_size = 12,ncol=2,rel_widths = c(0.75, 0.25))

dev.off()

ggsave("Map of interventions.tiff",width=8,height=4)
#
#scale_fill_gradient2(na.value="white")+
#  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 80))

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
