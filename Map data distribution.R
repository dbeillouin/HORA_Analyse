## Script to map data points and make whittaker graph
## Damien Beillouin, Sarah Jones
## 21 march 2023 last updated 6 March 2024

## Load Packages
library(mapproj)
library(magrittr)
library(googlesheets4)  # Interface with Google Sheets API v4
library(readxl)
library(igraph)
library(tidyverse)
library(measurements)
library(Ternary)
library(usethis) # for git configuration

#use_git_config(user.name= "Sarah Jones",user.email = "s.jones@cgiar.org")

#setwd("D:/02_Bioversity/41_HORA/R")
#setwd("C:/Users/sjones/OneDrive - CGIAR/Documents/HORA_Analyse")

# import files 
kew <- read_xlsx("./FoodPlants_SOTWPF2020.xlsx",sheet=1)
d <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")
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
d <- d %>%
  mutate(Intervention_reclass = ifelse(Intervention_reclass %in% c("Alley-cropping","Alley cropping","alley cropping","alley-cropping","Alley cropping /complex mutli-strata agroforestry","Alley-cropping, complex multi-strata systems","Alley-cropping/ complex multi-strata systems","Alley cropping, multistrata","Alley cropping/ complex multi-strata agroforestry", "alley-cropping/hedgerows"),"Alley cropping",
                                       ifelse(Intervention_reclass %in% c("multistata",  "multi strata","multistrata", "complex multi-strata-agroforestry systems", "Complex multistrata systems","multi-strata","multi-strata?","complex multi-strata agroforestry","complex multi-strata system","Complex multi-strata agroforestry"),"Multi-strata systems",
                                              ifelse(Intervention_reclass %in% c("Parkland","parkland?","parkland"),"Parkland",
                                                     ifelse(Intervention_reclass %in% c("Herdgerows","complex multi-strata-agroforestry systems/hedgerows"),"Hedgerows",
                                                            ifelse(Intervention_reclass %in% c("fallows","Alley cropping, fallows"), "Fallows",
                                                                   ifelse(Intervention_reclass %in% c("Shaded systems","?","NA"),"Other/unknown",Intervention_reclass)))))))
unique(d$Intervention_reclass)


# add typology to main dataset

names(d)
names(d_typology)

d <- d %>% mutate(Article_ID = as.character(`New ID`),
                  Row_ID = as.numeric(as.character(numéro))) 

# remove white space in ID numbers from both files
d <- d %>% 
  mutate(Article_ID = gsub(" ", "", Article_ID, fixed = TRUE),
         Row_ID = gsub(" ", "", Row_ID, fixed = TRUE))

d_typology <- d_typology %>% 
  mutate(New.ID = gsub(" ", "", New.ID, fixed = TRUE),
         `numéro` = gsub(" ", "", numéro, fixed = TRUE))

d_typology <- d_typology %>% mutate(Intervention_by_composition = ifelse(Categorie =="(NH_Herb), NH_Wood, H_Herb","Classic",
                                                       ifelse(Categorie =="(NH_Herb), H_Herb, H_Wood","Exclusive",
                                                              ifelse(Categorie =="(NH_Herb), H_Wood","Woody exclusive",
                                                                     ifelse(Categorie =="(NH_Herb), NH_Wood, H_Herb, H_Wood","Complex",
                                                                            ifelse(Categorie =="(NH_Herb), NH_Wood, H_Wood","Woody classic","check"))))))

test <- as.data.frame(d) %>% left_join(
  d_typology %>% as.data.frame() %>% select(-c("farm.type","Design","Scale")), by=c("Article_ID"="New.ID","Row_ID"="numéro")) %>% filter(!(is.na(Article_ID)))

check <- test %>% select(Article_ID,Row_ID,Intervention_by_composition, Intervention_reclass) 

length(unique(check$Article_ID))
length(unique(check$Row_ID))

check <- check %>% filter(is.na(Intervention_by_composition)) %>% unique()

write.csv(check,"check_no_intervention_found.csv",row.names=FALSE)

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
         lon_map = ifelse(is.na(longitude),lon_centroid,longitude))

d$latitude<-as.numeric(d$latitude)
d$longitude<-as.numeric(d$longitude)
d$lat_map<-as.numeric(d$lat_map)
d$lon_map<-as.numeric(d$lon_map)

d_map <- d %>% group_by(source_id, Intervention_by_composition, lat_map,lon_map) %>%
  filter(!is.na(Intervention_by_composition)) %>%
  summarise(n_experiments = n())
#filter(!(is.na(latitude))&!(is.na(longitude)))

write.csv(d_map,"data_map.csv",row.names=FALSE)

#### Map number of experiments per region ####
# Note this code wasn't used to make final figure for time and aesthetic reasons 

library(installr)
install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE)
library(raster)
library(rasterVis)
library(rgdal)
library(mapproj)
library(shadowtext)
library(forcats)
library(rgeos)
library(mapdata)
library(ggspatial)

# Set parameters for rmapping
crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
col.terrain6 <- c("#fffdd0","#EEB99F","#EAB64E","#E6E600", "#63C600","forestgreen")
col.intervention <- c("purple")
# import data
tree_ag <- raster("D:/00_Data/ICRAF/tc_ag_2010_moll.tif") 
#tree_ag_df <- as.data.frame(tree_ag, xy = T)
tree_ag_df <- as(tree_ag,"SpatialPixelsDataFrame")
tree_ag_df <- as.data.frame(tree_ag_df)
colnames(tree_ag_df) <- c("value","x","y")

#install.packages("rnaturalearthdata")
#world_ne <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# quick world map
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

world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group),fill="white",colour="black") + 
  coord_map("mollweide",xlim=c(-180,180))+ 
  plain
worldplot

# Using base mapping
tiff(paste0(output,"Fig_1.tif"),height=4,width=7.4,units="in",res=900,compression="lzw")

par(mar = c(2, 1,1,1), oma=c(2,1,1,1), mfrow = c(1,1),adj=0) 
raster::plot(tree_ag, axes=FALSE, ann=F,box=F,legend=T,legend.width=0.5, legend.shrink=0.75,horizontal=TRUE,
             legend.args=list(text="Tree cover on agricultural land (%)",side=3,cex=0.8),
             col=col.terrain6, xpd=TRUE)
plot(world, add=TRUE, lwd=0.3)
plot(map_data,add=TRUE,col=col.intervention)
title(main="",font.main=2)


# Or with ggplot but world plotting takes a long time
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
