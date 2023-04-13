## Script to map data points and make whittaker graph
## Damien Beillouin, Sarah Jones
## 21 march 2023 last updated 13 April 2023

## Load Packages
library(mapproj)
library(magrittr)
library(googlesheets4)
library(readxl)
library(igraph)
library(tidyverse)
library(measurements)
library(Ternary)

setwd("D:/02_Bioversity/41_HORA/R")

# import files 
kew <- read_xlsx("./FoodPlants_SOTWPF2020.xlsx",sheet=1)
d <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")
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
                          ifelse(source_id == 4619,"China",country))) 

d <- d %>%   left_join(unsd,by=c("country"="Country.or.Area")) %>%rename("Region"="Region.Name") 

d <- d %>% 
  mutate(`farm size` = as.character(`farm size`),
         `Duration of study` = as.character(`Duration of study`),
         soil = as.character(soil),
         `tree age treatment` = as.character(`tree age treatment`),
         `fertiliser N treatment` = as.character(`fertiliser N treatment`),
         `Tree age control` = as.character(`Tree age control`),
         `fertiliser N control` = as.character(`fertiliser N control`),
         `tillage control` = as.character(`tillage control`),
         `raw density` = as.character(`raw density`),
         `row N treatment` = as.character(`row N treatment`),
         `row N control` = as.character(`row N control`))

# convert lat and long to decimal degrees
# from either decimal degrees with a comma instead of point separation (format 42,654563°N)
# or degrees, minutes, seconds (format 34°23'45,750W or 34°23’45,750W or 34°23′45,750W)
# We use the measurement package to convert the second type.

d <- d %>% mutate(latitude = ifelse(latitude=="NA",NA,latitude),
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

#### Map number of experiments per region ####

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#ne_country <- data.frame(country_ne=world$region) %>% unique()
#d <- d %>% left_join(ne_country,by=("country"="country_ne"))
#d_id_unique <- d %>% distinct(source_id,latitude,longitude, .keep_all = TRUE)

# drawing the map
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
  #coord_fixed(1.3)+
  coord_map("mollweide")+
  plain
worldplot

d$latitude<-as.numeric(d$latitude)
d$longitude<-as.numeric(d$longitude)

names(d)
d_map <- d %>% group_by(source_id,`Intervention in our classification`,latitude,longitude) %>%
  summarise(n_experiments = n()) %>%
  rename("treatment" = "Intervention in our classification") %>%
  filter(!(is.na(latitude))&!(is.na(longitude))) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

world <- world %>% filter(region !="Antarctica")

g <- ggplot() + 
  geom_polygon(data = world,
               aes(x=long, y = lat, group = group), 
           fill="white",color = "black", 
           linewidth = 0.1) +
  #coord_sf(default_crs =sf::st_crs(4326))+
  coord_map("mollweide")+ # azequalarea
  #coord_fixed(1.3)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "None") +
  geom_point(data = arrange(d_map,desc(n_experiments)), 
             aes(x=longitude,y=latitude,colour=treatment,size=n_experiments),
             alpha = 0.7,
             shape=20)+
  scale_colour_viridis_d(7)+
  scale_size_continuous(breaks=c(1,5,10,50),range=c(2,10))

g

library(cowplot)
legend <- get_legend(g+theme(legend.position="bottom"))

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
