## Script to map data points and make whittaker graph
## Damien Beillouin
## 21 march 2023

library(mapproj)

# import lookup table of country-regions
databasis_region <- read_excel("./databasis_region.xlsx")

# format data for mapping
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
country <- data.frame(world$geounit)
#write.csv(country,'country_region.csv')
DB1_country <- data.frame(left_join(DB_0,databasis_region,by=c('country'='geounit')))

#### Map nombre d'articles par régions ####

DB1_country_unique<- DB1_country %>%
  distinct(New.ID,latitude,longitude, .keep_all = TRUE)

occ_region <- data.frame(table(DB1_country_unique$region))
occ_geounit <- data.frame(table(DB1_country_unique$country))

names(occ_geounit)[1] <- 'geounit'
names(occ_geounit)[2] <- 'no_articles'
occ_geounit$no_articles2 <- cut(occ_geounit$no_articles,breaks=c(as.numeric(0),as.numeric(1),as.numeric(5),as.numeric(10),as.numeric(20),as.numeric(30),as.numeric(50),as.numeric(70)))

names(occ_geounit)[1] <- 'country'
names(occ_geounit)[2] <- 'no_articles'
world$geounit <-tolower(world$geounit)
occ_geounit$countyr <- tolower(occ_geounit$country)

# we change the names of the geounit to make them match between the 2 table

world$geounit<-plyr::revalue(world$geounit,c("united kingdom"="uk","united states of america"="usa"))
setdiff(world$geounit,occ_geounit$country)

# merging map background and data

T0world2<-merge(world,occ_geounit, all=TRUE)
world2 <- T0world2 #%>% select(geounit,no_articles)

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
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

names(occ_geounit)[1]<-'region'
world$region<-tolower(world$region)
worldSubset <- left_join(world, occ_geounit, by = "region")

ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  #coord_fixed(1.3) +
  coord_map("mollweide")+
  geom_polygon(aes(fill = no_articles)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  ggtitle("What a beautiful map") +
  plain

DB_0$latitude<-as.numeric(DB_0$latitude)
DB_0$longitude<-as.numeric(DB_0$longitude)

ggplot() +
  geom_map(
    data = worldSubset %>% filter(!(region %in% c("antarctica"))), map = world,
    aes(long, lat, map_id = region, fill= no_articles ), color = "black", size = 0.1
  ) +
  coord_map("mollweide")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "None")+
  geom_point(
    data = DB_0,
    shape=21,
    fill= "purple",
    aes(longitude, latitude),
    alpha = 0.7)+
  scale_fill_gradient2(na.value="white" )

#+
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
