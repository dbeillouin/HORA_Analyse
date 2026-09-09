requiredPackages <- c("tidyverse","tidymodels", "tinytex","tidytext","dplyr",
                      "ggplot2","magrittr", "textrecipes", "stopwords", "precrec",
                      "naivebayes", "readr", "readxl", "naivereg", "data.table",
                      "parsnip","discrim", "foreign", "utils", "xlsx", "magrittr",
                      "plyr", "ggpubr", "readxl","remotes", "rnaturalearth","rnaturalearthdata",
                      "sf","raster","spData","tmap","leaflet","ggplot2","ggcorrplot",
                      "lattice", "plotly", "hrbrthemes","forcats","fmsb","RColorBrewer",
                      "ellipse", "ggExtra", "cowplot","MortalityTables","plotbiomes",
                      "remotes","viridis","gridExtra","googledrive","measurements")

for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}

remotes::install_github("valentinitnelav/plotbiomes")

library(tidyverse)
library(tidymodels)
library(tinytex)
library(tidytext)
library(dplyr)
library(ggplot2)
library(magrittr)
library(textrecipes)
library(stopwords)
library(precrec)
library(naivebayes)
library(readr)
library(readxl)
library(naivereg)
library(data.table)
library(parsnip)
library(discrim)
library(foreign)
library(utils)
#library(xlsx)
library(magrittr)
library(plyr)
library(ggpubr)
library(readxl)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(raster)
library(spData)
library(tmap)
library(leaflet)
library(ggplot2)
library(ggcorrplot)
library(lattice)
library(plotly)
library(hrbrthemes)
library(forcats)
library(fmsb)
library(RColorBrewer)
library(ellipse)
library(ggExtra)
library(cowplot)
library(MortalityTables)
library(plotbiomes)
library(viridis)
library(gridExtra)
library(googledrive) #package pour charger les données directment depuis drive
library(measurements) #package permettant de convertir les coordonnées du formar degrès minute seconde au format décimal
library(readxl)

#setwd("~/Desktop")
setwd("D:/02_Bioversity/41_HORA/R")

#Chargement de la base de données
#DB_0 <- read.csv("~/Downloads/DB_HORA_final_V0 - Final.csv")
DB_0 <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")

#Transformation des coordonnées GPS
#Il y a deux formes de données GPS, celles au format 42,654563°N et celles au format 34°23'45,750W.Les premières nécéssitent uniquement des changements de caractères. Les secondes nécessites "measurments" pour passer de ddmmss,sss au format décimal. On sépare les cas via l'apostrophe (2 types d'apostrophes) et on met au format de la fonction du package measurments

CODE<-grepl("'|′",DB_0$latitude) #discrimination des deux cas

for(i in 1: length(DB_0$latitude)){
  
  if(CODE[i] == TRUE){
    
    #mise au format de measurments
    
    DB_0$latitude[i] <-gsub("°"," ",DB_0$latitude[i]) 
    DB_0$latitude[i] <-gsub("'"," ",DB_0$latitude[i])
    DB_0$latitude[i] <-gsub("′"," ",DB_0$latitude[i])
    DB_0$latitude[i] <-gsub(",",".",DB_0$latitude[i])
    
    DB_0$longitude[i] <-gsub("°"," ",DB_0$longitude[i])
    DB_0$longitude[i] <-gsub("'"," ",DB_0$longitude[i])
    DB_0$longitude[i] <-gsub("′"," ",DB_0$longitude[i])
    DB_0$longitude[i] <-gsub(",",".",DB_0$longitude[i])
    
    #ajout des - pour coordonnées S et W
    
    DB_0$latitude[i] <- if (grepl("S",DB_0$latitude)[i] == TRUE) {paste0("-",DB_0$latitude[i])}else {DB_0$latitude[i]} 
    DB_0$longitude[i]<- if (grepl("W",DB_0$longitude[i]) == TRUE) {paste0("-",DB_0$longitude[i])}else {DB_0$longitude[i]}
    
    #mise au format measurments
    
    DB_0$latitude[i] <-gsub("N","",DB_0$latitude[i])
    DB_0$latitude[i] <-gsub("S","",DB_0$latitude[i])
    DB_0$longitude[i] <-gsub("E","",DB_0$longitude[i])
    DB_0$longitude[i] <-gsub("W","",DB_0$longitude[i])
    
    #conversion des formats via measurments
    
    DB_0$longitude[i] <-conv_unit(DB_0$longitude[i], from = "deg_min_sec", to = "dec_deg")
    DB_0$latitude[i] <-conv_unit(DB_0$latitude[i], from = "deg_min_sec", to = "dec_deg")
    
  }
  
  else {
    
    #changement de caractères pour passer au format décimal complet
    
    DB_0$latitude[i] <-gsub("°","",DB_0$latitude[i])
    DB_0$latitude[i] <-gsub(",",".",DB_0$latitude[i])
    
    DB_0$longitude[i] <-gsub("°","",DB_0$longitude[i])
    DB_0$longitude[i] <-gsub(",",".",DB_0$longitude[i])
    
    #ajout des - pour coordonnées S et W
    
    DB_0$latitude[i] <- if (grepl("S",DB_0$latitude[i]) == TRUE) {paste0("-",DB_0$latitude[i])}else {DB_0$latitude[i]}
    DB_0$longitude[i]<- if (grepl("W",DB_0$longitude[i]) == TRUE) {paste0("-",DB_0$longitude[i])}else {DB_0$longitude[i]}
    
    #changement de caractères pour passer au format décimal complet
    
    DB_0$latitude[i] <-gsub("N","",DB_0$latitude[i])
    DB_0$latitude[i] <-gsub("S","",DB_0$latitude[i])
    DB_0$longitude[i] <-gsub("E","",DB_0$longitude[i])
    DB_0$longitude[i] <-gsub("W","",DB_0$longitude[i])
    
  }
  
}