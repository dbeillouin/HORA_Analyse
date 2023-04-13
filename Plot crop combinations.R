# Script to make network diagram of crop combinations #
# Sarah Jones 
# 21 March 2023 updated 12 April 2023

# Useful references: 
# https://r.igraph.org/articles/igraph.html
# https://stackoom.com/en/question/3bnLB

## Load Packages
library(magrittr)
library(googlesheets4)
library(readxl)
library(igraph)
library(tidyverse)

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
  
#temp <- d %>% filter(is.na(country))

### Descriptive stats ####
names(d)
length(unique(d$source_id))
ggplot(d,aes(x=Region,fill=country))+ geom_bar() + theme_classic()
round(proportions(table(d$Region))*100,1)
ggplot(d,aes(x=Region))+ geom_bar() + theme_classic()

### Network diagram ####

# get list of crop species combinations in treatments
# count each combination once per article and country
names(d)

crop_comb <- d %>% dplyr::select(c(`species treatment`,source_id,country,Region,Region.Code)) %>%
  rename("species_hora_comb"=`species treatment`) %>%
  mutate(species_hora_comb = gsub(",","-",species_hora_comb)) %>%
  mutate(species_hora_comb_region = paste0(species_hora_comb," ",Region.Code)) %>%
  unique() %>%
  select(-c(source_id)) %>%
  group_by_at("species_hora_comb") %>% 
  mutate(freq=n()) %>% ungroup() %>%
  group_by_at("species_hora_comb_region") %>%
  mutate(freq_region = n()) %>% ungroup()

# classify crops by taxonomy and trait 

crop_split <- function(level="global"){
  
  if(level!="global"){
    crop_comb <- crop_comb %>% filter(Region %in% c(level))
  }
  
  crops <- str_split(crop_comb$species_hora_comb, pattern = "\\-|\\ × ") 

  crops_unique <- data.frame("species_hora" = unlist(crops)) %>% unique()

  crops_unique <- crops_unique %>%
    mutate(species_hora = trimws(species_hora, "both", "[ \t\r\n]")) %>%
    mutate(species_hora = ifelse(species_hora == " Olea europaea subsp. Cuspidata","Olea europaea subsp. Cuspidata",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora == "Corchorus olotorius","Corchorus olitorius",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora == "Casuarina cunninghamania","Casuarina cunninghamiana",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora == "Vicia Faba","Vicia faba",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora == "Pueraria Phaseoloides","Pueraria phaseoloides",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora %in% c("choudae","Ficus vallis"),"Ficus vallis-choudae",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora %in% c("indica"),"Cannabis sativa subsp. indica",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora %in% c("Persea Americana"),"Persea americana",species_hora)) %>%
    mutate(species_hora = ifelse(species_hora %in% c("Inga rodrigueziana"),"Inga sapindoides",species_hora)) %>% # https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:128714-2
    mutate(species_hora = ifelse(species_hora %in% c("Elusine coracana"),"Eleusine coracana",species_hora))  # https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:128714-2

  crops_unique <- crops_unique %>% left_join(list_species,by=c("species_hora"="NAME"),multiple="first") %>%
    mutate(Unmatched_terms = as.character(Unmatched_terms),
           Height = as.character(Height),
           Growth = as.character(Growth),
           hadiness = as.character(hadiness),
           Edible_pfaf = as.character(Edible_pfaf),
           alim_trop = as.character(alim_trop),
           alim_combined = as.character(alim_combined),
           Medicinal_M = as.character(Medicinal_M),
           medi_trop = as.character(medi_trop),
           medi_combined = as.character(medi_combined),
           Other = as.character(Other),
           other_trop = as.character(other_trop),
           other_combined = as.character(other_combined)) %>%
    mutate(usage_alim = ifelse(alim_combined>0,1,0),
           usage_medi = ifelse(medi_combined>0,1,0),
           usage_other = ifelse(other_combined>1,1,0))

  crops_unique <- crops_unique %>% mutate(Woodiness = ifelse(is.na(Woodiness),"Non-woody",Woodiness)) 

  missing <- crops_unique %>% filter(is.na(Name_matched_accepted_family)) %>% select(species_hora)
  #missing # not included yet in the list_speciesDB tab - need adding
  #write.csv(missing,"missing.csv",row.names=FALSE)

  crops_unique <- crops_unique %>%  
    left_join(kew[,c("speciesname", "binomial_acc_name")], by=c("species_hora" = "speciesname")) %>%
    mutate(species_hort = ifelse(!is.na(binomial_acc_name),1,0)) %>% 
    select(-c(binomial_acc_name)) 

  crops_unique <- crops_unique %>%
    mutate(Life_cycle = ifelse(Habit_Combined %in% c("Annual","Annual Climber","Annual, Not climbing, Herb","Annual/Biennial","Annual/Perennial","Bulb","Herb","Herbaceous","Herbs, shrubs, twiners or (rarely) trees"),"Annual","Perennial or biannual"))

  crops_unique <- crops_unique %>%
    mutate(fao_group = ifelse(is.na(fao_group)|fao_group %in% c("#N/A",""),"Other",fao_group))
 
  print("horticultural (1) or not (0):")
  print(table(crops_unique$species_hort))
  
  print("life cycle and fao group for horticultural crops:")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==1,]$fao_group,crops_unique[crops_unique$species_hort ==1,]$Life_cycle)))
  
  print("usage for horticultural crops:")
  print("food")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==1,]$usage_alim)))
  print("medicinal")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==1,]$usage_medi)))
  print("other")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==1,]$usage_other)))
  print(addmargins(table(crops_unique[crops_unique$species_hort ==1,]$cultivation_trop)))
  
  print("life cycle and fao group for non-horticultural crops:")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==0,]$fao_group,crops_unique[crops_unique$species_hort ==0,]$Life_cycle)))
  
  print("usage for non-horticultural crops:")
  print("food")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==0,]$usage_alim)))
  print("medicinal")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==0,]$usage_medi)))
  print("other")
  print(addmargins(table(crops_unique[crops_unique$species_hort ==0,]$usage_other)))
  print(addmargins(table(crops_unique[crops_unique$species_hort ==0,]$cultivation_trop)))

  # make edge list of crop combinations
  edge_list <- do.call(rbind,lapply(crops,function(convert){t(combn(convert,2))}))
  # rename columns
  edge_list <- data.frame(from = factor(edge_list[,1]),to = factor(edge_list[,2]))
  # add freq column
  edge_list <- edge_list %>%
    mutate(species_hora_comb = paste0(from,",",to)) %>%
    group_by_at("species_hora_comb") %>% mutate(freq=n()) #%>% unique()
  
  edge_family <- edge_list[,c("from","to","freq")] %>% 
    left_join(crops_unique[,c("species_hora","Name_matched_accepted_family")],by=c("from"="species_hora"),multiple="all") %>%
    rename("from_family"="Name_matched_accepted_family") %>%
    left_join(crops_unique[,c("species_hora","Name_matched_accepted_family")],by=c("to"="species_hora"),multiple="all") %>%
    rename("to_family"="Name_matched_accepted_family") %>%
    select(-c("to","from")) %>% na.omit %>% 
    filter(!(from_family =="NA")) %>% filter(!(to_family =="NA")) %>%
    rename("to"="to_family","from" = "from_family") %>%
    mutate(species_hora_comb = paste0(from,",",to)) %>%
    group_by_at("species_hora_comb") %>% mutate(freq=n()) %>% ungroup() %>%
    select(c("to","from","freq")) 
  
  edge_family_woody <- edge_list[,c("from","to","freq")] %>% 
    left_join(crops_unique[,c("species_hora","Name_matched_accepted_family","Woodiness")],by=c("from"="species_hora"),multiple="all") %>%
    rename("from_family"="Name_matched_accepted_family",
           "from_woodiness"="Woodiness") %>%
    left_join(crops_unique[,c("species_hora","Name_matched_accepted_family","Woodiness")],by=c("to"="species_hora"),multiple="all") %>%
    rename("to_family"="Name_matched_accepted_family",
           "to_woodiness" = "Woodiness") %>%
    select(-c("to","from")) %>% na.omit %>% 
    filter(!(from_family =="NA")) %>% filter(!(to_family =="NA")) %>%
    rename("to"="to_family","from" = "from_family") %>%
    mutate(to = paste0(to, " ",substr(to_woodiness,1,1)),
           from = paste0(from, " ", substr(from_woodiness,1,1))) %>%
    mutate(species_hora_comb = paste0(from,",",to)) %>%
    group_by_at("species_hora_comb") %>% mutate(freq=n()) %>% ungroup() %>%
    select(c("to","from","freq"))
  
  assign(paste0("crops_",level),crops, envir = globalenv())
  assign(paste0("crops_unique_",level),crops_unique, envir = globalenv())
  assign(paste0("edge_list_",level),edge_list, envir = globalenv())
  assign(paste0("edge_family_",level),edge_family, envir = globalenv())
  assign(paste0("edge_family_woody_",level),edge_family_woody, envir = globalenv())
}
unique(d$Region)
crop_split(level="global")
crop_split(level="Africa")
crop_split(level="Asia")
crop_split(level="Americas")
crop_split(level="Europe")
crop_split(level="Oceania")


#### make network diagram (igraph) ####

# Arrange plots
# https://statisticsglobe.com/r-layout-function-arrange-plots/
dev.off()

# species level ####
tiff("Network species by region.tif",width=180,height=80,units="mm",compression="lzw",res=300)
jpeg("Network species by region.jpg",res=300,width=180,height=80,units="mm")

layout_matrix_1 <- matrix(1:3, ncol = 3)
layout_matrix_1 
layout(layout_matrix_1)
par(xpd=TRUE) 

x <- graph.data.frame(d=edge_list_Africa[c(edge_list_Africa$freq>9),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% left_join(crops_unique_Africa,by=c("vertex"="species_hora"))
V(x)$species_hort <- v$species_hort
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="Woody","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>14,12,8))
E(x)$weight <- edge_attr(x)$freq
x <- igraph::simplify(x, edge.attr.comb=list(weight="median", "first"),remove.loops=FALSE)
V(x)$label <- str_wrap(V(x)$name,12)
plot(x,main="Africa",vertex.size=V(x)$size,edge.width=E(x)$freq/2,color=V(x)$vertex.color,layout=layout_as_star,vertex.label.dist=3.5)

x <- graph.data.frame(d=edge_list_Americas[c(edge_list_Americas$freq>9),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% left_join(crops_unique_Asia,by=c("vertex"="species_hora"))
V(x)$species_hort <- v$species_hort
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="Woody","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>14,12,8))
E(x)$weight <- edge_attr(x)$freq
x <- igraph::simplify(x, edge.attr.comb=list(weight="median", "first"),remove.loops=FALSE)
V(x)$label <- str_wrap(V(x)$name,12)
plot(x,main="Americas",vertex.size=V(x)$size,edge.width=E(x)$freq/2,color=V(x)$vertex.color,layout=layout_as_star,vertex.label.dist=3.5)
#legend_1 <- legend(title="Plant combination\nfrequency",'top',inset=c(-0.1,1),legend=c("10-14","15-19",">20"),pt.cex=c(8/10,12/10,20/10), col='black',pch=21, pt.bg='grey',bty="n")
legend_1 <- legend(title="Plant combination\nfrequency",'top',inset=c(-0.1,1),legend=c("10-14","15-19",">20"),lwd=c(8/2,12/2,20/2), col='grey',bty="n")

x <- graph.data.frame(d=edge_list_Asia[c(edge_list_Asia$freq>9),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% left_join(crops_unique_Asia,by=c("vertex"="species_hora"))
V(x)$species_hort <- v$species_hort
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="Woody","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>14,12,8))
E(x)$weight <- edge_attr(x)$freq
x <- igraph::simplify(x, edge.attr.comb=list(weight="median", "first"),remove.loops=FALSE)
V(x)$label <- str_wrap(V(x)$name,12)
plot(x,main="Asia",vertex.size=V(x)$size,edge.width=E(x)$freq/2,color=V(x)$vertex.color,layout=layout_as_star,vertex.label.dist=3.5)
legend_2 <- legend(title="Plant woodiness", 'top',inset=c(-0.1,1),legend=c("Woody","Non-woody"),pt.cex=c(12/10), col="black",pch=21, pt.bg=c('forestgreen',"yellow"),bty="n")

dev.off()

x <- graph.data.frame(d=edge_list_Europe[c(edge_list_Europe$freq>2),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% left_join(crops_unique_Asia,by=c("vertex"="species_hora"))
V(x)$species_hort <- v$species_hort
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="Woody","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>14,12,8))
E(x)$weight <- edge_attr(x)$freq
x <- igraph::simplify(x, edge.attr.comb=list(weight="median", "first"),remove.loops=FALSE)
V(x)$label <- str_wrap(V(x)$name,12)
plot(x,main="Europe",vertex.size=V(x)$size,edge.width=E(x)$freq/2,color=V(x)$vertex.color,layout=layout_nicely,vertex.label.dist=3.5)

# family level ####
x <- graph.data.frame(d=edge_family_Africa[c(edge_family_Africa$freq>9),],directed=FALSE)
V(x)$label <- str_wrap(V(x)$name,12)
V(x)$color <- "grey80"
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>9,12,8))
plot(x,vertex.size=V(x)$size,layout=layout_nicely,vertex.label.dist=2.5)

#tkplot(x) # interactive
#tk_off()

jpeg("Network family by region.jpg",res=120,width=200,height=120,units="mm")

layout_matrix_1 <- matrix(1:2, ncol = 2)
layout_matrix_1 
layout(layout_matrix_1)
par(xpd=TRUE) 

# family woodiness level ####
x <- graph.data.frame(d=edge_family_woody_Africa[c(edge_family_woody_Africa$freq>9),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% 
  mutate(label = substr(vertex,1,nchar(vertex)-2),
         Woodiness = substr(vertex,nchar(vertex),nchar(vertex)))
V(x)$label <- v$label
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="W","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>14,12,8))
plot(x,main="Africa",vertex.size=V(x)$size,edge.color="grey",color=V(x)$vertex.color,layout=layout_with_kk,vertex.label.dist=2.5)
legend_1 <- legend(title="Plant combination\nfrequency",'bottomright',inset=c(-0.1,-0.3),
                   legend=c("10-14","15-19","20-82"),pt.cex=c(8/5,12/5,20/5), col='black',pch=21, 
                   pt.bg='grey',y.intersp=c(1,1.2,1.3), x.intersp=1,bty="n")

x <- graph.data.frame(d=edge_family_woody_Asia[c(edge_family_woody_Asia$freq>9),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% 
  mutate(label = substr(vertex,1,nchar(vertex)-2),
         Woodiness = substr(vertex,nchar(vertex),nchar(vertex)))
V(x)$label <- v$label
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="W","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>19,20,ifelse(degree(x)>9,14,8))
plot(x,main="Asia",vertex.size=V(x)$size,edge.color="grey",color=V(x)$vertex.color,layout=layout_with_kk,vertex.label.dist=2.5)
legend_2 <- legend(title="Plant woodiness",'bottomleft',inset=c(-0.1,-0.3),
                   legend=c("Woody","Non-woody"),pt.cex=c(14/5), col="black",pch=21, 
                   pt.bg=c('forestgreen',"yellow"),y.intersp=c(1.2), x.intersp=1,bty="n")

dev.off()


# Global 

tiff("Network family global.tif",width=200,height=170,units="mm",compression="lzw",res=300)
jpeg("Network family global.jpg",width=200,height=170,units="mm",res=150)

layout_matrix_1 <- matrix(1:1, ncol = 1)
layout(layout_matrix_1)
par(xpd=TRUE) 

x <- graph.data.frame(d=edge_family_woody_global[c(edge_family_woody_global$freq>19),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% 
  mutate(label = substr(vertex,1,nchar(vertex)-2),
         Woodiness = substr(vertex,nchar(vertex),nchar(vertex)))
V(x)$label <- v$label
V(x)$Woodiness <- v$Woodiness
V(x)$color <- ifelse(V(x)$Woodiness =="W","forestgreen","yellow")
V(x)$size <- ifelse(degree(x)>49,20,ifelse(degree(x)>34,12,8))
plot(x,main="Global",vertex.size=V(x)$size,edge.color="grey",color=V(x)$vertex.color,layout=layout.auto,vertex.label.dist=2.7)
legend_1 <- legend(title="Plant combination\nfrequency",'bottomleft',inset=c(-0.1,0),
                   legend=c("20-34","35-49","50-237"),pt.cex=c(8/5,12/5,20/5), col='black',pch=21, 
                   pt.bg='grey',y.intersp=c(1.2,1.3,1.4), x.intersp=1.1,bty="n")

legend_2 <- legend(title="Plant woodiness",'bottomleft',inset=c(-0.1,0.3),
                   legend=c("Woody","Non-woody"),pt.cex=c(14/5), col="black",pch=21, 
                   pt.bg=c('forestgreen',"yellow"),y.intersp=c(1.2), x.intersp=1.1, bty="n")

dev.off()

#plot(x,vertex.size=V(x)$size,edge.color="grey",color=V(x)$vertex.color,layout=layout_on_sphere)
#legend(title="Combination\nfrequency",title.adj=0,title.cex=0.8,'bottomleft',inset=c(0.01,0.01),legend=c("1-9","10-19","20-50"),pt.cex=c(8/4,15/4,20/4),y.intersp=c(1.5,1.5,2.5), x.intersp=1.5,col='black',pch=21, pt.bg='grey',bty="n",border=NULL)


# Make one graph per region
# make sure all points are labelled, and wrap labels when long
# Make network by FAO group...but for this all species need correctly classifying
