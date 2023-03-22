# Script to make network diagram of crop combinations #
# Sarah Jones 
# 21 March 2023

# Useful references: 
# https://r.igraph.org/articles/igraph.html
# https://stackoom.com/en/question/3bnLB

## Load Packages
library(magrittr)
library(googlesheets4)
library(readxl)
library(igraph)

setwd("D:/02_Bioversity/41_HORA/R")

# import files 
kew <- read_xlsx("./FoodPlants_SOTWPF2020.xlsx",sheet=1)
DB_0 <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")

# get list of crop species combinations in treatments
names(DB_0)
crop_comb <- DB_0 %>% dplyr::select(c(`species treatment`,source_id)) %>%
  dplyr::rename("species_hora_comb"=`species treatment`) %>%
  mutate(species_hora_comb = gsub(",","-",species_hora_comb)) %>% unique() %>%
  dplyr::select(-c(source_id)) %>%
  group_by_at("species_hora_comb") %>% count()
  
# classify crops as horticultural or not
crops <- str_split(crop_comb$species_hora_comb, pattern = "-") 
crops_unique <- data.frame("species_hora" = unlist(crops)) %>% unique() %>%
  left_join(kew[,c("speciesname", "binomial_acc_name")], by=c("species_hora" = "speciesname")) %>%
  mutate(species_hort = ifelse(!is.na(binomial_acc_name),1,0)) %>% 
  dplyr::select(-c(binomial_acc_name)) 

table(crops_unique$species_hort)

crops_hort <- crops_unique %>% filter(species_hort ==1)

# make edge list of crop combinations
edge_list <- do.call(rbind,lapply(crops,function(convert){t(combn(convert,2))}))
# rename columns
edge_list <- data.frame(from = factor(edge_list[,1]),to = factor(edge_list[,2]))
# add freq column
edge_list <- edge_list %>%
  mutate(species_hora_comb = paste0(from,",",to)) %>%
  group_by(species_hora_comb) %>% count() %>% #mutate(freq=n()) 
  left_join(crops_unique,by=c("from"="species_hora"))

# make adjacency matrix
adj.mat <- table(edge_list[,1:2])
#adj.mat[lower.tri(adj.mat)] <- adj.mat[upper.tri(adj.mat)]

#### make network diagram (igraph) ####

# examples
x <- make_graph(edges=c(1,2,1,5),n=10, directed = FALSE)
plot(x)

x <- make_tree(127,2,mode="undirected")
plot(x)

# real thing

x <- graph.data.frame(d=edge_list[c(edge_list$freq>4),],directed=FALSE)
v <- data.frame(vertex = V(x)$name) %>% left_join(crops_unique,by=c("vertex"="species_hora"))
V(x)$species_hort <- v$species_hort
V(x)$color <- ifelse(V(x)$species_hort ==1,"steelblue","grey80")
plot(x,vertex.size=10,layout=layout_on_sphere)
plot(x,vertex.size=10,layout=layout_in_circle)
plot(x,vertex.size=10,layout=layout_with_kk)
plot(x,vertex.size=10,layout=layout_nicely, vertex.label.dist=3.5)

tiff("Network 1.tif",width=20,height=20,units="cm",compression="lzw",res=300)
plot(x,vertex.size=10,layout=layout_nicely, vertex.label.dist=3.5)
dev.off
# NEXT: get vertex size to frequency of occurrence # vertex_size
# Get line width to match freqency of combination #edge_width in pixels
# Make one graph per region
# make sure all points are labelled, and wrap labels?