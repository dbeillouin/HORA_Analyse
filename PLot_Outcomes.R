## Script to analyse the Outcome
## Damien Beillouin
## 18 march 2023

## Load Packages
library(ggplot2)
library(ggpubr)
library(magrittr)
library(googlesheets4)

#Load Data

x <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0', sheet = "Articles caractérisés")

# List all outcomes of interest
Outcomes <- c("Biodiversity","Yields","Financial returns", "Soil",
              "Carbon", "Water","Crop traits", "Tree traits",
              "Pest and disease","Other")

# Refine the Dataframe
TAB <- x %>% dplyr::select("New ID","numéro", all_of(Outcomes))

## Supress details within each outcome (could be analysed later)
TAB %<>%  mutate(Biodiversity = if_else(is.na(Biodiversity), 0, 1))
TAB %<>%  mutate(Yields = if_else(is.na(Yields), 0, 1))
TAB %<>%  mutate(`Financial returns` = if_else(is.na(`Financial returns`), 0, 1))
TAB %<>%  mutate(Soil = if_else(is.na(Soil), 0, 1))
TAB %<>%  mutate(Carbon = if_else(is.na(Carbon), 0, 1))
TAB %<>%  mutate(Water = if_else(is.na(Water), 0, 1))
TAB %<>%  mutate(`Crop traits` = if_else(is.na(`Crop traits`), 0, 1))
TAB %<>%  mutate(`Tree traits` = if_else(is.na(`Tree traits`), 0, 1))
TAB %<>%  mutate(`Pest and disease` = if_else(is.na(`Pest and disease`), 0, 1))
TAB %<>%  mutate(Other = if_else(is.na(Other), 0, 1))

# Melt the Dataframe
melt_TAB<- data.table::melt(TAB, id= c("New ID", "numéro"))
# Supress row with no studied combinations
melt_TAB %<>% dplyr::filter(!value ==0)

# Make a dataframe with all possible combination of the outcomes
ALL_combi<- expand.grid(Outcomes,Outcomes)
names(ALL_combi)<- c("var1","var2")

for (i in 1:length(GG$var1)){
  ID1<- melt_TAB %>% filter(variable == ALL_combi$var1[i])
  ID2<- melt_TAB %>% filter(variable == ALL_combi$var2[i])
  ALL_combi$COUNT[i]<-length(intersect(ID1$numéro,ID2$numéro))
  ALL_combi$COUNT_unique[i]<-length(intersect(ID1$`New ID`,ID2$`New ID`))
}

# calculate the proportion of experiments/articles
ALL_combi$COUNT_prop<- ALL_combi$COUNT/ length(melt_TAB$numéro)*100
ALL_combi$COUNT_unique_prop<- ALL_combi$COUNT_unique/ length(unique(melt_TAB$`New ID`))*100

ggplot(ALL_combi, aes(var1,var2)) +
  geom_tile(aes(fill = COUNT_unique_prop))+
  scale_fill_gradient(low = "white", high = "red")+
  geom_text(aes(label = round(COUNT_unique_prop, 1)),size=3)+
  # facet_grid(Land_use~., scales="free",space = "free")+
  #scale_fill_manual(values = c('#b2182b','#fddbc7','#d1e5f0','white','#2166ac'))+
  theme_pubr()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=8))

