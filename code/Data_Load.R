##### HORA Project
## Author: Damien Beillouin
## December 2023
# Aim: Analyse the caracteristic of horticultural-agroforestry systems

####### Initialisation ##########
#####

# Load Packages
library(stringr)        # String manipulation
library(readr)          # Read the archived CSV tables
library(dplyr)          # Data manipulation and transformation
library(tidyr)          # Reshaping
library(magrittr)       # Pipe operator (%>%)
# Packages loaded in earlier exploratory versions of this script and no longer
# required by the archived analysis: factoextra, gtools, Factoshiny, shiny,
# xml2, rvest, ggpubr.

# Load Data ------------------------------------------------------------------
# The two tables below are the study database, exported from the working
# spreadsheet and archived with this code so that the analysis is reproducible
# without any account or login.
#   data/species_characteristics.csv : one row per plant species (traits, uses,
#                                      woodiness, height, IUCN status, family)
#   data/experiments.csv             : one row per experiment (agroforestry vs
#                                      paired reference system)

# Location of the data/ folder. Works whether the working directory is the
# repository root or the code/ folder; set data_dir yourself to override.
if (!exists("data_dir") || !dir.exists(data_dir)) {
  data_dir <- if (dir.exists("data")) "data" else
              if (dir.exists(file.path("..", "data"))) file.path("..", "data") else
              stop("data/ not found - set data_dir to its path before sourcing this script.")
}
message("Reading the database from: ", normalizePath(data_dir))

CARACT <- readr::read_csv(file.path(data_dir, "species_characteristics.csv"),
                          show_col_types = FALSE) %>%
  mutate(NAME = tolower(NAME),
         Height_combined = as.numeric(Height_combined))

DATA <- readr::read_csv(file.path(data_dir, "experiments.csv"),
                        show_col_types = FALSE)

# Create a new data frame TAB_FINALE with relevant columns
TAB_FINALE <- DATA %>%
  mutate(NB_sp = str_count(`species treatment`, ',') + 1) %>%
  mutate(NB_spC = str_count(`species control`, ',') + 1) %>%
  dplyr::select(`New ID`, NB_sp, NB_spC,country,latitude,longitude, "numéro", "farm type", "Design", "Scale",'Intervention_reclass')
TAB_FINALE$NB_spC[is.na(TAB_FINALE$NB_spC)] <- 1

TAB_FINALE <- TAB_FINALE %>%
  mutate(Intervention_reclass = ifelse(Intervention_reclass %in% c("Alley-cropping","Alley cropping","alley cropping","Alley cropping, fallows","alley-cropping","alley-cropping/hedgerows"),"Alley cropping",
                                       ifelse(Intervention_reclass %in% c("multistata","multi strata","multistrata","Alley cropping/ complex multi-strata agroforestry","complex multi-strata-agroforestry systems","Alley cropping /complex mutli-strata agroforestry","Alley cropping, multistrata", "Complex multistrata systems","Alley-cropping, complex multi-strata systems",
                                                                          "multi-strata","multi-strata?","complex multi-strata agroforestry","complex multi-strata system","complex multi-strata-agroforestry systems/hedgerows", "Alley-cropping/ complex multi-strata systems","Complex multi-strata agroforestry"),"Multi-strata systems",
                                              ifelse(Intervention_reclass %in% c("Parkland","parkland?","parkland"),"Parkland",
                                                     ifelse(Intervention_reclass == "Herdgerows","Hedgerows",
                                                            ifelse(Intervention_reclass =="fallows", "Fallows",
                                                                   ifelse(Intervention_reclass=="Shaded systems","Other/unknown",Intervention_reclass)))))))

table(TAB_FINALE$Intervention_reclass)


# Combine sheet characteristics and Raw Database
DATA2 <- DATA %>%
  tidyr::separate_rows(`species treatment`, sep = ",", convert = TRUE) %>%
  mutate(`species treatment` = tolower(`species treatment`)) %>%
  dplyr::left_join(CARACT, by = c(`species treatment` = "NAME")) %>%
  dplyr::mutate(Woodiness = tolower(Woodiness)) %>%
  dplyr::select(c(`New ID`, "numéro", "Intervention_reclass", "Evergreen", "Herb", "Annual", "Woodiness", "Alim", "Medicinal.y", "Timber", "Part", "Woodiness", `species treatment`, "Height_combined", "N_Fixing", "IUCN", "cultivation_trop", `tree age treatment`, "Habit_Combined","Polli")) %>%
  dplyr::mutate(Height_class = cut(Height_combined, breaks = c(0, 1, 2.5, 5, 10, 60)))



###################################
## Some Initial Checks
###################################

# on vérifie que toutes les espèces sont ben caractérisées
List_species<- DATA %>%  tidyr::separate_rows(`species treatment`, sep = ",", convert = TRUE)
List_species<- unique(trimws(tolower(List_species$`species treatment`)))

List_caract<-unique(tolower(CARACT$NAME))
AA<-setdiff(List_species,List_caract)
BB<-setdiff(List_caract,List_species)

List_species_control<- DATA %>%  tidyr::separate_rows(`species control`, sep = ",", convert = TRUE)
List_species_control<- unique(trimws(tolower(List_species_control$`species control`)))
AA<-setdiff(tolower(List_species_control),tolower(List_caract))
#write.csv(AA, "control.csv")

BB<-setdiff(List_caract,List_species)

#write.csv(AA, 'AA.csv')
setdiff(tolower(CARACT$NAME),List_species)

# On vérfie que les ID sont bien uniques
VERIF<-data.frame(table(DATA$numéro))
VERIF<- VERIF %>% filter(Freq>1)



##### STEP 1: Check if all lines have horticultural crops (here: alim or medicinal)
######

ALIM<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Alim) %>%
  dplyr::group_by(`numéro`,Alim) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Alim,
                     values_from = "n") %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))
  ) %>%
  mutate_if(is.character, as.numeric)

Medic<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Medicinal.y) %>%
  dplyr::group_by(`numéro`,Medicinal.y) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Medicinal.y,
                     values_from = "n") %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))
  ) %>%
  mutate_if(is.character, as.numeric)

TOT<-full_join(ALIM,Medic,by = join_by(numéro))

# Identify row with no horticultural crops
TO_EXCLUDE<-TOT %>% filter(Alim<1)%>%
  filter(medicinal<1)

KEEP <- DATA2 %>% filter(!numéro%in%  TO_EXCLUDE$numéro)

TAB_FINALE <- TAB_FINALE %>%
  dplyr::filter(!numéro %in%  TO_EXCLUDE$numéro)
DATA <- DATA %>%
  dplyr::filter(!numéro %in%  TO_EXCLUDE$numéro)


## STEP 2 : Check if all lines have woody crops

Wood<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Woodiness,Annual)%>%
  dplyr::group_by(`numéro`,Woodiness,Annual) %>%
  dplyr::summarise(n=sum(n)) %>%
  dplyr::mutate(Woodiness= paste(Woodiness,Annual)) %>%
  dplyr::select(-Annual)%>%
  tidyr::pivot_wider(names_from = Woodiness,
                     values_from = "n") %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))
  ) %>%
  mutate_if(is.character, as.numeric)

TO_EXCLUDE2<-Wood %>% filter(`woody Perennial`<1)

TAB_FINALE <- TAB_FINALE %>%
  dplyr::filter(!numéro %in%  TO_EXCLUDE2$numéro)
DATA <- DATA %>%
  dplyr::filter(!numéro %in%  TO_EXCLUDE2$numéro)

# STEP 3: Check if all line have at least 2 species:
library(tidyr)
GROUP<-DATA %>%
  separate_longer_delim(`species treatment`, delim = ",")

VERIF<-data.frame(table(GROUP$numéro))
VERIF<- VERIF %>% filter(Freq<2)

TAB_FINALE <- TAB_FINALE %>%
  dplyr::filter(!numéro %in%  VERIF$Var1)
DATA <- DATA %>%
  dplyr::filter(!numéro %in%  VERIF$Var1)



##  STEP 4: Check if the species associations are OK to be considered as agroF-horticultural

## présence espèce Woody_horti
Woody<-CARACT  %>%
  mutate(Woodiness =tolower(Woodiness)) %>%
  filter(Woodiness %in% c("woody","liane")) %>%
  mutate(Alim_Med = paste(Alim, Medicinal)) %>%
  filter(!Alim_Med == "NA NA")
Woody_horti<-Woody$NAME

## présence espèce herb-woody
Woody<-CARACT  %>%
  mutate(Woodiness =tolower(Woodiness)) %>%
  filter(!Woodiness %in% c("woody","liane")) %>%
  mutate(Alim_Med = paste(Alim, Medicinal)) %>%
  filter(!Alim_Med == "NA NA")
herb_horti<-Woody$NAME

## présence espèce Woody_NONhorti
Woody<-CARACT  %>%
  mutate(Woodiness =tolower(Woodiness)) %>%
  filter(Woodiness %in% c("woody","liane")) %>%
  mutate(Alim_Med = paste(Alim, Medicinal)) %>%
  filter(Alim_Med == "NA NA")
Woody_NONhorti<-Woody$NAME

## préence espèce herb-NONwoody
Woody<-CARACT  %>%
  mutate(Woodiness =tolower(Woodiness)) %>%
  filter(!Woodiness %in% c("woody","liane")) %>%
  mutate(Alim_Med = paste(Alim, Medicinal)) %>%
  filter(Alim_Med == "NA NA")
herb_NONhorti<-Woody$NAME


###
GROUP<-DATA %>%
  separate_longer_delim(`species treatment`, delim = ",")

GROUP <- GROUP %>%
  mutate(herb_NONhorti = ifelse(tolower(`species treatment`) %in% herb_NONhorti, "HNE", NA),
         Woody_NONhorti = ifelse(tolower(`species treatment`) %in% Woody_NONhorti, "WNE", NA),
         herb_woody = ifelse(tolower(`species treatment`) %in% herb_horti, "HE", NA),
         Woody_horti = ifelse(tolower(`species treatment`) %in% Woody_horti, "WE", NA))

result <- GROUP %>%
  group_by(numéro) %>%
  summarise(
    Concatenation = paste(unique(na.omit(c(herb_NONhorti, Woody_NONhorti, herb_woody, Woody_horti))), collapse = ", ")
  )
table(result$Concatenation)

result <- result %>%
  mutate(Categorie = case_when(
    Concatenation %in% c("WNE, HE", "HNE, WNE, HE") ~ "(HNE), WNE, HE",
    Concatenation %in% c("HE, WE", "HNE, HE, WE") ~ "(HNE), HE, WE",
    Concatenation %in% c("WNE, WE", "HNE, WNE, WE") ~ "(HNE), WNE, WE",
    Concatenation %in% c("HNE, WNE, HE, WE", "WNE, HE, WE") ~ "(HNE), WNE, HE, WE",
    Concatenation %in% c("WE", "HNE, WE") ~ "(HNE), WE",
    TRUE ~ "Autre"
  ))
TAB_FINALE$numéro<-as.character(TAB_FINALE$numéro)
setdiff(TAB_FINALE$numéro,result$numéro)
setdiff(result$numéro,TAB_FINALE$numéro)

TAB_FINALE<- left_join(TAB_FINALE, result, by='numéro')

# #ON enlève les lignes qui ne correspondent pas
  data.frame(table(TAB_FINALE$Categorie))
#
 TO_EXCLUDE<-TAB_FINALE %>% filter(Concatenation %in% c("WE"))
#
 KEEP <- DATA %>% filter(numéro%in%  TO_EXCLUDE$numéro) %>%
   select(numéro, "species treatment", 'species control')


 TO_EXCLUDE <- TAB_FINALE %>%
   filter(Categorie %in% c("(HNE), WE"),NB_sp==1)

 # KEEP <- DATA %>% filter(numéro%in%  TO_EXCLUDE$numéro) %>%
 #   select(numéro, "species control","species treatment","NB")



## même chose pour le contrôle
GROUP_C<-DATA %>%
  separate_longer_delim(`species control`, delim = ",") %>%
  mutate(`species control` = tolower(`species control`))

GROUP_C <- GROUP_C %>%
  mutate(herb_NONhorti  = ifelse(tolower(`species control`) %in% herb_NONhorti, "HNE", NA),
         Woody_NONhorti = ifelse(tolower(`species control`) %in% Woody_NONhorti, "WNE", NA),
         herb_woody     = ifelse(tolower(`species control`) %in% herb_horti, "HE", NA),
         Woody_horti    = ifelse(tolower(`species control`) %in% Woody_horti, "WE", NA))
# head(GROUP_C$`species control`)
# head(GROUP_C$Woody_horti)

result_C <- GROUP_C %>%
  group_by(numéro) %>%
  summarise(
    Concatenation = paste(unique(na.omit(c(herb_NONhorti, Woody_NONhorti, herb_woody, Woody_horti))), collapse = ", ")
  )
table(result_C$Concatenation)

result_C <- result_C %>%
  mutate(Categorie = case_when(
    Concatenation %in% c("WNE, HE", "HNE, WNE, HE") ~ "(HNE), WNE, HE",
    Concatenation %in% c("HE, WE", "HNE, HE, WE") ~ "(HNE), HE, WE",
    Concatenation %in% c("WNE, WE", "HNE, WNE, WE") ~ "(HNE), WNE, WE",
    Concatenation %in% c("HNE, WNE, HE, WE", "WNE, HE, WE") ~ "(HNE), WNE, HE, WE",
    Concatenation %in% c("WE", "HNE, WE") ~ "(HNE), WE",
    Concatenation %in% c("WNE") ~ "WNE",
    Concatenation %in% c("HE") ~ "HE",
    Concatenation %in% c("HNE, WNE") ~ "HNE, WNE",
    TRUE ~ "Autre"
  ))

names(result_C)[c(2,3)]<- c("Concatenation_C","Categorie_C")

data.frame(table(result_C$Categorie_C))
TAB_FINALE$numéro<-as.character(TAB_FINALE$numéro)
TAB_FINALE<- left_join(TAB_FINALE, result_C, by='numéro')

# on va regarder les controles qui pourraient être des systèmes agroF-horticoles:
unique(TAB_FINALE$Categorie_C)


TO_EXCLUDE <- TAB_FINALE %>%
  filter(Categorie_C %in% c("(HNE), WNE, HE"))

KEEP <- DATA %>% filter(numéro%in%  TO_EXCLUDE$numéro) %>%
  select(numéro, "species control","species treatment")


TAB_FINALE <- TAB_FINALE %>%
  dplyr::filter(!numéro %in%  c("599_1", "599_4"))
DATA <- DATA %>%
  dplyr::filter(!numéro %in%  c("599_1", "599_4"))

TAB_FINALE <- TAB_FINALE %>%
  dplyr::filter(!numéro %in%  KEEP$numéro)
DATA <- DATA %>%
  dplyr::filter(!numéro %in%  KEEP$numéro)

# on va exclure les control forêts

# on va exclure de cette liste les systèmes qui comporte 2 espèces dont une poacées ( car on a dit que l'on ne considérait pas les poacées commes hoerticoles)
POACEES <-CARACT  %>% filter(Name_matched_accepted_family =="Poaceae")
POACEES<- unique(POACEES$NAME)

KEEP <- DATA %>% filter(Number_Total<2.5)
KEEP$poa <- sapply(strsplit(as.character(tolower(KEEP$`species treatment`)), ","), function(x) sum(x %in% POACEES))
KEEP<- KEEP %>% filter(poa>0.5)
KEEP$`species treatment`

### TODO : CEs lignes seront à regarder précisément
## pour l'instant je vais les virer

TAB_FINALE <- TAB_FINALE %>% filter(!numéro %in% KEEP$numéro)


#on recomence à coder une fois qu'on a fait le tri
table(TAB_FINALE$Concatenation_C)

TAB_FINALE <- TAB_FINALE %>%
  mutate(Categorie_C2 = case_when(
    Concatenation_C %in% c("WE") ~ "WE",
    Concatenation_C %in% c("HE") ~ "HE",
    Concatenation_C %in% c("WNE") ~ "WNE",
    Concatenation_C %in% c("HNE") ~ "HNE",
    TRUE ~ "Other"
  ))


###
table(TAB_FINALE$NB_sp)

(4512-(2660+1091+302))/4512
302/4512*100

FILTRE <-TAB_FINALE %>% filter(NB_sp>2.5)
table(FILTRE$Concatenation)
(49+22+49+63+106+424+507+178)/1853*100
(120+345+990)/ (557+120+345+648+990)
(23+2+1+11+3+22)/762*100

counts <- DATA2 %>%
  filter(numéro%in% TAB_FINALE$numéro) %>%
  group_by(numéro) %>%
  summarise(n = n())

complex_numbers <- counts$numéro[counts$n  >2.5]

complex_data <- DATA2 %>%
  filter(numéro %in% complex_numbers)
length(unique(complex_data$numéro))
length(unique(DATA2$numéro))
## 780 expé avec plus de 3 espèces.

result <- complex_data %>%
  filter(Woodiness == "woody" & Alim == "Alim")

FF<-data.frame(table(result$numéro))
FF<-FF %>% filter(Freq>2.5)
length(unique(FF$Var1))
  ## 205 expé avec au moins 3 woody alim

####

##Nombre de strates
Height<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Height_class) %>%
  tidyr::pivot_wider(names_from = Height_class,
                     values_from = "n") %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))
  ) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(NB_strates = rowSums(across(everything(), ~ . != 0))) %>%
  dplyr::select(numéro,NB_strates)

GG<- TAB_FINALE %>% select(Intervention_reclass, numéro)

STRATES<-left_join(Height, GG) %>%
  filter(numéro %in% TAB_FINALE$numéro) %>%
  mutate(cutStrates= cut(NB_strates, breaks=c(0.9,2.9,99))) %>%
  group_by(cutStrates, Intervention_reclass) %>%
  count()


##### Nombre of species, family

DATAT <- DATA %>%
  mutate(NB_sp = str_count(`species treatment`, ',') + 1) %>%
  tidyr::separate_rows(`species treatment`, sep = ",", convert = TRUE) %>%
  mutate(`species treatment` = tolower(`species treatment`)) %>%
  dplyr::left_join(CARACT, by = c("species treatment" = "NAME")) %>%  # Corriger les guillemets autour de species control
  dplyr::mutate(Woodiness = tolower(Woodiness)) %>%
  dplyr::select(c(`New ID`, "numéro",`farm type`, `species treatment`, "IUCN",'NB_sp',`Name_matched_accepted_family`)) %>%
  filter(numéro %in% TAB_FINALE$numéro)

FF<-data.frame(NAME=unique(DATAT$`species treatment`))

length(unique(DATAT$`New ID`))
length(unique(DATAT$numéro))

##Nombre d'espèces avec données IUCN
IU<-CARACT %>% filter(is.na(Notes)) %>%
  filter(NAME %in% FF$NAME)
table(IU$IUCN)

FF<- FF %>% left_join(CARACT)

FAMILY_T<-data.frame(table(FF$Name_matched_accepted_family))
FAMILY_T$type= 'treat'

FF$Alim_med<-paste(FF$Alim,FF$Medicinal)

table(FF$Woodiness)
table(FF$Alim_med)

FFF<-FF %>% filter(!Alim_med== "NA NA")
table(FFF$Woodiness)

# pour le control

DATAC <- DATA %>%
  mutate(NB_spC = str_count(`species control`, ',') + 1) %>%
  mutate(NB_sp = str_count(`species treatment`, ',') + 1) %>%
  tidyr::separate_rows(`species control`, sep = ",", convert = TRUE) %>%
  mutate(`species control` = tolower(`species control`)) %>%
  dplyr::left_join(CARACT, by = c("species control" = "NAME")) %>%  # Corriger les guillemets autour de species control
  dplyr::mutate(Woodiness = tolower(Woodiness)) %>%
  dplyr::select(c(`New ID`, "numéro", `species control`, "IUCN",'NB_spC',"NB_sp","Name_matched_accepted_family")) %>%
  filter(`New ID` %in% TAB_FINALE$`New ID`)


FF<-data.frame(NAME=unique(DATAC$`species control`))

FF<- FF %>% left_join(CARACT)

FF$Alim_med<-paste(FF$Alim,FF$Medicinal)

FAMILY_C<-data.frame(table(FF$Name_matched_accepted_family))
FAMILY_C$type= 'control'

FAMILY<-rbind(FAMILY_T,FAMILY_C)

SELECT <- FAMILY_T %>%
  arrange(desc(Freq)) %>% head(15)

# Sélectionner les 10 premières familles
FAMILY <- FAMILY %>% filter(Var1 %in% SELECT$Var1)

ggplot(FAMILY, aes(x = reorder(Var1,Freq), y = Freq, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Famille", y = "Fréquence", fill = "Traitement", title = "") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_pubr()+
  coord_flip()


FAMILY<-left_join(FAMILY_T,FAMILY_C, by='Var1')
FAMILY$ratio<-FAMILY$Freq.x/FAMILY$Freq.y



table(FF$Woodiness)
table(FF$Alim_med)

FFF<-FF %>% filter(!Alim_med== "NA NA")
table(FFF$Woodiness)

#write.csv(TAB_FINALE,"TAB_FINALE_TRANSFERT_SARAH.csv")
