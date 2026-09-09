##### HORA Project
## Author: Damien Beillouin
## December 2023
# Aim: Analyse the caracteristic of horticultural-agroforestry systems

####### Initialisation ##########
#####

# Load Packages
library(stringr)        # String manipulation functions
library(factoextra)    # Visualization and clustering functions for factor analysis
library(gtools)         # Various R programming tools
library(Factoshiny)     # Shiny application for exploratory factor analysis
library(shiny)          # Web application framework for interactive data visualization
library(googlesheets4)  # Interface with Google Sheets API v4
library(gsheet)         # Interact with Google Sheets
library(dplyr)          # Data manipulation and transformation
library(magrittr)       # Pipe operator (%>%)
library(xml2)           # pour read_html
library(rvest)          # pour scrapping
library(tidyverse)      # for data formatting


# Load Data
url_caract <- 'https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=328818542'
CARACT <- gsheet2tbl(url_caract) %>%
  mutate(NAME = tolower(NAME),
         Height_combined =  as.numeric(Height_combined))

url_data <- 'https://docs.google.com/spreadsheets/d/1Z5JiEmVaUu4gPKbWE-lNxg1dDTyRpCsC5CGii4EUQm8/edit#gid=0'
DATA <- gsheet2tbl(url_data)


############################ COMPLETE DATABASE #################################@@
######
DATA$`tree age treatment`<- as.numeric(DATA$`tree age treatment`)

# Complete Common name
 CARACT<- data.frame(CARACT)
 #names(CARACT)[1]<-"NAME"
 CARACT$`Common Name`<- "NA"
#
# CARACT$`Common Name2` <- ifelse(CARACT$`Common Name`=="NA",
#                                 sci2comm(sci = CARACT$NAME)[[1]],
#                                 CARACT$`Common Name`)

library(taxize)
CARACT$`Common Name2` <- ifelse(is.na(CARACT$`Common Name`),
                               sci2comm(sci = CARACT$NAME)[[1]],
                               CARACT$`Common Name`)

# write.xlsx(CARACT, "CARACT.xlsx", sheetName = "Sheet1",
#            col.names = TRUE, row.names = TRUE, append = FALSE)

# Define constants
const_cols <- c("Habit","Height","Pollinators","Cultivation Status")
const_images <- c("medi.png","other.png","banana.png")
CARACT$Polli <-NA
CARACT$Hazard <-NA
CARACT$Habitats<-NA
CARACT$Hardiness<-NA
CARACT$Common<-NA
CARACT$range<-NA
CARACT$weeds<-NA
# Avec PFAF
for (i in 1:length(CARACT$NAME)) {
  print(i)
  url<-paste0("https://pfaf.org/user/Plant.aspx?LatinName=",   gsub("[[:space:]]", "+", CARACT$NAME[i]))
  CARACT$NAME[i]
  # Lire le contenu de la page HTML
  page <- read_html(url)
  print(CARACT$NAME[i])
  node <- html_nodes(page, "p")[1]
  text <- html_text(node)

  # Extraire le tableau de la page
  tryCatch({
    # Update Page Accessible column to "Yes"
    # Extract table data
    table <- NULL
    table_filtered <- NULL
    image_urls <- NULL
    image_urls_filtered <- NULL
    text2 <- NULL
    debut_cible <- NULL
    fin_cible <- NULL
    texte_cible <- NULL
    table <- page %>% html_node("table")

    # Check if the table exists on the page
    if (!inherits(table, "xml_missing")) {
      table <- table %>% html_table()
      table_filtered <- table %>% filter(X1 %in% const_cols)
      text2 <- html_text(page)
      debut_cible <- regexpr("Physical Characteristics", text2, ignore.case = TRUE)
      fin_cible <- regexpr("Synonyms", text2, ignore.case = TRUE)
      texte_cible <- substr(text2, debut_cible,fin_cible)

      # Extract image URLs
      image_urls <- page %>% html_nodes("img") %>% html_attr("src")
      image_urls_filtered <- image_urls[image_urls %in% const_images]

      # Update TABDB
      CARACT$alim_PFAF[i] <- table[table$X1=="Edibility Rating",2]
      CARACT$medi_PFAF[i] <- table[table$X1=="Medicinal Rating",2]
      CARACT$other_PFAF[i] <- table[table$X1=="Other Uses",2]
      CARACT$PerennialPFAF[i] <- grepl("perennial", texte_cible, ignore.case = TRUE)
      CARACT$annualPFAF[i] <- grepl("annual", texte_cible, ignore.case = TRUE)
      CARACT$climberPFAF[i] <- grepl("climber", texte_cible, ignore.case = TRUE)
      CARACT$DeciduousPFAF[i]<- grepl("deciduous", texte_cible, ignore.case = TRUE)
      CARACT$EvergreenPFAF[i] <- grepl("evergreen", texte_cible, ignore.case = TRUE)
      CARACT$TreePFAF[i]<- grepl("tree", texte_cible, ignore.case = TRUE)
      CARACT$SchrubPFAF[i] <- grepl("schrub", texte_cible, ignore.case = TRUE)
      CARACT$woodyPFAF[i] <- grepl("woody", texte_cible, ignore.case = TRUE)
      CARACT$herbaceousPFAF[i] <- grepl("herbaceous", texte_cible, ignore.case = TRUE)
      motif <- "\\b(\\d+)\\s*m\\b"
      CARACT$HeightPFAF[i]<- regmatches(texte_cible, gregexpr(motif, texte_cible))
      motif_pollinationPFAF <- ".*\\pollinat.*?\\."
      CARACT$Polli[i]<- gsub("[\r\n\t]", " ", regmatches(texte_cible, gregexpr(motif_pollinationPFAF, texte_cible, ignore.case = TRUE))[[1]])
      CARACT$Hazard[i]<-table[table$X1=="Known Hazards",2]
      CARACT$Habitats[i]<-table[table$X1=="Habitats",2]
      CARACT$Hardiness[i]<-table[table$X1=="USDA hardiness",2]
      CARACT$Common[i]<-table[table$X1=="Common Name",2]
      CARACT$range[i]<-table[table$X1=="Range",2]
      CARACT$weeds[i]<-table[table$X1=="Weed Potential",2]


      CARACT$OK[i] <- "yes"
    } else {
      # If the table is missing, handle it here
      CARACT$alim_PFAF[i] <- "NA"
      CARACT$medi_PFAF[i] <- "NA"
      CARACT$other_PFAF[i] <- "NA"
      CARACT$PerennialPFAF[i] <- "NA"
      CARACT$annualPFAF[i] <- "NA"
      CARACT$climberPFAF[i] <- "NA"
      CARACT$DeciduousPFAF[i]<- "NA"
      CARACT$EvergreenPFAF[i] <- "NA"
      CARACT$TreePFAF[i]<- "NA"
      CARACT$SchrubPFAF[i] <- "NA"
      CARACT$woodyPFAF[i] <- "NA"
      CARACT$herbaceousPFAF[i] <- "NA"
      CARACT$HeightPFAF[i]<- "NA"
      CARACT$Polli[i]<- "NA"
      CARACT$Hazard[i]<- "NA"
      CARACT$Habitats[i]<- "NA"
      CARACT$Hardiness[i]<- "NA"
      CARACT$Common[i]<- "NA"
      CARACT$range[i]<- "NA"
      CARACT$weeds[i]<- "NA"
      CARACT$OK[i] <- "NO"
    }


  }, error = function(e) {
    # Handle error
    CARACT$alim_PFAF[i] <- "NA"
    CARACT$medi_PFAF[i] <- "NA"
    CARACT$other_PFAF[i] <- "NA"
    CARACT$PerennialPFAF[i] <- "NA"
    CARACT$annualPFAF[i] <- "NA"
    CARACT$climberPFAF[i] <- "NA"
    CARACT$DeciduousPFAF[i]<- "NA"
    CARACT$EvergreenPFAF[i] <- "NA"
    CARACT$TreePFAF[i]<- "NA"
    CARACT$SchrubPFAF[i] <- "NA"
    CARACT$woodyPFAF[i] <- "NA"
    CARACT$herbaceousPFAF[i] <- "NA"
    CARACT$HeightPFAF[i]<- "NA"
    CARACT$Polli[i]<- "NA"
    CARACT$Hazard[i]<- "NA"
    CARACT$Habitats[i]<- "NA"
    CARACT$Hardiness[i]<- "NA"
    CARACT$Common[i]<- "NA"
    CARACT$range[i]<- "NA"
    CARACT$weeds[i]<- "NA"
    #CARACT$OK[i] <- "NO"

  })

  print(CARACT$OK[i])
}
CARACT$EdibilityPFAF<-as.numeric(substr(CARACT$alim_PFAF, 2, 2))
CARACT$UsesPFAF<-as.numeric(substr( CARACT$other_PFAF, 2, 2))
CARACT$MedicinalPFAF<-as.numeric(substr( CARACT$medi_PFAF, 2, 2))

# Avec FERN tropical
for (i in 1:length(CARACT$NAME)) {
  print(i)
  url <- paste0("https://tropical.theferns.info/viewtropical.php?id=", gsub("[[:space:]]", "%20", CARACT$NAME[i]))
  CARACT$NAME[i]
  # Lire le contenu de la page HTML
  page <- read_html(url)
  print(CARACT$NAME[i])
  node <- html_nodes(page, "p")[1]
  text <- html_text(node)

  # Extraire le tableau de la page
  tryCatch({
    # Update Page Accessible column to "Yes"
    # Extract table data
    table <- NULL
    table_filtered <- NULL
    image_urls <- NULL
    image_urls_filtered <- NULL
    text2 <- NULL
    debut_cible <- NULL
    fin_cible <- NULL
    texte_cible <- NULL
    table <- page %>% html_node("table")

    # Check if the table exists on the page
    if (!inherits(table, "xml_missing")) {
      table <- table %>% html_table()
      table_filtered <- table %>% filter(X1 %in% const_cols)
      text2 <- html_text(page)
      debut_cible <- regexpr("General Information", text2, ignore.case = TRUE)
      fin_cible <- regexpr("Known Hazards", text2, ignore.case = TRUE)
      texte_cible <- substr(text2, debut_cible,fin_cible)

      # Extract image URLs
      image_urls <- page %>% html_nodes("img") %>% html_attr("src")
      image_urls_filtered <- image_urls[image_urls %in% const_images]

      # Update TABDB
      CARACT$alim_tropFERNS[i] <- sum(image_urls_filtered == const_images[3])
      CARACT$medi_tropFERNS[i] <- sum(image_urls_filtered == const_images[1])
      CARACT$other_tropFERNS[i] <- sum(image_urls_filtered == const_images[2])
      CARACT$habitFERNS[i] <- table[table$X1=="Habit",2]
      CARACT$HeightFERNS[i] <- table[table$X1=="Height",2]
      CARACT$Pollinators[i] <- table[table$X1=="Pollinators",2]
      CARACT$Cultivation[i] <- table[table$X1=="Cultivation Status",2]
      CARACT$Growth[i] <- table[table$X1=="Growth Rate",2]

      CARACT$PerennialFERNS[i] <- grepl("perennial", texte_cible, ignore.case = TRUE)
      CARACT$annualFERNS[i] <- grepl("annual", texte_cible, ignore.case = TRUE)
      CARACT$climberFERNS[i] <- grepl("climber", texte_cible, ignore.case = TRUE)
      CARACT$DeciduousFERNS[i]<- grepl("deciduous", texte_cible, ignore.case = TRUE)
      CARACT$EvergreenFERNS[i] <- grepl("evergreen", texte_cible, ignore.case = TRUE)
      CARACT$TreeFERNS[i]<- grepl("tree", texte_cible, ignore.case = TRUE)
      CARACT$SchrubFERNS[i] <- grepl("schrub", texte_cible, ignore.case = TRUE)
      CARACT$woodyFERNS[i] <- grepl("woody", texte_cible, ignore.case = TRUE)
      CARACT$herbaceousFERNS[i] <- grepl("herbaceous", texte_cible, ignore.case = TRUE)
      motif <- "\\b(\\d+)\\s*m\\b"
      CARACT$HeightFERNS[i]<- regmatches(texte_cible, gregexpr(motif, texte_cible))
      motif_pollinationFERNS <- ".*\\bpollinat.*?\\."
      CARACT$OK[i] <- "yes"
    } else {
      # If the table is missing, handle it here
      CARACT$alim_tropFERNS[i] <- "NA"
      CARACT$medi_tropFERNS[i] <- "NA"
      CARACT$other_tropFERNS[i] <- "NA"
      CARACT$habitFERNS[i] <- "NA"
      CARACT$HeightFERNS[i] <- "NA"
      CARACT$Pollinators[i] <- "NA"
      CARACT$Cultivation[i] <- "NA"
      CARACT$Growth[i] <- "NA"
      CARACT$PerennialFERNS[i] <- "NA"
      CARACT$annualFERNS[i] <- "NA"
      CARACT$climberFERNS[i] <- "NA"
      CARACT$DeciduousFERNS[i]<- "NA"
      CARACT$EvergreenFERNS[i] <- "NA"
      CARACT$TreeFERNS[i]<- "NA"
      CARACT$SchrubFERNS[i] <- "NA"
      CARACT$woodyFERNS[i] <- "NA"
      CARACT$herbaceousFERNS[i] <- "NA"
      CARACT$HeightFERNS[i]<- "NA"
      CARACT$OK[i] <- "NO"
    }


  }, error = function(e) {
    # Handle error
    CARACT$alim_tropFERNS[i] <- "NA"
    CARACT$medi_tropFERNS[i] <- "NA"
    CARACT$other_tropFERNS[i] <- "NA"
    CARACT$habitFERNS[i] <- "NA"
    CARACT$HeightFERNS[i] <- "NA"
    CARACT$Pollinators[i] <- "NA"
    CARACT$Cultivation[i] <- "NA"
    CARACT$Growth[i] <- "NA"
    CARACT$PerennialFERNS[i] <- "NA"
    CARACT$annualFERNS[i] <- "NA"
    CARACT$climberFERNS[i] <- "NA"
    CARACT$DeciduousFERNS[i]<- "NA"
    CARACT$EvergreenFERNS[i] <- "NA"
    CARACT$TreeFERNS[i]<- "NA"
    CARACT$SchrubFERNS[i] <- "NA"
    CARACT$woodyFERNS[i] <- "NA"
    CARACT$herbaceousFERNS[i] <- "NA"
    CARACT$HeightFERNS[i]<- "NA"
    CARACT$OK[i] <- "NO"

  })

  print(CARACT$OK[i])
}
CARACT$alim_tropFERNS<-as.numeric(CARACT$alim_tropFERNS)
CARACT$medi_tropFERNS<-as.numeric(CARACT$medi_tropFERNS)
CARACT$other_tropFERNS<-as.numeric(CARACT$other_tropFERNS)

# Avec FERN temperate
for (i in 1:length(CARACT$NAME)) {
  print(i)
  url <- paste0("https://temperate.theferns.info/viewtropical.php?id=", gsub("[[:space:]]", "%20", CARACT$NAME[i]))
  CARACT$NAME[i]
  # Lire le contenu de la page HTML
  page <- read_html(url)
  print(CARACT$NAME[i])
  node <- html_nodes(page, "p")[1]
  text <- html_text(node)

  # Extraire le tableau de la page
  tryCatch({
    # Update Page Accessible column to "Yes"
    # Extract table data
    table <- NULL
    table_filtered <- NULL
    image_urls <- NULL
    image_urls_filtered <- NULL
    text2 <- NULL
    debut_cible <- NULL
    fin_cible <- NULL
    texte_cible <- NULL
    table <- page %>% html_node("table")

    # Check if the table exists on the page
    if (!inherits(table, "xml_missing")) {
      table <- table %>% html_table()
      table_filtered <- table %>% filter(X1 %in% const_cols)
      text2 <- html_text(page)
      debut_cible <- regexpr("General Information", text2, ignore.case = TRUE)
      fin_cible <- regexpr("Known Hazards", text2, ignore.case = TRUE)
      texte_cible <- substr(text2, debut_cible,fin_cible)

      # Extract image URLs
      image_urls <- page %>% html_nodes("img") %>% html_attr("src")
      image_urls_filtered <- image_urls[image_urls %in% const_images]

      # Update TABDB
      CARACT$alim_tempFERNS[i] <- sum(image_urls_filtered == const_images[3])
      CARACT$medi_tempFERNS[i] <- sum(image_urls_filtered == const_images[1])
      CARACT$other_tempFERNS[i] <- sum(image_urls_filtered == const_images[2])
      CARACT$habittempFERNS[i] <- table[table$X1=="Habit",2]
      CARACT$HeighttempFERNS[i] <- table[table$X1=="Height",2]
      CARACT$Pollinators[i] <- table[table$X1=="Pollinators",2]
      CARACT$Cultivation[i] <- table[table$X1=="Cultivation Status",2]
      CARACT$Growth[i] <- table[table$X1=="Growth Rate",2]

      CARACT$PerennialtempFERNS[i] <- grepl("perennial", texte_cible, ignore.case = TRUE)
      CARACT$annualtempFERNS[i] <- grepl("annual", texte_cible, ignore.case = TRUE)
      CARACT$climbertempFERNS[i] <- grepl("climber", texte_cible, ignore.case = TRUE)
      CARACT$DeciduoustempFERNS[i]<- grepl("deciduous", texte_cible, ignore.case = TRUE)
      CARACT$EvergreentempFERNS[i] <- grepl("evergreen", texte_cible, ignore.case = TRUE)
      CARACT$TreetempFERNS[i]<- grepl("tree", texte_cible, ignore.case = TRUE)
      CARACT$SchrubtempFERNS[i] <- grepl("schrub", texte_cible, ignore.case = TRUE)
      CARACT$woodytempFERNS[i] <- grepl("woody", texte_cible, ignore.case = TRUE)
      CARACT$herbaceoustempFERNS[i] <- grepl("herbaceous", texte_cible, ignore.case = TRUE)
      motif <- "\\b(\\d+)\\s*m\\b"
      CARACT$HeighttempFERNS[i]<- regmatches(texte_cible, gregexpr(motif, texte_cible))
      motif_pollinationtempFERNS <- ".*\\bpollinat.*?\\."
      CARACT$OK[i] <- "yes"
    } else {
      # If the table is missing, handle it here
      CARACT$alim_tempFERNS[i] <- "NA"
      CARACT$medi_tempFERNS[i] <- "NA"
      CARACT$other_tempFERNS[i] <- "NA"
      CARACT$habittempFERNS[i] <- "NA"
      CARACT$HeighttempFERNS[i] <- "NA"
      CARACT$Pollinators[i] <- "NA"
      CARACT$Cultivation[i] <- "NA"
      CARACT$Growth[i] <- "NA"
      CARACT$PerennialtempFERNS[i] <- "NA"
      CARACT$annualtempFERNS[i] <- "NA"
      CARACT$climbertempFERNS[i] <- "NA"
      CARACT$DeciduoustempFERNS[i]<- "NA"
      CARACT$EvergreentempFERNS[i] <- "NA"
      CARACT$TreetempFERNS[i]<- "NA"
      CARACT$SchrubtempFERNS[i] <- "NA"
      CARACT$woodytempFERNS[i] <- "NA"
      CARACT$herbaceoustempFERNS[i] <- "NA"
      CARACT$HeighttempFERNS[i]<- "NA"
      CARACT$OK[i] <- "NO"
    }


  }, error = function(e) {
    # Handle error
    CARACT$alim_tempFERNS[i] <- "NA"
    CARACT$medi_tempFERNS[i] <- "NA"
    CARACT$other_tempFERNS[i] <- "NA"
    CARACT$habittempFERNS[i] <- "NA"
    CARACT$HeighttempFERNS[i] <- "NA"
    CARACT$Pollinators[i] <- "NA"
    CARACT$Cultivation[i] <- "NA"
    CARACT$Growth[i] <- "NA"
    CARACT$PerennialtempFERNS[i] <- "NA"
    CARACT$annualtempFERNS[i] <- "NA"
    CARACT$climbertempFERNS[i] <- "NA"
    CARACT$DeciduoustempFERNS[i]<- "NA"
    CARACT$EvergreentempFERNS[i] <- "NA"
    CARACT$TreetempFERNS[i]<- "NA"
    CARACT$SchrubtempFERNS[i] <- "NA"
    CARACT$woodytempFERNS[i] <- "NA"
    CARACT$herbaceoustempFERNS[i] <- "NA"
    CARACT$HeighttempFERNS[i]<- "NA"
    #CARACT$OK[i] <- "NO"

  })

  print(CARACT$OK[i])
}
CARACT$alim_tempFERNS<-as.numeric(CARACT$alim_troptempFERNS)
CARACT$medi_tempFERNS<-as.numeric(CARACT$medi_troptempFERNS)
CARACT$other_tempFERNS<-as.numeric(CARACT$other_troptempFERNS)

#SAUV<- CARACT
## synthèse
CARACT <- CARACT[, c(1, 2, order(names(CARACT)[-c(1, 2)]))]

process_columns <- function(df, prefix, target) {
  df %>%
    mutate(across(starts_with(prefix), ~as.integer(. == TRUE))) %>%
    mutate({{ target }} := rowSums(select(., starts_with(prefix)), na.rm = TRUE) > 0) %>%
    select(-matches(paste0(prefix, "_FERNS")), -matches(paste0(prefix, "_PFAF")))
}

CARACT <- CARACT %>%
  process_columns("Evergreen", Evergreen) %>%
  process_columns("Perennial", Perennial) %>%
  process_columns("woody", woody) %>%
  process_columns("Deciduous", Deciduous) %>%
  process_columns("Schrub", Schrub) %>%
  process_columns("Tree", Tree) %>%
  process_columns("annual", Annual) %>%
  process_columns("herbaceous", herbaceous)




CARACT2 <- CARACT[,-c(1:3)] %>%
  mutate(across(where(is.character), ~case_when(. == TRUE ~ 1, . == FALSE ~ 0)))
CARACT<- cbind(CARACT[, c(1:3)],CARACT2)

CARACT <- CARACT %>%
  mutate(Evergreen = rowSums(select(., starts_with("Evergreen")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('EvergreenFERNS',"EvergreenPFAF","EvergreentempFERNS"))
CARACT <- CARACT %>%
  mutate(Perennial = rowSums(select(., starts_with("Perennial")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('PerennialFERNS',"PerennialPFAF","PerennialtempFERNS"))
CARACT <- CARACT %>%
  mutate(woody = rowSums(select(., starts_with("woody")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('woodyFERNS',"woodyPFAF"))
CARACT <- CARACT %>%
  mutate(Deciduous = rowSums(select(., starts_with("Deciduous")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('DeciduousFERNS',"DeciduousPFAF","DeciduoustempFERNS"))
CARACT <- CARACT %>%
  mutate(Schrub = rowSums(select(., starts_with("Schrub")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('SchrubFERNS',"SchrubtempFERNS","SchrubFERNS"))
CARACT <- CARACT %>%
  mutate(Tree = rowSums(select(., starts_with("Tree")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('TreePFAF',"TreeFERNS"))
CARACT <- CARACT %>%
  mutate(Annual = rowSums(select(., starts_with("Annual")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('annualFERNS',"annualPFAF","annualtempFERNS"))
CARACT <- CARACT %>%
  mutate(herbaceous = rowSums(select(., starts_with("herbaceous")), na.rm = TRUE) > 0) %>%
  dplyr::select(-c('herbaceousPFAF',"herbaceousFERNS"))


# Calculer la moyenne des trois colonnes pour chaque ligne
CARACT$alim_tropMEAN <- rowMeans(CARACT[, c("alim_tropFERNS", "alim_tempFERNS", "EdibilityPFAF")], na.rm = TRUE)
CARACT$medi_tropMEAN <- rowMeans(CARACT[, c("medi_tropFERNS", "medi_tempFERNS", "MedicinalPFAF")], na.rm = TRUE)
CARACT$other_tropMEAN <- rowMeans(CARACT[, c("other_tropFERNS", "other_tempFERNS", "UsesPFAF")], na.rm = TRUE)


CARACT$EdibilityPFAF<-as.numeric(substr( CARACT$EdibilityPFAF, 2, 2))
CARACT$UsesPFAF<-as.numeric(substr( CARACT$UsesPFAF, 2, 2))
CARACT$MedicinalPFAF<-as.numeric(substr( CARACT$MedicinalPFAF, 2, 2))

## Check names

# Take a subset of the testfile to speed up runtime
tnrs_testfile <- CARACT$NAME

library(TNRS)
httr::set_config(httr::config(ssl_verifypeer = 0L))
tnrs_testfile <- CARACT$NAME
results <- TNRS(taxonomic_names = tnrs_testfile)
#or https://tnrs.biendata.org/

tnrs_result <- read.csv("~/Downloads/tnrs_result.csv")
CARACTGG<-data.frame(CARACT)
write.csv(CARACT, 'CARACT.AlimJANV02.csv')

#####

## Some Checks

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

################################ Analysis #############################



# Create a new data frame TAB_FINALE with relevant columns
TAB_FINALE <- DATA %>%
  mutate(NB_sp = str_count(`species treatment`, ',') + 1) %>%
  dplyr::select(`New ID`, NB_sp, "numéro", "farm type", "Design", "Scale")

# Combine sheet characteristics and Raw Database
DATA2 <- DATA %>%
  tidyr::separate_rows(`species treatment`, sep = ",", convert = TRUE) %>%
  mutate(`species treatment` = tolower(`species treatment`)) %>%
  dplyr::left_join(CARACT, by = c(`species treatment` = "NAME")) %>%
  dplyr::mutate(Woodiness = tolower(Woodiness)) %>%
  dplyr::select(c(`New ID`, "numéro", "Intervention_reclass", "Evergreen", "Herb", "Annual", "Woodiness", "Alim", "Medicinal.y", "Timber", "Part", "Woodiness", `species treatment`, "Height_combined", "N_Fixing", "IUCN", "cultivation_trop", `tree age treatment`, "Habit_Combined","Polli")) %>%
  dplyr::mutate(Height_class = cut(Height_combined, breaks = c(0, 1, 2.5, 5, 10, 60)))


##### STEP 1: Check if all lines have horticultural crops (here: alim or medicinal)
######

ALIM<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Alim) %>%
  dplyr::group_by(`numéro`,Alim) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Alim,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric)

Medic<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Medicinal.y) %>%
  dplyr::group_by(`numéro`,Medicinal.y) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Medicinal.y,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
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
  dplyr::count(Woodiness) %>%
  dplyr::group_by(`numéro`,Woodiness) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Woodiness,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric)

TO_EXCLUDE2<-Wood %>% filter(woody<1)

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
  dplyr::filter(!numéro %in%  VERIF$numéro)
DATA <- DATA %>%
  dplyr::filter(!numéro %in%  VERIF$numéro)



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
  mutate(herb_NONhorti = ifelse(tolower(`species treatment`) %in% herb_NONhorti, "NH_Herb", NA),
         Woody_NONhorti = ifelse(tolower(`species treatment`) %in% Woody_NONhorti, "NH_Wood", NA),
         herb_woody = ifelse(tolower(`species treatment`) %in% herb_horti, "H_Herb", NA),
         Woody_horti = ifelse(tolower(`species treatment`) %in% Woody_horti, "H_Wood", NA))

result <- GROUP %>%
  group_by(numéro) %>%
  summarise(
    Concatenation = paste(unique(na.omit(c(herb_NONhorti, Woody_NONhorti, herb_woody, Woody_horti))), collapse = ", ")
  )
table(result$Concatenation)

result <- result %>%
  mutate(Categorie = case_when(
    Concatenation %in% c("NH_Wood, H_Herb", "NH_Herb, NH_Wood, H_Herb") ~ "(NH_Herb), NH_Wood, H_Herb",
    Concatenation %in% c("H_Herb, H_Wood", "NH_Herb, H_Herb, H_Wood") ~ "(NH_Herb), H_Herb, H_Wood",
    Concatenation %in% c("NH_Wood, H_Wood", "NH_Herb, NH_Wood, H_Wood") ~ "(NH_Herb), NH_Wood, H_Wood",
    Concatenation %in% c("NH_Herb, NH_Wood, H_Herb, H_Wood", "NH_Wood, H_Herb, H_Wood") ~ "(NH_Herb), NH_Wood, H_Herb, H_Wood",
    Concatenation %in% c("H_Wood", "NH_Herb, H_Wood") ~ "(NH_Herb), H_Wood",
    TRUE ~ "Autre"
  ))

TAB_FINALE$numéro<-as.character(TAB_FINALE$numéro)
setdiff(TAB_FINALE$numéro,result$numéro)
setdiff(result$numéro,TAB_FINALE$numéro)
TAB_FINALE<- left_join(TAB_FINALE, result, by='numéro')

# #ON enlève les lignes qui ne correspondent pas
table(TAB_FINALE$Concatenation)

TO_EXCLUDE<-TAB_FINALE %>% filter(Concatenation %in% c("H_Wood"))

KEEP <- DATA %>% filter(numéro%in%  TO_EXCLUDE$numéro) %>%
  select(numéro, "species treatment")

## même chose pour le contrôle
GROUP_C<-DATA %>%
  separate_longer_delim(`species control`, delim = ",") %>%
  mutate(`species control` = tolower(`species control`))

GROUP_C <- GROUP_C %>%
  mutate(herb_NONhorti  = ifelse(tolower(`species control`) %in% herb_NONhorti, "NH_Herb", NA),
         Woody_NONhorti = ifelse(tolower(`species control`) %in% Woody_NONhorti, "NH_Wood", NA),
         herb_woody     = ifelse(tolower(`species control`) %in% herb_horti, "H_Herb", NA),
         Woody_horti    = ifelse(tolower(`species control`) %in% Woody_horti, "H_Wood", NA))
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
    Concatenation %in% c("NH_Wood, H_Herb", "NH_Herb, NH_Wood, H_Herb") ~ "(NH_Herb), NH_Wood, H_Herb",
    Concatenation %in% c("H_Herb, H_Wood", "NH_Herb, H_Herb, H_Wood") ~ "(NH_Herb), H_Herb, H_Wood",
    Concatenation %in% c("NH_Wood, H_Wood", "NH_Herb, NH_Wood, H_Wood") ~ "(NH_Herb), NH_Wood, H_Wood",
    Concatenation %in% c("NH_Herb, NH_Wood, H_Herb, H_Wood", "NH_Wood, H_Herb, H_Wood") ~ "(NH_Herb), NH_Wood, H_Herb, H_Wood",
    Concatenation %in% c("H_Wood", "NH_Herb, H_Wood") ~ "(NH_Herb), H_Wood",
    Concatenation %in% c("NH_Wood") ~ "NH_Wood",
    Concatenation %in% c("H_Herb") ~ "H_Herb",
    Concatenation %in% c("NH_Herb, NH_Wood") ~ "NH_Herb, NH_Wood",
    TRUE ~ "Autre"
  ))
names(result_C)[c(2,3)]<- c("Concatenation_C","Categorie_C")

table(result_C$Categorie_C)
TAB_FINALE$numéro<-as.character(TAB_FINALE$numéro)
TAB_FINALE<- left_join(TAB_FINALE, result_C, by='numéro')

# on va regarder les controles qui pourraient être des systèmes agroF-horticoles:
TO_EXCLUDE<-TAB_FINALE %>% filter(Concatenation_C %in% c("H_Herb, H_Wood",
                                                       "NH_Herb, H_Wood",
                                                       "NH_Herb, NH_Wood NH_Herb, NH_Wood, H_Herb",
                                                       "NH_Herb, NH_Wood, H_Wood",
                                                       "NH_Wood, H_Herb",
                                                       "NH_Wood, H_Wood"))
dim(TO_EXCLUDE)
KEEP <- DATA %>% filter(numéro%in%  TO_EXCLUDE$numéro) %>%
  select(numéro, "species control")

# on va exclure de cette liste les systèmes qui comporte 2 espèces dont une poacées ( car on a dit que l'on ne considérait pas les poacées commes hoerticoles)
POACEES <-CARACT  %>% filter(Name_matched_accepted_family =="Poaceae")
POACEES<- unique(POACEES$NAME)

KEEP$poa <- sapply(strsplit(as.character(tolower(KEEP$`species control`)), ","), function(x) sum(!x %in% POACEES))
KEEP<- KEEP %>% filter(poa>1)

### TODO : CEs lignes seront à regarder précisément
## pour l'instant je vais les virer

TAB_FINALE <- TAB_FINALE %>% filter(!numéro %in% KEEP$numéro)


#on recomence à coder une fois qu'on a fait le tri
table(TAB_FINALE$Concatenation_C)

TAB_FINALE <- TAB_FINALE %>%
     mutate(Categorie_C2 = case_when(
         Concatenation_C %in% c("H_Wood") ~ "H_Wood",
         Concatenation_C %in% c("H_Herb") ~ "H_Herb",
         Concatenation_C %in% c("NH_Wood") ~ "NH_Wood",
         Concatenation_C %in% c("NH_Herb") ~ "NH_Herb",
         TRUE ~ "Other"
   ))

TAB_FINALE <- TAB_FINALE %>% 
  mutate(Intervention_by_composition = ifelse(Categorie =="(NH_Herb), NH_Wood, H_Herb","Classic",
                                              ifelse(Categorie =="(NH_Herb), H_Herb, H_Wood","Exclusive",
                                                     ifelse(Categorie =="(NH_Herb), H_Wood","Woody exclusive",
                                                            ifelse(Categorie =="(NH_Herb), NH_Wood, H_Herb, H_Wood","Complex",
                                                                   ifelse(Categorie =="(NH_Herb), NH_Wood, H_Wood","Woody classic","check"))))))


# Je vais faire un graphique pour mettre cela au propre
library(tidyverse)
library(network)
library(tidygraph)
library(ggraph)
library(networkD3)

TAB_F <- TAB_FINALE
names(TAB_F)[c(8,11)]<- c("destination", 'source')
TAB_F$destination<- as.factor(TAB_F$destination)
TAB_F <- TAB_F %>% mutate(destination =
                            factor(destination, levels = c("(NH_Herb), NH_Wood, H_Herb",
                                                   "(NH_Herb), H_Herb, H_Wood",
                                                   "(NH_Herb), NH_Wood, H_Wood",
                                                   "(NH_Herb), H_Wood",
                                                  "(NH_Herb), NH_Wood, H_Herb, H_Wood" )))

TAB_F$source<-paste("C_",TAB_F$source)

destinations <- TAB_F %>%
  distinct(destination) %>%
  rename(label = destination)

destinations <- destinations %>% arrange(match(label, c("(NH_Herb), NH_Wood, H_Herb, H_Wood" ,
                                                        "(NH_Herb), H_Wood",
                                                        "(NH_Herb), NH_Wood, H_Wood",
                                                        "(NH_Herb), H_Herb, H_Wood",
                                                        "(NH_Herb), NH_Wood, H_Herb")))


sources <- TAB_F %>%
  distinct(source) %>%
  rename(label = source)

sources <- sources %>% arrange(match(label, c("C_ H_Wood" ,
                                              "C_ H_Herb",
                                              "C_ NH_Wood",
                                              "C_ NH_Herb",
                                              "C_ Other")))
table(TAB_F$source)


sources <- sources %>% arrange(match(label, c("C_ Other" ,
                                                        "C_ NH_Herb",
                                                        "C_ NH_Wood",
                                                        "C_ H_Herb",
                                                        "C_ H_Wood")))


nodes <- full_join(sources, destinations, by = "label")
nodes <- rowid_to_column(nodes, "id")

per_route <- TAB_F %>%
  group_by(source, destination) %>%
  summarise(weight = n(), .groups = "drop")

edges <- per_route %>%
  left_join(nodes, by = c("source" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(nodes, by = c("destination" = "label")) %>%
  rename(to = id)

edges <- select(edges, from, to, weight)

nodes_d3 <- mutate(nodes, id = id - 1)
nodes_d3$color=c("blue","blue","blue","blue","blue","blue","blue","blue","gray")
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)


# Add a 'group' column to each connection:
edges_d3$group <- as.factor(edges_d3$to)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes_d3$group <- as.factor(c("my_unique_group"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["5","6","7","8","9"]) .range(["#69b3a2","steelblue","#ccebc5","#fdc086","#beaed4","#EEEEEE"])'

# Make the Network
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
                   Value = "weight", NodeID = "label",
                   colourScale=my_color, LinkGroup="group", NodeGroup="group",
              iterations = 0)


######

##### STEP 2: Complete the database

##### Count Number of alim, medicinal, other species
#####

ALIM<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Alim) %>%
  dplyr::group_by(`numéro`,Alim) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Alim,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(numéro,Alim )

Medic<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Medicinal.y) %>%
  dplyr::group_by(`numéro`,Medicinal.y) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Medicinal.y,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(numéro,medicinal )

other<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Timber) %>%
  dplyr::group_by(`numéro`,Timber) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Timber,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(-"NA") %>%
  rowwise() %>%
  mutate(other = sum(c_across(where(is.numeric)))) %>%
  dplyr::select(numéro,other )

# # check to see if the number of species is OK
# Total_species<-data.frame(table(DATA2$numéro))
# names(Total_species)<-c('numéro','total_sp')
# Total_species$numéro<- as.numeric(as.character(Total_species$numéro))
#
TOT<-full_join(ALIM,Medic,by = join_by(numéro))
TOT<-full_join(TOT,other,by = join_by(numéro))

TAB_FINALE<- left_join(TAB_FINALE, TOT, by='numéro')


### Number of woody
Woody<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Woodiness =tolower(Woodiness)) %>%
  dplyr::count(Woodiness) %>%
  dplyr::group_by(`numéro`,Woodiness) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Woodiness,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(-"NA") %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric))),
         prop_woody = woody/total) %>%
  dplyr::select(numéro,woody,prop_woody )

TAB_FINALE<- left_join(TAB_FINALE, Woody, by='numéro')

### Type of woody
Herb<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Herb =tolower(Herb)) %>%
  dplyr::count(Herb) %>%
  dplyr::group_by(`numéro`,Herb) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Herb,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(-"NA") %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric))),
         shrub = sum(across(contains("shrub"))),
         climber = sum(across(contains("climber"))),
         prop_shrub = shrub/total,
         prop_climber = climber/total) %>%
  dplyr::select(numéro,shrub,climber,prop_shrub,prop_climber )

TAB_FINALE<- left_join(TAB_FINALE, Herb, by='numéro')


### Type of annual/perennial
Annual<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Annual =tolower(Annual)) %>%
  dplyr::count(Annual) %>%
  dplyr::group_by(`numéro`,Annual) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Annual,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(-"NA") %>%
  rowwise()  %>%
  mutate(total   = sum(c_across(where(is.numeric))),
         Annual2 = sum(across(contains("annua"))),
         Prop_annual = Annual2/total) %>%
  dplyr::select(numéro,Annual2,Prop_annual)

TAB_FINALE<- left_join(TAB_FINALE, Annual, by='numéro')


### Evergreen
Evergreen<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Evergreen =tolower(Evergreen)) %>%
  dplyr::count(Evergreen) %>%
  dplyr::group_by(`numéro`,Evergreen) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Evergreen,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(-"NA") %>%
  rowwise()%>%
  mutate(total   = sum(c_across(where(is.numeric))),
         Woody_evergreen = sum(across(contains("evergreen"))),
         Prop_woody_evergreen = Woody_evergreen/total)  %>%
  dplyr::select(numéro,Woody_evergreen,Prop_woody_evergreen)

TAB_FINALE<- left_join(TAB_FINALE, Evergreen, by='numéro')

### N_fixing
N_Fixing<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Evergreen =tolower(N_Fixing)) %>%
  dplyr::count(N_Fixing) %>%
  dplyr::group_by(`numéro`,N_Fixing) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = N_Fixing,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  dplyr::select(-"NA") %>%
  rowwise()%>%
  mutate(total   = sum(c_across(where(is.numeric))),
         N_Fixing = total -No,
         Prop_N_fixing = N_Fixing/total)   %>%
  dplyr::select(numéro,N_Fixing,Prop_N_fixing)

TAB_FINALE<- left_join(TAB_FINALE, N_Fixing, by='numéro')

### Woody_horticole
DATA2$Woody_hort<-paste(DATA2$Woodiness,DATA2$Alim)
Woody_hort<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Evergreen =tolower(Woody_hort)) %>%
  dplyr::count(Woody_hort) %>%
  dplyr::group_by(`numéro`,Woody_hort) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Woody_hort,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  rowwise()%>%
  mutate(total           = sum(c_across(where(is.numeric))),
         Woody_hort      = sum(across(contains("woody alim"))),
         Prop_Woody_hort = Woody_hort/total,
         Non_Woody_hort  = sum(across(contains("herbaceous alim")))+sum(across(contains("liane alim"))),
         Prop_Non_Woody_hort = Non_Woody_hort/total) %>%
  dplyr::select(numéro,Woody_hort,Prop_Woody_hort,Non_Woody_hort,Prop_Non_Woody_hort)

TAB_FINALE<- left_join(TAB_FINALE, Woody_hort, by='numéro')

### IUCN
IUCN<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(IUCN =tolower(IUCN)) %>%
  dplyr::count(IUCN) %>%
  dplyr::group_by(`numéro`,IUCN) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = IUCN,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  rowwise()%>%
  mutate(total   = sum(c_across(where(is.numeric))),
         EN_VU_NT_CR = sum(across(contains("en -")))+ sum(across(contains("vu -")))+
                       sum(across(contains("nt -")))+ sum(across(contains("cr -"))),
         Prop_EN_VU_NT_CR = EN_VU_NT_CR/total) %>%
  dplyr::select(numéro,EN_VU_NT_CR,Prop_EN_VU_NT_CR)

TAB_FINALE<- left_join(TAB_FINALE, IUCN, by='numéro')

### Pollinators
Polli<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(Polli =tolower(Polli)) %>%
  dplyr::count(Polli) %>%
  dplyr::group_by(`numéro`,Polli) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = Polli,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  rowwise()%>%
  mutate(total   = sum(c_across(where(is.numeric))),
         Prop_Insects = insects/total)%>%
  dplyr::select(numéro,Prop_Insects,insects)

TAB_FINALE<- left_join(TAB_FINALE, Polli, by='numéro')

### Wild
cultivation_trop<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  mutate(cultivation_trop =tolower(cultivation_trop)) %>%
  dplyr::count(cultivation_trop) %>%
  dplyr::group_by(`numéro`,cultivation_trop) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = cultivation_trop,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  rowwise()%>%
  mutate(total     = sum(c_across(where(is.numeric))),
         Wild      = sum(across(contains("wild"))),
         Prop_wild = Wild/total)  %>%
  dplyr::select(numéro,Wild,Prop_wild)

TAB_FINALE<- left_join(TAB_FINALE, cultivation_trop, by='numéro')



### Height of the species
Height<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(Height_class) %>%
  tidyr::pivot_wider(names_from = Height_class,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  mutate(NB_strates = rowSums(across(everything(), ~ . != 0))) %>%
  dplyr::select(numéro,NB_strates)

TAB_FINALE<- left_join(TAB_FINALE, Height, by='numéro')


Height2 <- DATA2 %>%
  group_by(`numéro`) %>%
  summarise(mean_Height = mean(Height_combined, na.rm = TRUE),
            max_Height = ifelse(any(!is.na(Height_combined)), max(Height_combined, na.rm = TRUE), 0))

TAB_FINALE<- left_join(TAB_FINALE, Height2, by='numéro')


### Human interventions

Human_trait <- DATA %>%
  dplyr::select(`New ID`, "numéro", pruning, `fertiliser organic treatment`,
                `fertiliser chemical treatment`, `fertiliser N treatment`,
                `irrigation treatment`, `tillage treatment`,
                `pesticide use treatment`, `Mulch treatment` ) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(`fertiliser N treatment` = case_when(
    `fertiliser N treatment` == "NA" | `fertiliser N treatment` == "0" | is.na(`fertiliser N treatment`) ~ "NO",
    TRUE ~ "YES")) %>%
  rename(pruning_treatment = pruning) %>%
  select(-`fertiliser N treatment`) %>%
  mutate(Human_trait = case_when(if_any(contains("treatment"),
                                        ~. == "YES") ~ "Yes", TRUE ~ "No"))%>%
  select("numéro", Human_trait)

TAB_FINALE<- left_join(TAB_FINALE, Human_trait, by='numéro')


## Densité

Density <- DATA %>%
  dplyr::select(c(`numéro`,`tree density`,"Intervention_reclass")) %>%
  mutate(total_density = sub(";.*", "", `tree density`))%>%
  select("numéro", total_density)
table(Density$numéro)
TAB_FINALE<- left_join(TAB_FINALE, Density, by='numéro')



Woody<-CARACT  %>%
  mutate(Woodiness =tolower(Woodiness)) %>%
  filter(Woodiness== "woody")
list<-Woody$NAME

# Verif que toutes les espèces de species treatment sont des woody:

DATA$count_especesWoody <- str_count(tolower(DATA$`species treatment`), paste(list, collapse = "|"))
DATA$count_densityWoody <- str_count(DATA$`tree density`, ";")
DATA$VERIF<-DATA$count_especesWoody -DATA$count_densityWoody

VERIF<- DATA %>% filter(!VERIF==0) %>% select(numéro, source_id, `species treatment`,count_especesWoody,count_densityWoody)
unique(VERIF$source_id)
##TODO!!! CES lignes là seront à revérifier!!!!

LIST<- VERIF$numéro
SP<-DATA %>%
  dplyr::filter(!numéro %in% LIST) %>%
  separate_longer_delim(`species treatment`, delim = ",") %>%
  filter(tolower(`species treatment`) %in% list) %>%
  # group_by(numéro) %>%
  #    tally()
   select(numéro, `species treatment`)
dim(SP)

Dens<-DATA %>%
  dplyr::filter(!numéro %in% LIST) %>%
 mutate(DENS = sub("^[^;]*;", "",`tree density`)) %>%
  separate_longer_delim(DENS, delim = ";")%>%
  # group_by(numéro) %>%
  #  tally()
  select(numéro, DENS)
dim(Dens)

Densite.arbe<-cbind(SP,Dens)
Densite.arbe<-Densite.arbe[,-1]
Densite.arbe <- Densite.arbe %>%
  mutate(DENS= as.numeric(as.character(DENS))) %>%
  filter(!is.na(DENS))

Somme_dens_Woo<- Densite.arbe %>%
  group_by(numéro) %>%
  summarise(Sum_dens = sum(DENS, na.rm=TRUE))
TAB_FINALE<- left_join(TAB_FINALE, Somme_dens_Woo, by='numéro')


Densite.arbeMEAN <- Densite.arbe %>%
  group_by(`species treatment`) %>%
  mutate(DENS= as.numeric(as.character(DENS)))%>%
  summarise(mean = mean(DENS, na.rm = TRUE),
            n     = n())

TT<- Densite.arbe %>% filter(DENS >80000)
SELECT<- Densite.arbeMEAN %>% filter(n>30)
PLOT<- Densite.arbe %>% filter(`species treatment` %in%  SELECT$`species treatment`)
PLOT %>%
  ggplot()+
  geom_boxplot(aes(x= reorder(`species treatment`,DENS), y=log(DENS)))+
  coord_flip()+
  theme_pubr()
PLOT

#
# TAB<-NULL
# NAMES<-NULL
# for (i in 1:length(DATA$numéro)){
#   print(i%%length(DATA$numéro))
# TAB[[i]]<-DATA[i,] %>%
#   dplyr::select(c(`numéro`,`tree density`)) %>%
#   tidyr::separate(`tree density`, sep=";", into = letters[1:26])
#
# NAMES[[i]]<-DATA[i,] %>%
#   dplyr::select(c(`numéro`,`species treatment`)) %>%
#   tidyr::separate(`species treatment`, sep=",", into = letters[1:26])
#
# DD<-NAMES[[i]][1,2:27] %>% c(., recursive=TRUE) %>%
#   unname
#   DD<-DD[tolower(DD) %in% list]
#   DD[is.na(DD)] <- 1:length(DD[is.na(DD)])
#
#   TAB[[i]]<- TAB[[i]][,+1:(length(DD)+2)]
#   names(TAB[[i]])<- c("numéro","total",DD)
#
# }
#
# big_data <- dplyr::bind_rows(TAB)
# %>%
#   select_if(~sum(!is.na(.)) > 0)
#
# Woody<-CARACT %>% dplyr::filter(Woodiness %in% c("woody","Woody")) %>%
#   dplyr::select(NAME) %>%
#    c(., recursive=TRUE) %>%
#   unname
#
# inter<-intersect(names(big_data),Woody)
# big_data %<>% dplyr::select("numéro", inter)%>%
#   mutate_if(is.character, as.numeric) %>%
#    replace(is.na(.), 0) %>%
#   mutate(DEsnity_tree_total = rowSums(across(where(is.numeric))),
#          DEsnity_tree_total = DEsnity_tree_total - numéro  )
#
# big_data$DEsnity_tree_total[big_data$DEsnity_tree_total == 0] <- NA
#
# big_data %<>% dplyr::select("numéro","DEsnity_tree_total" )
# big_data$`numéro`<-as.character(big_data$`numéro`)
#
# TAB_FINALE<- left_join(TAB_FINALE, big_data)
# dim(TAB_FINALE)
#


### Tree Age


### Wild
tree_age<-DATA2  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::count(`tree age treatment`) %>%
  dplyr::group_by(`numéro`,`tree age treatment`) %>%
  dplyr::summarise(n=sum(n))%>%
  tidyr::pivot_wider(names_from = `tree age treatment`,
                     values_from = "n") %>%
  replace(is.na(.), 0)%>%
  mutate_if(is.character, as.numeric) %>%
  rowwise() %>%
  mutate(total     = sum(c_across(where(is.numeric))),
         Wild      = sum(across(contains("wild"))),
         Prop_wild = Wild/total)  %>%
  dplyr::select(numéro,Wild,Prop_wild)

TAB_FINALE<- left_join(TAB_FINALE, cultivation_trop, by='numéro')


AGE<-DATA  %>%
  dplyr::group_by(`numéro`) %>%
  dplyr::select(`tree age treatment`,`numéro`) %>%
  tidyr::separate( `tree age treatment`, sep=";", into = c("AgeA","AgeB","AgeC","AgeD","AgeE","AgeF","AgeG","AgeH","AgeI","AgeJ", "AgeK","AgeL", "AgeM", "AgeN", "AgeO", "AgeP", "AgeQ", "AgeR", "AgeS" ,"AgeT","AgeU","AgeV","AgeW","AgeX","AgeY","AgeZ"))%>%
  mutate(Age_max_tree = max(c_across(contains("Age")), na.rm=TRUE))

AGE %<>% dplyr::select("numéro","Age_max_tree" )
AGE$`numéro`<-as.character(AGE$`numéro`)

TAB_FINALE<- left_join(TAB_FINALE, AGE)
dim(TAB_FINALE)




TAB_FINALE$Human_trait<-as.factor(TAB_FINALE$Human_trait)
TAB_FINALE$Age_max_tree<- as.numeric(TAB_FINALE$Age_max_tree)
TAB_FINALE$numéro<- as.numeric(TAB_FINALE$numéro)

TAB_FINALE$PROP_Woody2<-TAB_FINALE$PROP_Woody
TAB_FINALE$PROP_Woody2[TAB_FINALE$PROP_Woody2 == 1] <- 0.95
TAB_FINALE$PROP_Woody2[TAB_FINALE$PROP_Woody2 == 0] <- 0.05


TAB_FINALE$PROP_Woody2<-logit(TAB_FINALE$PROP_Woody2)

# We will add all other information by merging files
TAB_FINALE$numéro<-as.character(TAB_FINALE$numéro)
DATA_FINAL_SARAH<- TAB_FINALE %>% left_join(DATA)

write.csv(TAB_FINALE, 'TAB_FINALE.HORA_20240306.csv')


# #### Manual typology
#
# JJ<- DATA2 %>% dplyr::select("New ID","numéro", "Intervention_reclass")
# unique_combinations <- distinct(JJ, `New ID`, numéro, Intervention_reclass)
# TAB_FINALE$numéro<-as.character(TAB_FINALE$numéro)
#
# TAB_F<-left_join(TAB_FINALE,unique_combinations,join_by(`New ID`, numéro))
#
# #TAB_F<- TAB_FINALE
# # libraries
# library(ggraph)
# library(igraph)
# library(tidyverse)
#
# TAB_F$Intervention_reclass<- tolower(trimws(TAB_F$Intervention_reclass))
# TAB_F$Intervention_reclass <- tolower(TAB_F$Intervention_reclass)
# TAB_F$Intervention_reclass <- gsub("\\s*\\/?\\s*", "", TAB_F$Intervention_reclass)
# TAB_F$Intervention_reclass[TAB_F$Intervention_reclass %in% c("herdgerows", "hedgerows")] <- "hedgerows"
# TAB_F$Intervention_reclass[TAB_F$Intervention_reclass %in% c("parkland?")] <- "parkland"
# TAB_F$Intervention_reclass[TAB_F$Intervention_reclass %in% c("other/unknown")] <- "unknown"
#
#
# TAB_F$group <- case_when(
#   grepl("fallows", tolower(TAB_F$Intervention_reclass)) ~ "Group Fallows",
#   grepl("hedgerow", tolower(TAB_F$Intervention_reclass)) ~ "Group Hedgerow",
#   grepl("multistrata", tolower(TAB_F$Intervention_reclass)) ~ "Group Multistrata",
#   grepl("parkland", tolower(TAB_F$Intervention_reclass)) ~ "Group Parkland",
#   grepl("alley cropping", tolower(TAB_F$Intervention_reclass)) ~ "Group Alley Cropping",
#   TRUE ~ "Other"
# )
#
# TAB_F$Intervention_reclass <- recode(TAB_F$Intervention_reclass,
#                                      "Herdgerows" = "Hedgerows",
#                                "complex multi-strata-agroforestry systems" = "Multi",
#                                "Alley cropping /complex mutli-strata agroforestry" = "various",
#                                "Complex multistrata systems" = "Multi",
#                                "Alley-cropping, complex multi-strata systems" = "various",
#                                "multi-strata" = "Multi",
#                                "Alley cropping, multistrata" = "Multi",
#                                "parkland?" = "Parkland",
#                                "Shaded systems" = "Alley-cropping",
#                                "complex multi-strata agroforestry" = "Multi",
#                                "complex multi-strata system" = "Multi",
#                                "Other/unknown" = "various",
#                                "alley-cropping/hedgerows" = "Alley-cropping",
#                                "Alley cropping/ complex multi-strata agroforestry" = "various",
#                                "complex multi-strata-agroforestry systems/hedgerows" = "various",
#                                "Alley-cropping/ complex multi-strata systems" = "various",
#                                "Complex multi-strata agroforestry" = "Multi",
#                                "alley cropping" = "alley-cropping")
#
# ## Sarah
# # différences:
# #Si Alley cropping et fallows -> Alley cropping. Moi: Fallows
# #Si Alley cropping et multi-strata -> multi. Moi: alley-cropping
#
# d <- d %>%
#   mutate(Intervention_reclass = ifelse(Intervention_reclass %in% c("Alley-cropping","Alley cropping","alley cropping","Alley cropping, fallows","alley-cropping","alley-cropping/hedgerows"),"Alley cropping",
#                                        ifelse(Intervention_reclass %in% c("Alley cropping/ complex multi-strata agroforestry","complex multi-strata-agroforestry systems","Alley cropping /complex mutli-strata agroforestry","Alley cropping, multistrata", "Complex multistrata systems","Alley-cropping, complex multi-strata systems",
#                                                                           "multi-strata","multi-strata?","complex multi-strata agroforestry","complex multi-strata system","complex multi-strata-agroforestry systems/hedgerows", "Alley-cropping/ complex multi-strata systems","Complex multi-strata agroforestry"),"Multi-strata systems",
#                                               ifelse(Intervention_reclass %in% c("Parkland","parkland?","parkland"),"Parkland",
#                                                      ifelse(Intervention_reclass == "Herdgerows","Hedgerows",
#                                                             ifelse(Intervention_reclass =="fallows", "Fallows",
#                                                                    ifelse(Intervention_reclass=="Shaded systems","Other/unknown",Intervention_reclass)))))))
#
#
# TAB_F$Intervention_reclass<-tolower(TAB_F$Intervention_reclass)
#
#
# table(TAB_F$Intervention_reclass)
# #TAB_F$total_sp<-as.factor(TAB_F$total_sp)
#
# # Créer une nouvelle colonne 'classe_species' en fonction du nombre d'espèces
# TAB_F$classe_species <- cut(TAB_F$total_sp, c(0,2,80), labels = c("<2", ">2"), include.lowest = TRUE)
# TAB_F$classe_species <- replace(TAB_F$classe_species, is.na(TAB_F$classe_species), "<2")
#
#
# TAB_F$Timber_class <- cut(TAB_F$other, c(0,1,20), labels = c("NO", "YES"), include.lowest = TRUE)
#
# TAB_F$MAIN<-"A"
# # transform it to a edge list!
# edges_level0_1 <- TAB_F %>% select(MAIN, group) %>% unique %>% rename(from=MAIN, to=group)
# edges_level1_2 <- TAB_F %>% select(group, classe_species) %>% unique %>% rename(from=group, to=classe_species)
# edges_level2_3 <- TAB_F %>% select(classe_species, Timber_class) %>% unique %>% rename(from=classe_species, to=Timber_class)
# edge_list=rbind(edges_level0_1,edges_level1_2, edges_level2_3)
#
# # Now we can plot that
# mygraph<-graph_from_data_frame( edge_list )
# ggraph(routes_tidy, layout = 'treemap') +
#   geom_edge_diagonal() +
#  # geom_edge_diagonal2(aes(colour = node.class))+
#   geom_node_text(aes( label=label)) +
#   theme_void()
#
# ggraph(mygraph, layout = 'circlepack') +
#   geom_edge_diagonal() +
#   geom_node_text(aes( label=name)) +
#   theme_void()
#
# #TAB_F$classe_species2<- paste0(TAB_F$Intervention_reclass, TAB_F$classe_species)
# TAB_F$classe_species2<- TAB_F$classe_species
#
#
# TABLE1<- TAB_F %>% select(MAIN,Intervention_reclass)
# TABLE2<- TAB_F %>% select(Intervention_reclass,classe_species2)
# #TABLE3<- TAB_F %>% select(classe_species,Timber_class)
# names(TABLE2)<-names(TABLE1)
# #names(TABLE3)<-names(TABLE1)
#
# TABLE<-rbind(TABLE1,TABLE2)
# #TABLE<-rbind(TABLE,TABLE3)
#
# sources <- TABLE %>%
#   distinct(MAIN) %>%
#   rename(label = MAIN)
#
# destinations <- TABLE %>%
#   distinct(Intervention_reclass) %>%
#   rename(label = Intervention_reclass)
#
# nodes <- full_join(sources, destinations, by = "label")
# nodes <- rowid_to_column(nodes, "id")
#
#
# per_route <- TABLE %>%
#   group_by(MAIN, Intervention_reclass) %>%
#   summarise(weight = n(), .groups = "drop")
# per_route
#
#
# edges <- per_route %>%
#   left_join(nodes, by = c("MAIN" = "label")) %>%
#   rename(from = id)
#
# edges2 <- edges %>%
#   left_join(nodes, by = c("Intervention_reclass" = "label")) %>%
#   rename(to = id)
#
# AH<-TAB_F %>% group_by(MAIN) %>% tally(); names(AH )[1]<-"label"
# BH<-TAB_F %>% group_by(Intervention_reclass) %>% tally(); names(BH )[1]<-"label"
# CH<-TAB_F %>% group_by(classe_species2) %>% tally(); names(CH)[1]<-"label"
# DATASUP<-rbind(AH,BH,CH)
#
# nodes <- left_join(nodes, DATASUP, by="label")
#
#
# names(DATASUP)[1]<-"MAIN"
# edges2<-left_join(edges2, DATASUP, by="MAIN")
#
# edges <- select(edges2, from, to, weight,n, everything())
# #edges<- edges2
#
#
#
# #### deuxième
# #TAB_F$Timber_class2<- paste(TAB_F$classe_species2,TAB_F$Timber_class)
# TAB_F$Timber_class2<- TAB_F$Timber_class
#
# TABLE3<- TAB_F %>% select(Intervention_reclass,classe_species2,Timber_class2)
#
# sources2 <- TABLE3 %>%
#   distinct(classe_species2) %>%
#   rename(label = classe_species2)
#
# destinations2 <- TABLE3 %>%
#   distinct(Timber_class2) %>%
#   rename(label = Timber_class2)
#
# nodes2 <- full_join(sources2, destinations2, by = "label")
# nodes2 <- rowid_to_column(nodes2, "id")
#
#
# per_route2 <- TABLE3 %>%
#   group_by(Intervention_reclass,classe_species2, Timber_class2) %>%
#   summarise(weight = n(), .groups = "drop")
# per_route2
#
#
# edges2 <- per_route2 %>%
#   left_join(nodes2, by = c("classe_species2" = "label")) %>%
#   rename(from = id)
#
# edges3 <- edges2 %>%
#   left_join(nodes2, by = c("Timber_class2" = "label")) %>%
#   rename(to = id)
#
# BH<-TAB_F %>% group_by(Timber_class2) %>% tally(); names(BH )[1]<-"label"
# CH<-TAB_F %>% group_by(classe_species2) %>% tally(); names(CH)[1]<-"label"
# DATASUP<-rbind(BH,CH)
#
# nodes2 <- left_join(nodes2, DATASUP, by="label")
#
#
# names(DATASUP)[1]<-"classe_species2"
# edges4<-left_join(edges3, DATASUP, by="classe_species2")
#
# edges5 <- select(edges4, from, to, weight,n, everything())
#
# ## combine both
# node_code<-nodes
# node_code<- node_code %>% select(id, label)
# names(nodes2)[1]<-"old.id"
# nodes2<-left_join(nodes2,node_code, by='label')
# nodes2 <- nodes2 %>%
#   mutate(id = ifelse(is.na(id), cumsum(is.na(id)) + max(na.omit(id)), id))
# #nodes2<-nodes2[,c("id","label","n","old.id")]
# nodes2<-select(nodes2,id, label, n, old.id, everything())
#
# TOTAL_node<-bind_rows(nodes, nodes2) %>%
#   distinct(id, label, .keep_all = TRUE)
#
#
# EDGE<-left_join(edges4,nodes2,by= join_by(from == old.id))
# EDGE<-EDGE %>% select(id,to, weight, n.x, everything())
# names(EDGE)[1:4]<- c('from','to','weight','n')
# EDGE<-EDGE[,-c(8,10)]
#
# EDGE<-left_join(EDGE,nodes2,by= join_by(to == old.id))
#
# EDGE<-EDGE %>% select(from,id, weight, n.x, everything())
# names(EDGE)[c(1:4)]<- c('from','to','weight','n')
# EDGE<-EDGE[,-c(5,11)]
#
# TOTAL_EDGE<-bind_rows(edges, EDGE)
# edges
#
# library(network)
#
# routes_network <- network(TOTAL_EDGE,
#                           vertex.attr = TOTAL_node,
#                           matrix.type = "edgelist",
#                           ignore.eval = FALSE,
#                           multiple=TRUE)
# class(routes_network)
# plot(
#   routes_network,
#   displaylabels = TRUE,  # Afficher les étiquettes
#   vertex.cex = sqrt(sqrt(TOTAL_node$n/10)),        # Taille initiale des points
#   vertex.label.dist = 1.5)
#
#
#
# routes_tidy <- tbl_graph(nodes = TOTAL_node,
#                          edges = TOTAL_EDGE[,c(1:4)],
#                          directed = TRUE)
#
# routes_igraph <- graph_from_data_frame(d = TOTAL_EDGE,
#                                        vertices = TOTAL_node,
#                                        directed = TRUE)
# plot(routes_igraph,
#      vertex.size = 10)
#
#
# routes_igraph_tidy <- as_tbl_graph(routes_igraph)
#
#
# routes_igraph_tidyDB <- tbl_graph(nodes=nodes, edges=edges, directed=TRUE)
#
#
# routes_tidy %<>%
#   activate(edges) %>%
#   arrange(desc(weight))
#
#
# ggraph(routes_tidy, layout = "auto") +
#   geom_edge_link() +
#   geom_node_point() +
#   theme_graph()
#
# ggraph(routes_igraph_tidy, layout = "dendrogram", circular=FALSE) +
#   geom_edge_bend()+
#   geom_node_text(aes(label=n)) +
#  # geom_edge_link() +
#   geom_node_point() +
#   theme_graph()
#
#
# ggraph(routes_tidy, layout="dendrogram", circular=FALSE) +
#   geom_edge_diagonal(aes(edge_width=sqrt(weight)),
#                  alpha=0.7, show.legend=FALSE) +
#   # geom_node_point(aes(filter=leaf, shape=label),
#   #                 size=5, colour="black")+
#   geom_node_point(aes(filter=leaf,size=n, fill=n>=200),
#                   shape=21, show.legend=FALSE) +
#   geom_node_point(aes(filter=!leaf,size=c(1,TOTAL_EDGE$weight), fill=c(1,TOTAL_EDGE$weight)>=200),
#                   shape=21, show.legend=FALSE) +
#    # geom_node_text(aes(filter=leaf, label=c("A",TOTAL_EDGE$Intervention_reclass)),
#    #                hjust=1, nudge_y=-0.05, angle=90) +
#   geom_node_text(aes(filter=!leaf, label=c("A",TOTAL_EDGE$Intervention_reclass)),
#                  nudge_y=-0.05) +
#   theme_pubr()
#
#
#
# ggraph(routes_tidy, layout="dendrogram", circular=FALSE) +
#   scale_y_continuous(sec.axis = dup_axis(name=NULL)) +
#   geom_edge_bend(aes(edge_width=weight),
#                  alpha=0.7, show.legend=FALSE)
#
#
#
# ggraph(routes_tidy, layout = "dendrogram") +
#   geom_edge_diagonal(range = c(0.2, 2))+
#  # scale_edge_width(range = c(0.2, 2)) +
#   geom_node_text(aes(label = label), repel = TRUE) +
#   theme_graph()
#
# ggraph(routes_tidy, layout = "graphopt") +
#   geom_node_point() +
#   geom_edge_link(aes(width = weight), alpha = 0.8) +
#   scale_edge_width(range = c(0.2, 2)) +
#   geom_node_text(aes(label = label), repel = TRUE) +
#   labs(edge_width = "Letters") +
#   theme_graph()
#
#
# TOTAL_node$label <- TOTAL_node$label
# categories <- c("alley-cropping", "fallows", "multi", "hedgerows", "various", "parkland")
#
# # Utiliser str_extract pour extraire les catégories souhaitées
# TOTAL_node$new_label <- str_extract(TOTAL_node$label, paste(categories, collapse = "|"))
#
# labels_uniques <- unique(TOTAL_node$new_label)
# # Associez des couleurs à chaque label unique
# couleurs <- c("slategrey", "tomato", "gold", "forestgreen", "royalblue", "darkorange")
#
# # Créez une colonne 'color.background' basée sur la correspondance avec les couleurs
# TOTAL_node$color.background <- couleurs[match(TOTAL_node$new_label, labels_uniques)]
#
# TOTAL_node$color.border <- "black"
# TOTAL_node$size <- sqrt(sqrt(TOTAL_node$n)*20)
# TOTAL_node$color.highlight.border <- "darkred"
# TOTAL_node$label <- c("..","Alley-cropping", "Fallows", "Mulmtistrata","Hedgerows","Various","Parkland",
#                       "<2 sp", ">2 sp","<2 sp", ">2 sp","<2 sp","<2 sp", ">2 sp", ">2 sp", ">2 sp", ">2 sp","<2 sp", "<2 sp",
#                       "no Timber","no Timber","no Timber","no Timber","Timber","Timber","Timber","no Timber","no Timber",
#                       "Timber","Timber","no Timber","no Timber","no Timber","Timber","Timber","no Timber","no Timber","no Timber","Timber",
#                       "Timber","Na","Timber","Timber")
# TOTAL_node$title <- paste(TOTAL_node$label," Number of comparisons :" ,TOTAL_node$n)
# #TOTAL_node$size <- TOTAL_node$n/100
#
# visNetwork(TOTAL_node, TOTAL_EDGE)%>%
#   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>% visLegend() %>%
#   visInteraction(navigationButtons = TRUE)%>%
#  # visOptions(manipulation = TRUE)%>%
#  # visHierarchicalLayout() %>%
#   addFontAwesome() %>%
#   visGroups(groupname = "alley-cropping", shape = "icon", icon = list(code = "f0c0", size = 75))
#
# nodes <- data.frame(id = 1:3, label=2014:2016 ,font.size =(1:3)*10,shape='circle')
# edges <- data.frame(from = c(1,2), to = c(1,3))
# visNetwork(nodes, edges, width = "100%")
#
#
#
# edges <- mutate(edges, width = weight/1000 )
#
# visNetwork(nodes, edges) %>%
#   visIgraphLayout(layout = "layout_with_fr") %>%
#   visEdges(arrows = "middle")
#
# nodes_d3 <- mutate(TOTAL_node, id = id - 1)
# edges_d3 <- mutate(TOTAL_EDGE, from = from - 1, to = to - 1)
# forceNetwork(Links = edges_d3, Nodes = nodes_d3,
#              Source = "from", Target = "to",
#              NodeID = "label", Group = "id",
#              opacity = 0.4, fontSize = 1, zoom = TRUE)
#
#
# sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
#               NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")
#
# TAB_F2 <- TAB_F %>%
#   activate(nodes)
#
#
# iris_clust <- hclust(dist(TAB_F[1:4]))
# iris_tree <- as_tbl_graph(iris_clust)
#
# AA<-create_tree(20, 3)
# str(AA)
# %>%
#   mutate(leaf = node_is_leaf(), root = node_is_root()) %>%
#   ggraph(layout = 'tree') +
#   geom_edge_diagonal() +
#   geom_node_point(aes(filter = leaf), colour = 'forestgreen', size = 10) +
#   geom_node_point(aes(filter = root), colour = 'firebrick', size = 10) +
#   theme_graph()
#
#
# require(tidygraph)
# gr <- create_tree(mygraph)
# %>%
#   mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
#   activate(edges) %>%
#   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#
# gr <- create_complete(10) %>%
#   mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
#   activate(edges) %>%
#   mutate(class = sample(letters[1:3], n(), replace = TRUE))
#
#
# gr <-mygraph
# %>%
#   set_edge_attr("class", value = sample(letters[1:3], ecount(mygraph), replace = TRUE))
#
# # Print the resulting graph
# mygraph
#
# ggraph(routes_tidy, 'tree') +
#   geom_edge_diagonal(aes(alpha = after_stat(index)))
#
#
# # Créer le graph
# mygraph <- graph_from_data_frame(edge_list)
#
# # Obtenir les données pour networkD3
# network_data <- as.data.frame(get.edgelist(mygraph))
# network_data$group <- V(mygraph)$group[match(network_data$from, V(mygraph)$name)]
#
# nodes_data <- data.frame(name = unique(c(network_data$V1, network_data$V2)), group = unique(network_data$V1))
#
# forceNetwork(Links = network_data,Nodes = nodes_data)
#
# forceNetwork(Links = network_data, Nodes = NULL, Source = "from", Target = "to", NodeID = "from", Group = "group", opacity = 0.8)
#
#
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
#
#
# #PCAshiny(TAB_FINALE)
# #MFAshiny(TAB_FINALE)
#
# newDF <- TAB_FINALE[,c("PROP_Woody2","NB_strates","Human_trait","Spatial","DEsnity_tree_total")]
# #TAB_FINALE <- read.csv("~/Documents/HORA_Analyse/TAB_FINALE.HORA.csv")
# res.MFA<-MFA(newDF,group=c(1,1,1,1,1), type=c("s","s",'n',"n","s"),graph=FALSE)
#
#
# fviz_screeplot(res.MFA) ## 4 axers
# fviz_mfa_var(res.MFA, "group")
# fviz_mfa_var(res.MFA, "quanti.var", palette = "jco",
#              col.var.sup = "violet", repel = TRUE)
# fviz_mfa_ind(res.MFA, col.ind = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE)
# #fviz_mfa_ind(res.MFA, partial = "all")
# fviz_mfa_axes(res.MFA)
# fviz_contrib(res.MFA, choice = "quanti.var", axes = 1, top = 20,
#              palette = "jco")
# fviz_contrib(res.MFA, choice = "quanti.var", axes = 2, top = 20,
#              palette = "jco")
#
# hc.pca <- HCPC(res.MFA, nb.clust=-1)
# HH<-hc.pca$data.clust
# TAB_FINALE$GROUP<-HH$clust
#
# TAB<-TAB_FINALE
# NN<- length(TAB)-1
#
# TAB_F
# TAB_F$group <- as.factor(interaction(TAB_F$Intervention_reclass))
# VERIF<-TAB_F %>% filter(group=="alley-cropping")
# hist(VERIF$DEsnity_tree_total)
# summary(VERIF$DEsnity_tree_total)
# length(VERIF$DEsnity_tree_total)
#
# TAB_FINALE$total_density<-as.numeric(as.factor(TAB_FINALE$total_density))
# TAB_FINALE$`farm type`<-tolower(TAB_FINALE$`farm type`)

#TAB_FINALE<-TAB_FINALE[,-c(5,38)]
names(TAB_FINALE)
TAB_FINALE <- TAB_FINALE %>%
  mutate_at(vars(starts_with("prop_")), funs(. * 100))

res = catdes(TAB_FINALE, num.var=8, proba=0.05)


TAB<-NULL
for(i in 1 : length(res$quanti)){
#  HH[[i]]<-paste('GROUP',i)
  TAB[[i]]<- data.frame(res$quanti[[i]]) %>%
    dplyr::select(Mean.in.category,Overall.mean) %>%
    dplyr::rename( !!paste('GROUP',i) := Mean.in.category)
  TAB[[i]]$var<-row.names(TAB[[i]])
}

TAB<-Reduce(function(x, y) full_join(x, y), TAB)%>%
   mutate(across(where(is.numeric), round, 1))%>%
  relocate(var, .before = Overall.mean) %>%
  relocate(`GROUP 1`, .after = Overall.mean)%>%
  mutate(across(contains("GROUP"), ~replace(., is.na(.), 999)))


TAB$var <- c("Herb_hort","Annual","Herb_hort",
             "Edible_species","Annual2","Woody_hort",
             "Poll:Insects","Poll:insects","Woody_hort",
              "climber","Age_max_tree","prop_climber",
              "N_fixing","species","Woody_evergreen",
              "Shrub","Shrub", "IUCN: EN_VU_NT_CR",
              "IUCN: EN_VU_NT_CR","other","Wild.y",
              "Wild.x","woody","max_Height",
              "Prop_wild.y","Prop_wild.x","mean_Height",
              "Woody","woody_evergreen","medicinal",
               "Strates","N_Fixing","Density" )


TAB$unit <- c("%","Nb","Nb",
             "Nb","Nb","%",
              "%","Nb","Nb",
              "Nb","years","%",
              "%","Nb","Nb",
              "Nb","%","%",
              "%","?","Nb",
             "Nb","woody","m",
              "%","%","m",
              "%","%" ,"Nb",
              "Nb","Nb","tree.ha-1" )


TAB %<>%
relocate(unit, .after = var) %>%
  dplyr::filter(!var %in% c('ID','numéro','Woody2')) %>%
  arrange(var)

names(TAB)[1]<- c("Variable")

TAB<-TAB[c(31,2,17,21,23,33,6,5,18,1,4,8,11,12,14,26,27),]


TAB<-rbind(TAB,
c("Number_Data","Number", sum(data.frame(table(TAB_FINALE$GROUP))$Freq),data.frame(table(TAB_FINALE$GROUP))$Freq))


with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}

colnames(TAB)[c(4:8)]<-levels(factor(TAB_FINALE$Categorie))


reactable(TAB,defaultPageSize=20,columns = list(
  `(NH_Herb), H_Herb, H_Wood`  = colDef(
    style = function(value, index) {
      if (TAB$`(NH_Herb), H_Herb, H_Wood` [index] >= TAB$Overall.mean[index] ) {
        color <- "#008000"
      } else if (TAB$`(NH_Herb), H_Herb, H_Wood` [index] < TAB$Overall.mean[index] ){
        color <- "#e00000"
      }
      list(color = color)
    }
  ),
  `(NH_Herb), H_Wood` = colDef(
    style = function(value, index) {
      if (TAB$`(NH_Herb), H_Wood`[index] >= TAB$Overall.mean[index] ) {
        color <- "#008000"
      } else if (TAB$`(NH_Herb), H_Wood`[index] < TAB$Overall.mean[index] ){
        color <- "#e00000"
      }
      list(color = color)
    }
  ),
  `(NH_Herb), NH_Wood, H_Herb` = colDef(
    style = function(value, index) {
      if (TAB$`(NH_Herb), NH_Wood, H_Herb`[index] >= TAB$Overall.mean[index] ) {
        color <- "#008000"
      } else if (TAB$`(NH_Herb), NH_Wood, H_Herb`[index] < TAB$Overall.mean[index] ){
        color <- "#e00000"
      }
      list(color = color)
    }
  ),
  `(NH_Herb), NH_Wood, H_Herb, H_Wood` = colDef(
    style = function(value, index) {
      if (TAB$`(NH_Herb), NH_Wood, H_Herb, H_Wood`[index] >= TAB$Overall.mean[index] ) {
        color <- "#008000"
      } else if (TAB$`(NH_Herb), NH_Wood, H_Herb, H_Wood`[index] < TAB$Overall.mean[index] ){
        color <- "#e00000"
      }
      list(color = color)
    }
  ),
  `(NH_Herb), NH_Wood, H_Wood` = colDef(
    style = function(value, index) {
      if (TAB$`(NH_Herb), NH_Wood, H_Wood`[index] >= TAB$Overall.mean[index] ) {
        color <- "#008000"
      } else if (TAB$`(NH_Herb), NH_Wood, H_Wood`[index] < TAB$Overall.mean[index] ){
        color <- "#e00000"
      }
      list(color = color)
    }
  )
))


#### Categorial


TABQ<-NULL
for(i in 1 : length(res$quanti)){
  #  HH[[i]]<-paste('GROUP',i)
  TABQ[[i]]<- data.frame(res$category[[i]]) %>%
    dplyr::select(Mod.Cla,Global) %>%
    dplyr::rename( !!paste('GROUP',i) := Mod.Cla)
  TABQ[[i]]$var<-row.names( TABQ[[i]])
}

TABQ<-Reduce(function(x, y) full_join(x, y), TABQ) %>%
  mutate(across(where(is.numeric), round, 1))%>%
  relocate(var, .before = Global) %>%
  relocate(`GROUP 1`, .after = Global)%>%
  mutate(across(contains("GROUP"), ~replace(., is.na(.), 999))) %>%
  separate(var, c("Variable", "Modality"), sep="=")

TABQ$Variable<- c("Typo_agroF_PAUL","Spatial_organisation","Human_traitment", "Typo_agroF_PAUL","Typo_agroF_PAUL", "Typo_agroF_PAUL","Typo_agroF_PAUL","Human_traitment" ,"Typo_agroF_PAUL","Spatial_organisation")
TABQ$unit<- c("%","%","%", "%","%", "%","%","%" ,"%","%")
TABQ %<>% relocate(unit, .after = Modality)


GROUP <- dplyr::group_by(TABQ, `Variable`) %>%
  dplyr::summarize(Number = dplyr::n())

reactable(
  GROUP,
  details = function(index) {
    sales <- filter(TABQ, `Variable` == GROUP$`Variable`[index]) %>% select(-`Variable`)
    tbl <- reactable(sales, columns = list(
      Modality = colDef(footer = "Total"),
      `GROUP 1` = colDef(
        style = function(value, index) {
          if (TABQ$`GROUP 1`[index] >= TABQ$Global[index] ) {
            color <- "#008000"
          } else if (TABQ$`GROUP 1`[index] < TABQ$Global[index] ){
            color <- "#e00000"
          }
          list(color = color)
        },
        footer = function(values) sum(values)
      ),
      `GROUP 2` = colDef(
        style = function(value, index) {
          if (TABQ$`GROUP 2`[index] >= TABQ$Global[index] ) {
            color <- "#008000"
          } else if (TABQ$`GROUP 2`[index] < TABQ$Global[index] ){
            color <- "#e00000"
          }
          list(color = color)
        },
        footer = function(values) sum(values)
      ),
      `GROUP 3` = colDef(
        style = function(value, index) {
          if (TABQ$`GROUP 3`[index] >= TABQ$Global[index] ) {
            color <- "#008000"
          } else if (TABQ$`GROUP 3`[index] < TABQ$Global[index] ){
            color <- "#e00000"
          }
          list(color = color)
        },
        footer = function(values) sum(values)
      ),
      `GROUP 4` = colDef(
        style = function(value, index) {
          if (TABQ$`GROUP 4`[index] >= TABQ$Global[index] ) {
            color <- "#008000"
          } else if (TABQ$`GROUP 4`[index] < TABQ$Global[index] ){
            color <- "#e00000"
          }
          list(color = color)
        },
        footer = function(values) sum(values)
      ),
      `GROUP 5` = colDef(
        style = function(value, index) {
          if (TABQ$`GROUP 5`[index] >= TABQ$Global[index] ) {
            color <- "#008000"
          } else if (TABQ$`GROUP 5`[index] < TABQ$Global[index] ){
            color <- "#e00000"
          }
          list(color = color)
        },
        footer = function(values) sum(values)
      )
    ))
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)

