library("tidyverse")
library("DT")
library("googledrive")
library("googlesheets4")
library("lubridate")
library("here")
library("hrbrthemes")
library(readxl)
library(rstanarm)
library(brms)
library(bayestestR)
library(see)
library(bayesplot)
library(parameters)
options(scipen = 999)


#Acquisizione dati rds----
az <- readRDS(here("analisi", "data", "processed", "azienda.RDS"))
sanit <- readRDS(here("analisi", "data", "processed", "sanitaria.RDS"))
sanit <- sanit %>% 
  select(- anno, -mese, -c(9:11)) %>% 
  group_by(azienda) %>%
  summarise(across (where(is.numeric),   ~ mean(.x, na.rm = TRUE)))  
ben <- readRDS(here("analisi", "data", "processed", "benessere.RDS"))
ben <- ben %>% 
  mutate(anno = ifelse(anno == 2017, 2019, anno)) %>% 
  filter(anno == 2019) 
welfscore <- readRDS(here("analisi", "data", "processed", "welfscore.RDS"))
lattazione <- read_excel("analisi/data/raw/LattazioneAziende.xlsx")


# latte di massa dal dataset originale----

massa <- read_excel("analisi/data/raw/prc2018005.xlsx", 
                         sheet = "massa")

par <- read_excel("analisi/data/raw/prc2018005.xlsx", 
                  sheet = "par")


#DATSET PER ANALISI---------------------------------------

df <- az %>% 
  left_join(
    (ben %>% 
       select(-mese, -anno)
    ), by = "azienda") %>%
  left_join(
    sanit, by = "azienda"
  ) %>% 
  mutate(mese=recode(mese,
                     gennaio=1,febbraio=2,marzo=3,aprile=4,
                     maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
                     ottobre=10, novembre=11,dicembre=12), 
         time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
  arrange(time) %>% 
  left_join(
    (lattazione %>% 
       mutate(azienda = str_to_lower(azienda))), 
    by = "azienda"
  ) %>% 
  
  left_join(
    (welfscore %>% 
       select(azienda, score)), 
    by = "azienda") %>% 
  
   left_join(
    (massa %>% select(azienda, scc)), 
    by = "azienda"
  )



# #codici per ottenere l'autorizzazione al drive di google da fare una sola volta###
# # options(gargle_oauth_cache = ".secrets")
# # gargle::gargle_oauth_cache()
# # drive_auth()
# # list.files(".secrets/")#<---questo codice fa solo vedere il file presente nella cartella .secrets creata dal codice
# #precedente... la cartella .secrets deve essere inserita tra i documenti da mettere nel deploy per le applicazioni shiny
#  
# 
# # #Accesso a googlesheet----------------------------------------------
# # 
# # options(
# #   gargle_oauth_cache = ".secrets",
# #   gargle_oauth_email = TRUE
# # )
# # drive_auth()
# # gs4_auth(token = drive_token())
# # mydrive<-drive_find(type = "spreadsheet")
# # id<-mydrive %>%
# #  filter(name=="prc2018005") %>%
# #   select(id)
#  