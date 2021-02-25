library("tidyverse")
library("lubridate")
library("here")
library("hrbrthemes")
library("readxl")
library("eRm")
library("ltm")
library("difR")
library("hablar")
options(scipen = 999)

dati <- read_excel("analisi/data/raw/Dataset.xlsx", 
                      sheet = "gruppoITEM-A")
dati <- dati %>% 
  filter(anno==2019)


dt <- dati %>% 
  convert(fct(addetti, formazione,gestionegr, ispezioni, cure, razione, acqua, puliziaabb, puliziaamb, mungitura, biosicurezza)) %>% 
  mutate(addetti=fct_relevel(addetti, "Accettabile", after = 1), 
         formazione=fct_relevel(formazione,"Accettabile", after = 1 ), 
         gestionegr=fct_relevel(gestionegr,"Accettabile", after = 1 ),
         puliziaabb=fct_relevel(puliziaabb,"Accettabile", after = 1 ),
         puliziaamb=fct_relevel(puliziaamb,"Accettabile", after = 1 ),
         mungitura=fct_relevel(mungitura,"Accettabile", after = 1 ),
         biosicurezza=fct_relevel(biosicurezza,"Accettabile", after = 1 )
         )
  




 
