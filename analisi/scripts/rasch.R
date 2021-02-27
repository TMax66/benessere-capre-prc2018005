library("tidyverse")
library("lubridate")
library("here")
library("hrbrthemes")
library("readxl")
library("eRm")
library("ltm")
library("difR")
library("hablar")
library("mirt")
options(scipen = 999)

# dati <- read_excel("analisi/data/raw/Dataset.xlsx", 
#                       sheet = "gruppoITEM-A")


dati <- read_excel(here("analisi", "data", "raw", "Dataset.xlsx"), 
                        sheet = "Items")

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
  

summary(dt)

dt.poly <- dt[, c(4,5,6,11,12,13,14)]

dt.poly <- dt[, 4:14]
dt.poly <- dt.poly %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate_all(~.-1)

mod <- mirt(data=dt.poly, 
            model = 1, 
            itemtype = "gpcm")


itemplot(mod, 7, type = "info")

plot(mod, type = "info")

plotPImap(PCM(dt.poly))

PCM(dt.poly)

data("pcmdat")
