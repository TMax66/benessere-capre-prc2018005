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
library("DataExplorer")
options(scipen = 999)

# dati <- read_excel("analisi/data/raw/Dataset.xlsx", 
#                       sheet = "gruppoITEM-A")


dati <- read_excel(here("analisi", "data", "raw", "Dataset.xlsx"), 
                        sheet = "Items")

dt <- dati %>% 
  filter(anno==2019) %>% 
  dplyr::select(-oblivion) %>% 
  convert(fct(addetti, formazione,gestionegr, ispezioni,razione, acqua, puliziaabb, puliziaamb, mungitura, biosicurezza, 
              ripariest, stabulazione, supdecadulti, supdecubrimonta, boxbecchi, capretteria, mangiatoiaadulti, supabbadulti, infermeria, 
              manmungitura, tempumid, testlatenza, bcsadulte, puliziadulte, lesioneadulte, zoppie, unghioni, ascessi, mammella, 
              mortadulti, mortcapretti, mutilazioni, insettiroditori, contatti, ingressiestranei, ingressiabituali, disinfmezzi,
              contattimezzi, raccarcasse, caricoscaricoanim, aquistimov, quarantena, conoscepiani, monitorasan, infmamm, parassitosi,
              analisiacqua, approvacqua, controattrezz)) %>%  
  mutate(addetti=fct_relevel(addetti, "Accettabile", after = 1), 
         formazione=fct_relevel(formazione,"Accettabile", after = 1 ), 
         gestionegr=fct_relevel(gestionegr,"Accettabile", after = 1 ),
         puliziaabb=fct_relevel(puliziaabb,"Accettabile", after = 1 ),
         puliziaamb=fct_relevel(puliziaamb,"Accettabile", after = 1 ),
         mungitura=fct_relevel(mungitura,"Accettabile", after = 1 ),
         biosicurezza=fct_relevel(biosicurezza,"Accettabile", after = 1 ), 
         #ripariest = fct_relevel(ripariest,"Accettabile", after = 1 ), 
         stabulazione= fct_relevel(stabulazione,"Ottimale", after = 1 ),
         supdecubrimonta = fct_relevel(supdecubrimonta,"Accettabile", after = 1 ),
         boxbecchi = fct_relevel(boxbecchi,"Accettabile", after = 1 ),
         capretteria = fct_relevel(capretteria,"Accettabile", after = 1 ),
         mangiatoiaadulti= fct_relevel(mangiatoiaadulti,"Accettabile", after = 1 ),
         supabbadulti= fct_relevel(supabbadulti,"Accettabile", after = 1 ),
         infermeria= fct_relevel(infermeria,"Accettabile", after = 1 ),
         manmungitura= fct_relevel(manmungitura,"Accettabile", after = 1 ),
         tempumid= fct_relevel(tempumid,"Accettabile", after = 1 ),
         testlatenza= fct_relevel(testlatenza,"Accettabile", after = 1 ),
         #bcsadulte= fct_relevel(bcsadulte,"Accettabile", after = 1 ),
         #puliziadulte= fct_relevel(puliziadulte,"Accettabile", after = 1 ),
         lesioneadulte= fct_relevel(lesioneadulte,"Accettabile", after = 1 ),
         zoppie= fct_relevel(zoppie,"Accettabile", after = 1 ),
         unghioni= fct_relevel(unghioni,"Accettabile", after = 1 ),
         ascessi= fct_relevel(ascessi,"Accettabile", after = 1 ),
         mammella= fct_relevel(mammella,"Accettabile", after = 1 ),
         mortadulti= fct_relevel(mortadulti,"Accettabile", after = 1 ),
         mortcapretti= fct_relevel(mortcapretti,"Accettabile", after = 1 ),
         #mutilazioni= fct_relevel(mutilazioni,"Accettabile", after = 1 ),
         insettiroditori= fct_relevel(insettiroditori,"Accettabile", after = 1 ),
         contatti= fct_relevel(contatti,"Accettabile", after = 1 ),
         ingressiestranei= fct_relevel(ingressiestranei,"Accettabile", after = 1 ),
         disinfmezzi= fct_relevel(disinfmezzi,"Accettabile", after = 1 ),
         #contattimezzi= fct_relevel(contattimezzi,"Accettabile", after = 1 ),
         #raccarcasse= fct_relevel(raccarcasse,"Accettabile", after = 1 ),
         #caricoscaricoanim= fct_relevel(caricoscaricoanim,"Accettabile", after = 1 ),
         aquistimov= fct_relevel(aquistimov,"Accettabile", after = 1 ),
         quarantena= fct_relevel(quarantena,"Accettabile", after = 1 ),
         conoscepiani= fct_relevel(conoscepiani,"Accettabile", after = 1 ),
         #monitorasan= fct_relevel(monitorasan,"Accettabile", after = 1 ),
         infmamm= fct_relevel(infmamm,"Accettabile", after = 1 ),
         parassitosi= fct_relevel(parassitosi,"Accettabile", after = 1 ),
         #analisiacqua= fct_relevel(analisiacqua,"Accettabile", after = 1 ),
         approvacqua= fct_relevel(approvacqua,"Accettabile", after = 1 ),
         #controattrezz= fct_relevel(controattrezz,"Accettabile", after = 1 ),
         supdecadulti= fct_relevel(supdecadulti,"Accettabile", after = 1 ),
         parassitosi= fct_relevel(parassitosi,"Ottimale", after = 1 ),
         ) 
  

 


dt.poly <- dt[, 3:52]
dt.poly <- dt.poly %>% 
  mutate_if(is.factor, as.numeric) %>%  
  dplyr::mutate_if(is.numeric, ~.-1) 

mod <- mirt(data=dt.poly[, -1], 
            model = 1, 
            itemtype = "gpcm")


itemplot(mod, type = "trace")

plot(mod, type = "trace")

plotPImap(PCM(dt.poly[, -1]))

PCM(dt.poly)

data("pcmdat")
