library(shiny)
library(DT)
library(dplyr)
library(tidyr)
#library(googlesheets)
library(ggplot2)
library(rpivotTable)
library(timevis)
library(janitor)
library(shinyBS)
library(shinydashboard)
library(gridExtra)
library(cowplot)
library(psych)
library(patchwork)
options(scipen = 999)

# token <- gs_auth(cache = FALSE)
# gd_token()
# saveRDS(token, file = "googlesheets_token.rds")

gs_auth(token = "googlesheets_token.rds")
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))




Sys.sleep(5)


sheet <- gs_title("prc2018005")
Sys.sleep(5)
d1 <-gs_read(sheet, ws="dataset" )
d1$mese<-factor(d1$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
Sys.sleep(2)
d2 <-gs_read(sheet, ws="massa" )
d2$mese<-factor(d2$mese, levels=c("gennaio", "febbraio", "marzo", "aprile","maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
Sys.sleep(2)
d3 <-gs_read(sheet, ws="san" )
d3$mese<-factor(d3$mese, levels=c("gennaio", "febbraio", "marzo", "aprile","maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
Sys.sleep(2)
d4 <-gs_read(sheet, ws="par" )
d4$mese<-factor(d4$mese, levels=c("gennaio", "febbraio", "marzo", "aprile","maggio", "giugno", "luglio", "agosto",
                               "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
names(d4)[c(5,6)]<-c("coccidi", "strongili")


Sys.sleep(2)
d5 <-gs_read(sheet, ws="diagn" )
d5$mese<-factor(d5$mese, levels=c("gennaio", "febbraio", "marzo", "aprile","maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
Sys.sleep(2)
d6 <-gs_read(sheet, ws="ben" )
d6$mese<-factor(d6$mese, levels=c("gennaio", "febbraio", "marzo", "aprile","maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)




# ds <-gs_read(dati, ws="dataset" )
#timing <- gs_read(sheet, ws="timing" )

#analisi<-gs_title("analisi")
#ana<-gs_read(analisi,ws="dati")

# fieldsAll<-c("mese","azienda", "ncapre","capog", "adultdead", "puppydead", "rim",
#           "abo","ter","asc","trim","tpuppy","scc","prot","cas","grasso","latt","cbt","urea", "inib",
#           "paratbc",	"agalassia",	"caev",	"mal.ascessi",	"mastite", "score")
#           


fieldsAll <- c("anno", "mese","azienda", "ncapre","asciutta", "rimonta", "capretti", "becchi","capog", "adultdead", "puppydead", "rim",
               "abo","ter","asc","trim","tpuppy")

fieldsAll2<-c("manno","mmese","mazienda", "scc","prot","cas","grasso","latt","cbt", "stau", "ureaFTIR","ureapHm", "inib")

fieldsAll3<-c("sanno","smese", "sazienda", "parat",	"agal",	"caev",	"ascessi",	"mast")

fieldsAll4<-c("panno", "pmese", "pazienda", "cat","coccidi","strGE","strPO")

fieldsAll5<-c("danno", "dmese","dazienda","dcat","necro","bat","diagnosi")

fieldsAll6<-c("banno","bmese","bazienda", "bcompl","A","B","C","Biosic","GR")




# 
# fielsAll4<-c(	"controllo", "azienda", "score")





#####FUNZIONE LOAD DATA- CARICA IL FILE DOPO L'AGGIUNTA DI NUOVI RECORD x dati aziendali#######
loadData <- function() {
  Sys.sleep(3)
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="dataset" )
  
  }

loadData2 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="massa")
  d2
  }

loadData3 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="san" )}

loadData4 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="par" )}
loadData5 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="diagn" )}

loadData6 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="ben" )}






# 
# loadData4 <- function() {
#   sheet <- gs_title("prc2018005")
#   ds <-gs_read(sheet, ws="wellness" )}

shinyjs::useShinyjs()



#ds %>% 
#  group_by(azienda) %>% 
#  summarise_at(c("paratbc","agalassia","caev","mal.ascessi","mastite"), sum, na.rm = TRUE)

