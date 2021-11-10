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
library(psych)
options(scipen = 999)




massa <- read_excel("analisi/data/raw/prc2018005.xlsx", 
                    sheet = "massa")
massa%>% 
  filter(azienda=="039bg069") %>% 
  group_by(mese) %>% 
  summarise(scc=geometric.mean(scc, na.rm=T))%>%  
  ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()


massa %>% 
  mutate(azienda=casefold(azienda, upper = TRUE),
         mese=recode(mese,
                     gennaio=1,febbraio=2,marzo=3,aprile=4,
                     maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
                     ottobre=10, novembre=11,dicembre=12), 
         time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
 
ggplot(aes(x=time, y = log(scc)))+  
  stat_smooth()+
  geom_line(aes(x=time, y = log(scc)), alpha=0.3) + geom_point(alpha = 0.3)+
  facet_wrap(~azienda, scales = "free")+
  theme_ipsum_rc()

















cb<-d2 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese) %>% 
  summarise(cbt=geometric.mean(cbt, na.rm=T))%>% 
  ggplot(aes(x=mese, y=cbt, group=1))+geom_point()+geom_line()
pr<-d2 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese) %>% 
  summarise(proteine=mean(scc, na.rm=T))%>% 
  ggplot(aes(x=mese, y=proteine, group=1))+geom_point()+geom_line()
gr<-d2 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese) %>% 
  summarise(grasso= mean(grasso, na.rm=T))%>% 
  ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()
lt<-d2 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese) %>% 
  summarise(lattosio=geometric.mean(lattosio, na.rm=T))%>% 
  ggplot(aes(x=mese, y=lattosio, group=1))+geom_point()+geom_line()
cs<-d2 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese) %>% 
  summarise(caseina=mean(scc, na.rm=T))%>% 
  ggplot(aes(x=mese, y=caseina, group=1))+geom_point()+geom_line()



plot_grid(sc,cb,pr,gr,lt,cs,ncol = 3)
