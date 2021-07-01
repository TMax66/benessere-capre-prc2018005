library(tidyverse)
library(here)
library(binom)
library(readxl)


dt <- read_excel("analisi/data/raw/TabellaTesiLivia.xlsx")

bayesPrev <- binom.bayes(
  x = dt$positivi, n = dt$campioni, type = "highest", conf.level = 0.95, tol = 1e-9)


bayesPrev<- cbind(dt[, c(1:3)], bayesPrev )


z <- binom.bayes.densityplot(bayesPrev)
z+facet_wrap(~codice)+xlab("Prevalenza")


library(epitools)
library(tidyverse)
N1<-366
y1<-134
N2<-364
y2<-158
rr<-riskratio(c(N1-y1, y1, N2-y2, y2), rev="rows", method = "boot", replicates=1e5)
df<-tibble(est=0.843, lw=0.703, up=1.008)
ci<-df %>% 
  ggplot(aes(x=est, y=1))+geom_point()+
  geom_segment( aes(x = lw,  xend = up, y=1, 
                    yend=1))+
  labs(x="Rischio Relativo")+
  xlim(0.5915964,1.199596)+
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank())+
  
  geom_text(aes(x = 0.703), y = 1.005, label = "0.703",size=3)+
  geom_text(aes(x = 1.008), y = 1.005, label = "1.008",size=3)+
  geom_text(aes(x = 0.843), y = 1.005, label = "0.843",size=3)+
  geom_text(aes(x = 0.843), y = 0.98, label = " RR=0.84[95%CI:0.703-1.008]",size=5)+
  geom_text(aes(x = 0.843), y = 0.97, label = "p=0.067",size=5)+
  geom_vline(aes(xintercept=1), color="red")+
  geom_vline(aes(xintercept=0.9), color="red", linetype=2)+
  geom_vline(aes(xintercept=1.1), color="red", linetype=2)+
  geom_text(aes(x = 1), y = 1.03, label = "Equivalenza",size=4)

