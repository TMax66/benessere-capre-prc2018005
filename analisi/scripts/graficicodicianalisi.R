sc<-d2 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese) %>% 
  summarise(scc=geometric.mean(scc, na.rm=T))%>% 
  ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()
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
