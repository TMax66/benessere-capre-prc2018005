myfun <- function( dati, param)
{
  x <- dati %>% 
    mutate(azienda=casefold(azienda, upper = TRUE),
           mese=recode(mese,
                       gennaio=1,febbraio=2,marzo=3,aprile=4,
                       maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
                       ottobre=10, novembre=11,dicembre=12), 
           time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
   # drop_na(param) %>% 
    left_join(
      (welfscore %>% 
         dplyr::select(azienda, score) %>% 
         mutate(bencat = cut(score, quantile(score), include.lowest = T),
                azienda=casefold(azienda, upper = TRUE))), by='azienda')
  
    ggplot(x,aes(x=time, y = param))+  
    facet_wrap(bencat~., nrow = 1) + stat_smooth()+
    geom_line(aes(x=time, y = param, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
    theme_ipsum_rc()

}


myfun(dati = az, param = kgcapo)


x <- az %>% 
  mutate(azienda=casefold(azienda, upper = TRUE),
         mese=recode(mese,
                     gennaio=1,febbraio=2,marzo=3,aprile=4,
                     maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
                     ottobre=10, novembre=11,dicembre=12), 
         time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
  drop_na(kgcapo) %>% 
  left_join(
    (welfscore %>% 
       dplyr::select(azienda, score) %>% 
       mutate(bencat = cut(score, quantile(score), include.lowest = T),
              azienda=casefold(azienda, upper = TRUE))), by='azienda')  
  
ggplot(x, aes(x=time, y = kgcapo))+  
  facet_wrap(bencat~., nrow = 1) + stat_smooth()+
  geom_line(aes(x=time, y = kgcapo, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
  theme_ipsum_rc()
