source("analisi/scripts/start.R")
#Modello----
## specificazione del modello: 
## outcome: kgcapo
## predittore: benessere
## altre varibili : stato sanitario, biosicurezza, dimensione allev, tipologia di lattazione ( lunga/ breve)
#stato sanitario ( prev intrall di paratbc, caev , mastiti, pseudotub
##             biosicurezza ( confondente?)
   

library(dagitty)
library(ggdag)

dag <- dagify(Produzione~Benessere+Stato_Sanitario, #+ Herd_Size+ Lattazione+ Tempo + Razza, 
              
              Benessere~Stato_Sanitario, 
              # Stato_Sanitario~Herd_Size, 
              # Stato_Sanitario~Biosicurezza,
              # Biosicurezza~Herd_Size, 
              # Lattazione~Tempo, 
              # Stato_Sanitario~Razza, 
              
              
              
              exposure =  "Benessere", 
              outcome = "Produzione", 
             # latent = "Razza", 
              
              labels = c("Produzione" = "Produzione", 
                         "Benessere" = "Benessere", 
                         "Stato_Sanitario" = "Stato\n Sanitario")
                         # "Lattazione" = "Lattazione", 
                         # "Biosicurezza"= "Biosicurezza", 
                         # "Herd_Size"= "Herd\n Size", 
                         # "Tempo" = "Tempo", 
                         # "Razza" = "Razza")
              
              )



ggdag(dag, text = FALSE, use_labels = "label", node_size = 3)

ggdag_exogenous(dag)
ggdag_paths(dag, text = FALSE, use_labels = "label", shadow = TRUE)


ggdag_adjustment_set(dag)+theme_dag()

ggdag_dseparated(dag, controlling_for = c("Lattazione"), 
                 text = FALSE, use_labels = "label", collider_lines = FALSE)

-------------------------------------------------------------------------------
#Old Stuff----------

# wel <- ben %>% 
#   left_join(welfscore, by= "azienda") %>% 
#   mutate(bencat = cut(complben, quantile(complben), include.lowest = TRUE), 
#          scorecat = cut(score, quantile(score), include.lowest = T))
# 
# 
# 
# table(wel$bencat, wel$scorecat)


# prod_latte <- az %>% 
#   mutate(azienda=casefold(azienda, upper = TRUE),
#          mese=recode(mese,
#                      gennaio=1,febbraio=2,marzo=3,aprile=4,
#                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
#                      ottobre=10, novembre=11,dicembre=12), 
#          time=as.Date(paste(anno, mese, 15, sep="-")), 
#          prelievo = anno+mese) %>%  
#   drop_na(kgcapo) %>% 
#   left_join(
#     (welfscore %>% 
#         select(azienda, score) %>% 
#        mutate(
#          # bencat = cut(score, quantile(score), include.lowest = T), 
#          #      randomvalue = rnorm(nrow(.), 0,1),
#          #      rcat = cut(randomvalue, quantile(randomvalue), include.lowest = T), 
#        azienda=casefold(azienda, upper = TRUE))), by='azienda')
# 
# 
# 
# 
# 
# 
#   # ggplot(aes(x=time, y = kgcapo))+
#   # facet_wrap(rcat~., nrow = 1) + stat_smooth()+
#   # geom_line(aes(x=time, y = kgcapo, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
#   # theme_ipsum_rc()
# 
#  
# ###esamino l'andamento nel tempo della variabile kgcapo delle singole aziende
#   prod_latte %>% 
#   select(azienda, time, kgcapo, score, caprelatt, bencat) %>% 
#   group_by(azienda) %>% 
#   mutate(inmilk = mean(caprelatt), 
#          N= n()) %>% 
#   select(-caprelatt) %>% 
#   filter(N >= 10) %>% 
#   arrange(time) %>% 
#   #pivot_wider(names_from = time, values_from = kgcapo) %>% 
#   ggplot(aes(x = time, y = kgcapo)) +
#   geom_point() + geom_line()+
#   #coord_cartesian(ylim = c(1, 6)) +
#     stat_smooth(method = "loess", se = F, span = .9)+
#   theme(panel.grid = element_blank()) +
#   facet_wrap(~azienda)
#     
# ###esamino l'intero set delle traiettore smoothed
#   
#   prod_latte %>% 
#     select(azienda, time, kgcapo, score, caprelatt, bencat) %>% 
#     group_by(azienda) %>% 
#     mutate(inmilk = mean(caprelatt), 
#            N= n()) %>% 
#     select(-caprelatt) %>% 
#     filter(N >= 10) %>% 
#     arrange(time) %>% 
#     ggplot(aes(x = time, y = kgcapo)) +
#     stat_smooth(method = "lm", se = F, span = .9, size = 2) +
#     stat_smooth(aes(group = azienda),
#                 method = "lm", se = F, span = .9, size = 1/4)
#     
#     
# ###Smooting the empirical trajectory using single-level Bayesian regression
#   
#   by_az <- 
#     prod_latte %>% 
#     group_by(azienda) %>% 
#     nest()
# 
# library(brms)
# 
#   dati <- by_az$data[[1]]
#   fit2.1 <-
#     brm(data = dati, 
#         formula = kgcapo ~ 1 + prelievo,
#         prior = prior(normal(0, 2), class = b),
#         iter = 4000, chains = 4, cores = 4,
#         seed = 2)
# 
# 
# 
# prod_latte %>%
#   ggplot(aes(x = time, y = kgcapo)) +
#   geom_point() + geom_line()+
#   coord_cartesian(ylim = c(1, 4)) +
#   theme(panel.grid = element_blank()) +
#   facet_wrap(~azienda)
# 
# 
# #uso ben##
# # az %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(kgcapo) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = kgcapo))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = kgcapo, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# 
# 
# ##milk
# ### uso welfscore
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(scc) %>% 
# #   left_join(
# #     (welfscore %>% 
# #        dplyr::select(azienda, score) %>% 
# #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# #   ggplot(aes(x=time, y = log(scc)))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time, y = log(scc), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# 
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(proteine) %>% 
# #   left_join(
# #     (welfscore %>% 
# #        dplyr::select(azienda, score) %>% 
# #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# #   ggplot(aes(x=time, y = proteine))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time, y = proteine, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(grasso) %>% 
# #   left_join(
# #     (welfscore %>% 
# #        dplyr::select(azienda, score) %>% 
# #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# #   ggplot(aes(x=time, y = grasso))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time, y = grasso, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# #  
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(lattosio) %>% 
# #   left_join(
# #     (welfscore %>% 
# #        dplyr::select(azienda, score) %>% 
# #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# #   ggplot(aes(x=time, y = lattosio))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time, y = lattosio, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(caseina) %>% 
# #   left_join(
# #     (welfscore %>% 
# #        dplyr::select(azienda, score) %>% 
# #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# #   ggplot(aes(x=time, y = caseina))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time, y = caseina, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# #   drop_na(cbt) %>% 
# #   left_join(
# #     (welfscore %>% 
# #        dplyr::select(azienda, score) %>% 
# #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% glimpse()
# #   ggplot(aes(x=time, y = log(cbt)))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time, y = log(cbt), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = log(scc)))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = log(scc), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# 
# 
# 
# 
# 
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = proteine))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = proteine, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = grasso))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = grasso, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# 
# # 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = lattosio))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = lattosio, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# 
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = caseina))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = caseina, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = log(cbt)))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = log(cbt), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# 
# 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = ureaFTIR))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = ureaFTIR, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # 
# # milk %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = ureapHm))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# #   geom_line(aes(x=time.x, y = ureapHm, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # sanit %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = `paratbc(%)`))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth(se = FALSE)+
# #   geom_line(aes(x=time.x, y =`paratbc(%)`, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # sanit %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #   left_join( (ben %>% 
# #                 mutate(azienda=casefold(azienda, upper = TRUE),
# #                        mese=recode(mese,
# #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                                    ottobre=10, novembre=11,dicembre=12), 
# #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# #                 filter(anno != 2020 ) %>% 
# #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# #   ), by = "azienda") %>% 
# #   drop_na(bencat) %>% 
# #   ggplot(aes(x=time.x, y = `caev(%)`))+  
# #   facet_wrap(bencat~., nrow = 1) + stat_smooth(se = FALSE)+
# #   geom_line(aes(x=time.x, y =`caev(%)`, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   theme_ipsum_rc()
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # d2 <-read_sheet(id$id, sheet ="massa" ) %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-")))
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(scc) %>% 
# #   ggplot(aes(x=time, y=scc, group=azienda))+geom_line()+
# #   scale_y_log10()
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(proteine) %>% 
# #   ggplot(aes(x=time, y=proteine, group=azienda))+geom_line()
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(caseina) %>% 
# #   ggplot(aes(x=time, y=caseina, group=azienda))+geom_line()
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(grasso) %>% 
# #   ggplot(aes(x=time, y=grasso, group=azienda))+geom_line()
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(lattosio) %>% 
# #   ggplot(aes(x=time, y=lattosio, group=azienda))+geom_line()
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(cbt) %>% 
# #   ggplot(aes(x=time, y=cbt, group=azienda))+geom_line()+scale_y_log10()
# # 
# # d2 %>% 
# #   group_by(azienda, time) %>% 
# #   drop_na(ureaFTIR) %>% 
# #   ggplot(aes(x=time, y=ureaFTIR, group=azienda))+geom_line()+scale_y_log10()+
# #  
# # 
# # 
# # 
# # d3 <-read_sheet(id$id, sheet ="san" )
# # d4 <-read_sheet(id$id, sheet ="par" )
# # d5 <-read_sheet(id$id, sheet ="diagn" )
# # 
# # d6 <-read_sheet(id$id, sheet ="ben" )
# # 
# # 
# # library(GGally)
# # ggpairs(d6[,4:9])+ theme_bw()
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# 
# # 
# # 
# # 
# # 
# # 
# # 
# # library(GGally)
# # ggpairs(d6[,4:9])+ theme_bw()
# # 
# # summary(d6$complben)
# # sd(d6$complben)^2
# # 
# # summary(d6$areaA)
# # sd(d6$areaA)^2
# # 
# # summary(d6$areaB)
# # sd(d6$areaB)^2
# # 
# # summary(d6$gr)
# # sd(d6$gr)^2
# # 
# # 
# # d4 %>% 
# #     filter(azienda=="039BG069") %>% 
# #     group_by(mese, cat) %>% 
# #     summarise(strongili=mean(`strongiliGE (upg)`, na.rm=T))%>% 
# #     ggplot(aes(x=mese, y=strongili, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)
# # 
# # 
# # d4 %>% 
# #   filter(azienda=="039BG069") %>% 
# #   group_by(mese, cat) %>% 
# #   summarise(coccidi=mean(`coccidi (upg)`, na.rm=T))%>% 
# #   ggplot(aes(x=mese, y=coccidi, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)
# # 
# # 
# # 
# # 
# # ####SCC grafico
# # 
# # d2 %>% 
# #   group_by(azienda,mese) %>% 
# #   summarise(scc=geometric.mean(scc, na.rm=T))%>% 
# #   ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# #   
# # 
# # 
# # 
# # d2 %>% 
# #   group_by(azienda,mese) %>% 
# #   summarise(cbt=geometric.mean(cbt, na.rm=T))%>% 
# #   ggplot(aes(x=mese, y=cbt, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # 
# # 
# # 
# # d2 %>% 
# #   group_by(azienda,mese) %>% 
# #   summarise(proteine=mean(proteine, na.rm=T))%>% 
# #   ggplot(aes(x=mese, y=proteine, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # 
# # 
# # d2 %>% 
# #   group_by(azienda,mese) %>% 
# #   summarise(grasso=mean(grasso, na.rm=T))%>% 
# #   ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # 
# # 
# # d2 %>% 
# #   group_by(azienda,mese) %>% 
# #   summarise(grasso=mean(grasso, na.rm=T))%>% 
# #   ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # dt<-d1 %>% 
# # #  full_join(d6,  by=c("mese","azienda")) %>% 
# # #   full_join(d2, by=c("mese","azienda")) %>% 
# # #    full_join(d3,by=c("mese","azienda")) 
# # 
# # 
# # 
# # # d2$controllo<-ifelse(d2$mese=="gennaio",1,
# # #                      ifelse(d2$mese=="febbraio", 2,
# # #                             ifelse(d2$mese=="marzo", 3,
# # #                                    ifelse(d2$mese=="aprile",4,
# # #                                           ifelse(d2$mese=="maggio",5,
# # #                                                  ifelse(d2$mese=="giugno",6,
# # #                                                         ifelse(d2$mese=="luglio",7,
# # #                                                            ifelse(d2$mese=="agosto",8,
# # #                                                                       ifelse(d2$mese=="settembre",9,
# # #                                                                              ifelse(d2$mese=="ottobre",10,
# # #                                                                                     ifelse(d2$mese=="novembre",11,
# # #                                                                                            ifelse(d2$mese=="dicembre",12,d2$mese))))))))))))
# # # d2$controllo<-as.numeric(d2$controllo)  
# # 
# # #####scc#
# # d2w<-d2 %>% 
# #   drop_na(azienda) %>% 
# #   arrange(mese) %>% 
# #   select(azienda,mese,scc) %>% 
# #   group_by(azienda,mese) %>% 
# #   summarise(scc=mean(scc)) %>% 
# #   # mutate(id=row_number()) %>% 
# #   # group_by(azienda,controllo) %>% 
# #   #arrange(controllo) %>% 
# #   pivot_wider(names_from=mese,values_from=scc ,values_fn = list(scc = mean))
# #  
# #   #select(-11)
# 
# # 
# # 
# # 
# # 
# # dwj<-d1 %>% 
# #   select(-anno) %>% 
# #   left_join(d2w,by=c("azienda")) %>% 
# #   left_join(d6[,-c(1:2)],by=c("azienda")) %>% 
# #   pivot_longer(13:22,
# #   names_to = "month", values_to = "scc") %>% 
# #   mutate(month=as.numeric(month)) %>% 
# #   group_by(azienda, month) %>% 
# #   summarise(scc=geometric.mean(scc, na.rm=T)) %>% 
# #   ggplot(aes(x=month, y=scc))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# #   
# # ###CBT###
# # d2w<-d2 %>% 
# #   select(azienda,cbt) %>% 
# #   group_by(azienda,controllo) %>% 
# #   summarise(cbt=mean(cbt)) %>% 
# #   # mutate(id=row_number()) %>% 
# #   # group_by(azienda,controllo) %>% 
# #   #arrange(controllo) %>% 
# #   pivot_wider(names_from=controllo,values_from=cbt ,values_fn = list(cbt = mean)) %>% 
# #   select(-11)
# # 
# # 
# # dwj<-d1 %>% 
# #   select(-mese) %>% 
# #   left_join(d2w,by=c("azienda")) %>% 
# #   left_join(d6[,-1],by=c("azienda")) %>% 
# #   pivot_longer(12:20,
# #                names_to = "mese", values_to = "cbt") %>% 
# #   mutate(mese=as.numeric(mese)) %>% 
# #   group_by(azienda, mese) %>% 
# #   summarise(proteine=geometric.mean(cbt, na.rm=T)) %>% 
# #   ggplot(aes(x=mese, y=proteine))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # 
# # ###Proteine
# # d2w<-d2 %>% 
# # select(azienda,controllo,proteine) %>% 
# #   group_by(azienda,controllo) %>% 
# #   summarise(proteine=mean(proteine)) %>% 
# #   # mutate(id=row_number()) %>% 
# #   # group_by(azienda,controllo) %>% 
# #   #arrange(controllo) %>% 
# #   pivot_wider(names_from=controllo,values_from=proteine ,values_fn = list(proteine = mean)) %>% 
# #   select(-11)
# # 
# # 
# # dwj<-d1 %>% 
# #   select(-mese) %>% 
# #   left_join(d2w,by=c("azienda")) %>% 
# #   left_join(d6[,-1],by=c("azienda")) %>% 
# #   pivot_longer(12:20,
# #                names_to = "mese", values_to = "proteine") %>% 
# #   mutate(mese=as.numeric(mese)) %>% 
# #   group_by(azienda, mese) %>% 
# #   summarise(proteine=mean(proteine, na.rm=T)) %>% 
# #   ggplot(aes(x=mese, y=proteine))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # 
# # ####Grasso
# # d2w<-d2 %>% 
# #   select(azienda,controllo,grasso) %>% 
# #   group_by(azienda,controllo) %>% 
# #   summarise(grasso=mean(grasso)) %>% 
# #   # mutate(id=row_number()) %>% 
# #   # group_by(azienda,controllo) %>% 
# #   #arrange(controllo) %>% 
# #   pivot_wider(names_from=controllo,values_from=grasso ,values_fn = list(grasso = mean)) %>% 
# #   select(-11)
# # 
# # 
# # dwj<-d1 %>% 
# #   select(-mese) %>% 
# #   left_join(d2w,by=c("azienda")) %>% 
# #   left_join(d6[,-1],by=c("azienda")) %>% 
# #   pivot_longer(12:20,
# #                names_to = "mese", values_to = "grasso") %>% 
# #   mutate(mese=as.numeric(mese)) %>% 
# #   group_by(azienda, mese) %>% 
# #   summarise(grasso=mean(grasso, na.rm=T)) %>% 
# #   ggplot(aes(x=mese, y=grasso))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # 
# # ###Lattosio
# # d2w<-d2 %>% 
# #   select(azienda,controllo,lattosio) %>% 
# #   group_by(azienda,controllo) %>% 
# #   summarise(lattosio=mean(lattosio)) %>% 
# #   # mutate(id=row_number()) %>% 
# #   # group_by(azienda,controllo) %>% 
# #   #arrange(controllo) %>% 
# #   pivot_wider(names_from=controllo,values_from=lattosio ,values_fn = list(lattosio = mean)) %>% 
# #   select(-11)
# # 
# # 
# # dwj<-d1 %>% 
# #   select(-mese) %>% 
# #   left_join(d2w,by=c("azienda")) %>% 
# #   left_join(d6[,-1],by=c("azienda")) %>% 
# #   pivot_longer(12:20,
# #                names_to = "mese", values_to = "lattosio") %>% 
# #   mutate(mese=as.numeric(mese)) %>% 
# #   group_by(azienda, mese) %>% 
# #   summarise(lattosio=mean(lattosio, na.rm=T)) %>% 
# #   ggplot(aes(x=mese, y=lattosio))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# 
# # 
# # x<-d1 %>% 
# #   select(-mese) %>% 
# #   left_join(d2w,by=c("azienda")) %>% 
# #   left_join(d6[,-1],by=c("azienda")) 
