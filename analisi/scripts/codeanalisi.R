source("analisi/scripts/start.R")

#Modello nullo----

dt <- df %>% 
  
  mutate(Azienda = factor(azienda), 
         Time = factor(paste(anno,mese)), 
         Time2 = factor(Time, levels = c(
                         "2019 1", "2019 2",  "2019 3","2019 4", "2019 5", "2019 6", "2019 7", "2019 8", 
                         "2019 9", "2019 10",  "2019 11",  "2019 12", 
                         "2020 1" ,  "2020 2" , "2020 3", "2020 4", "2020 5", "2020 6", "2020 7", "2020 8",
                         "2020 9", "2020 10",  "2020 11",  "2020 12")
                       ),
         Occasion = as.numeric(Time2), 
         Welfare = scale(complben), 
         WelfA = scale(areaA), 
         WelfB = scale(areaB), 
         WelfC = scale(areaC), 
         para = scale(`paratbc(%)`), 
         malasc = scale(`mal.ascessi(%)`), 
         caev = scale(`caev(%)`), 
         hsize = scale(caprelatt), 
         WScore = scale(score)) %>% 
  select(Azienda, azienda, Time2, Welfare,  para, malasc, caev, Occasion,  kgcapo, hsize, LATTAZIONE, 
         WelfA, WelfB, WelfC, WScore, adumorte,scc )
  



##modelli----

 
# M1  <- stan_lmer(formula = kgcapo~Welfare+(1|azienda)+(1|Occasion),   
#                          data = dt,
#                          seed = 349)
# M2 <- stan_lmer(formula = kgcapo~WelfA+(1|azienda)+(1|Occasion),   
#                 data = dt,
#                 seed = 349)
# 
# M3 <- stan_lmer(formula = kgcapo~WelfB+(1|azienda)+(1|Occasion),   
#                 data = dt,
#                 seed = 349)
# 
# M4 <- stan_lmer(formula = kgcapo~WelfC+(1|azienda)+(1|Occasion),   
#                 data = dt,
#                 seed = 349)
# 
# M5 <- stan_lmer(formula = kgcapo~WScore+(1|azienda)+(1|Occasion),   
#                 data = dt,
#                 seed = 349)

plot(p_direction(M4))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


# library(parameters)
# pM1 <- model_parameters(M1, effects= "random")
# pM2 <- model_parameters(M2, effects= "random")
# pM3 <- model_parameters(M3, effects= "random")
# pM4 <- model_parameters(M4, effects= "random")
 



###Model con Welfare----
 


M1.full1 <- stan_lmer(formula = kgcapo~Welfare+(1|azienda)+(1|Occasion)+
                       LATTAZIONE+
                       hsize+
                       para+
                       caev+
                       malasc,  
                       data = dt,
                       seed = 349)


M2.full <- stan_lmer(formula = kgcapo~WelfA+(1|azienda)+(1|Occasion)+LATTAZIONE+hsize+
                       para+caev+malasc,   
                     data = dt,
                     seed = 349)

M3.full <- stan_lmer(formula = kgcapo~WelfB+(1|azienda)+(1|Occasion)+LATTAZIONE+hsize+
                       para+caev+malasc,   
                     data = dt,
                     seed = 349)

M4.full <- stan_lmer(formula = kgcapo~WelfC+(1|azienda)+(1|Occasion)+LATTAZIONE+hsize+
                       para+caev+malasc,   
                     data = dt,
                     seed = 349)



modelli <- list(M1.full,M2.full,M3.full,M4.full)
saveRDS(modelli, file = "modelli.RDS")

modelli <- readRDS("modelli.RDS")

library(gt)


M1 <- modelli[[1]]


plot(p_direction(M1))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()



tM1 <- describe_posterior(
  M1,
  centrality = "median",
  test = c("rope", "p_direction")
)


tM1 %>% 
  select(Parameter, Median, CI_low, CI_high, pd, ROPE_Percentage, Rhat, ESS) %>%
  mutate_at(2:8, round, 2) %>% 
  gt()# %>% 
 # gtsave("RV.rtf")







###SCC----

m1scc <- stan_lmer(formula = log(scc)~Welfare+(1|azienda)+(1|Occasion)+LATTAZIONE+hsize+
                       para+caev+malasc,   
                     data = dt,
                     seed = 349)












# looM1 <- loo(M1, k_threshold = 0.7)
# looM1.full <- loo(M1.full,k_threshold = 0.7)
# looM1.full1 <- loo(M1.full1,k_threshold = 0.7)
# 
# loo_compare(  looM1, looM1.full1)
# 
# 
# Mwelf <- stan_lmer(formula = Welfare~(1|azienda)+ para,  
#                   data = dt,
#                   seed = 349)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# pM1.1 <- model_parameters(M1.1)
# 
# plot(p_direction(M1.full))+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# tab_model(M1.1)
# 
# library(sjPlot)
#   
# 
# plot_model(M1.1, show.values = TRUE, show.intercept = TRUE)
# 
#  
#   
# # M1.3x <- stan_lmer(formula = kgcapo~Welfare+(1|azienda)+Occasion+Biosic+LATTAZIONE+hsize,   
# #                   data = dt,
# #                   seed = 349)
# # looM1.3x <- loo(M1.3x, k_threshold = 0.7)
# # 
# # 
# # loo_compare(looM1.3, looM1.3x)
# 
# ###Model con WelfA----
# # M2.1 <- stan_lmer(formula = kgcapo~WelfA+(1|azienda)+(1|Occasion)+ Biosic,   
# #                 data = dt,
# #                 # seed = 349)
# 
# M2.1 <- stan_lmer(formula = kgcapo~WelfA+(1|azienda)+(1|Occasion)+Biosic+LATTAZIONE+hsize, 
#                   data = dt,
#                   seed = 349)
# 
# M2.full <- stan_lmer(formula = kgcapo~WelfA+(1|azienda)+(1|Occasion)+Biosic+LATTAZIONE+hsize+
#                        para+caev+agal,   
#                      data = dt,
#                      seed = 349)
# 
# 
# 
# 
# pM2.1 <- model_parameters(M2.1)
# 
#  
# 
# plot(p_direction(M2.full))+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# 
# ###Model con WelfB---
# 
# 
# M3.1 <- stan_lmer(formula = kgcapo~WelfB+(1|azienda)+(1|Occasion)+Biosic+LATTAZIONE+hsize, 
#                   data = dt,
#                   seed = 349)
# 
# 
# 
# M3.full <- stan_lmer(formula = kgcapo~WelfB+(1|azienda)+(1|Occasion)+Biosic+LATTAZIONE+hsize+
#                        para+caev+agal,   
#                      data = dt,
#                      seed = 349)
# 
# 
# pM3.1 <- model_parameters(M3.1)
# 
# plot(p_direction(M3.full))+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# 
# 
# ###Model con WelfC
# 
# M4.1 <- stan_lmer(formula = kgcapo~WelfC+(1|azienda)+(1|Occasion)+Biosic+LATTAZIONE+hsize, 
#                   data = dt,
#                   seed = 349)
# 
# M4.full <- stan_lmer(formula = kgcapo~WelfC+(1|azienda)+(1|Occasion)+Biosic+LATTAZIONE+hsize+
#                        para+caev+agal,   
#                      data = dt,
#                      seed = 349)
# 
# 
# pM4.1 <- model_parameters(M4.1)
# 
# 
# plot(p_direction(M4.full))+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# 
# 
# 
# 
# ###
# Mx <- stan_lmer(formula = Welfare~ (1|azienda)+(1|Occasion)+Biosic,   
#                      data = dt,
#                      seed = 349,
#                      control = list(adapt_delta = 0.99),
#                                     cores = 8)
# 
# 
# 
# 
# #OLDSTUFF-----
# 
# #Modello DAG ----
# ## specificazione del modello: 
# ## outcome: kgcapo
# ## predittore: benessere
# ## altre varibili : stato sanitario, biosicurezza, dimensione allev, tipologia di lattazione ( lunga/ breve)
# #stato sanitario ( prev intrall di paratbc, caev , mastiti, pseudotub
# ##             biosicurezza ( confondente?)
# 
# # library(dagitty)
# # library(ggdag)
# 
# 
# 
# # dag0 <- dagify(Produzione~Benessere+Lattazione+ Herd_Size +ParaT+CAEV+PseudoT,
# #               
# #               
# #           
# #               
# #               exposure =  "Benessere", 
# #               outcome = "Produzione", 
# #               
# #               labels = c("Produzione" = "Produzione", 
# #                          "Benessere" = "Benessere", 
# #                           
# #                          "Lattazione" = "Lattazione", 
# #                          "Biosicurezza"= "Biosicurezza",
# #                          "Herd_Size"= "Herd Size", 
# #                          "ParaT" = "ParaT", 
# #                          "CAEV" = "CAEV", 
# #                          "PseudoT" = "PseudoT"
# #               )
# # )
# # 
# # ggdag(dag0, text_col = "blue",  node_size = 1)+
# #   theme_dag_grid()
# # 
# # 
# # 
# # dag1 <- dagify(Produzione~Benessere+Lattazione+ Herd_Size + Biosicurezza,
# #                
# #                
# #                Biosicurezza~Herd_Size, 
# #                Benessere ~ParaT , 
# #                Benessere ~ CAEV , 
# #                Benessere ~ Agalassia,
# #                Benessere ~ Biosicurezza,
# #                
# #                exposure =  "Benessere", 
# #                outcome = "Produzione", 
# #                
# #                labels = c("Produzione" = "Produzione", 
# #                           "Benessere" = "Benessere", 
# #                           
# #                           "Lattazione" = "Lattazione", 
# #                           "Biosicurezza"= "Biosicurezza",
# #                           "Herd_Size"= "Herd Size", 
# #                           "ParaT" = "ParaT", 
# #                           "CAEV" = "CAEV", 
# #                           "Agalassia" = "Agalassia"
# #                )
# # )
# # 
# # ggdag(dag1, text_col = "blue",  node_size = 1)+
# #   theme_dag_grid()
# #  
# # 
# # 
# # dag %>% dseparated("Biosicurezza",  "Produzione")
# # 
# # 
# # adjustmentSets(dag)
# # 
# # ggdag(dag, text_col = "blue",  node_size = 1)+
# #   theme_dag_grid()
# # 
# # ggdag_exogenous(dag0)
# # ggdag_paths(dag, text = FALSE, use_labels = "label", shadow = TRUE)+
# #   theme_dag_grid()
# # 
# # 
# # ggdag_adjustment_set(dag, text_col = "blue")+  theme_dag_grid()
# # 
# # ggdag_dseparated(dag1, controlling_for = c("ParaT", "CAEV", ""), 
# #                  text_col = "blue", collider_lines = FALSE)+theme_dag_grid()
# # 
# # 
# # 
# # dag <- dagify(Stato_Sanitario~Biosicurezza,
# #               Produzione ~ Stato_Sanitario+Benessere
# #               )
# 
# 
# ####grafico
# df %>% 
#   mutate(azienda=casefold(azienda, upper = TRUE)) %>% 
#   drop_na(kgcapo) %>% 
#   ggplot(aes(x=Occasion, y = kgcapo))+  
#   #facet_wrap(bencat~., nrow = 1) + 
#   stat_smooth()+
#   geom_line(aes(x=Occasion, y = kgcapo, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
#   theme_ipsum_rc()
# 
# # M0 <- brm(kgcapo~1,
# #           data = df, family = gaussian, 
# #           iter = 8000, cores = 8, seed = 1966)
# # 
# # M0_stanlmer <- stan_glm(formula = kgcapo~1,   
# #                          data = df,
# #                          seed = 349)
# # df %>% 
# #   group_by(azienda) %>% 
# # summarise(media = mean(kgcapo, na.rm = T), 
# #           sd = sd(kgcapo, na.rm = T)) %>% 
# # summarise(Media = mean(media), 
# #           SD = sd(sd, na.rm = T))
# # 
# # 
# # M1 <- brm(kgcapo~(1|azienda),
# #           data = df, family = gaussian, 
# #           iter = 8000, cores = 8, seed = 1966)
# # 
# #  
# # 
# # M1_stanlmer <- stan_lmer(formula = kgcapo~0+as.factor(azienda)+(1|azienda),   
# #                         data = df,
# #                         seed = 349)
# # 
# # M1_stanlmer <- stan_glm(formula = kgcapo~ 0+Azienda,
# #                          data = df,
# #                          seed = 349)
# # df %>% 
# #   group_by(Azienda) %>% 
# #   summarise(m = mean(kgcapo, na.rm = T), 
# #             sd = sd(kgcapo, na.rm = T)) %>% 
# #   left_join(
# #     (df %>% 
# #        select(Azienda, Welfare)), by = c("Azienda")   
# #       
# #   ) %>% 
# #   unique()
# # 
# # plot(p_direction(M1_stanlmer))+
# #   labs(title= "", y = "Aziende", x = "produzione latte (kgcapo)")+
# #   theme_light()+
# #   theme(legend.position = "none")+ scale_fill_brewer(palette="Blues")
# # 
# # post <- as.data.frame(M1_stanlmer)
# # 
# # M2 <- brm(kgcapo~ (1|azienda)+(1|Occasion),
# #           data = df, family = gaussian, 
# #           iter = 8000, cores = 8, seed = 1966)
# # 
# # 
# # M2_stanlmer <- stan_lmer(formula = kgcapo~ 0 + (1|azienda)+(1|Occasion),   
# #                          data = df,
# #                          seed = 349)
# # 
# # 
# # # sims <- as.matrix(M2_stanlmer)
# # # 
# # # 
# # # 
# # # mu_a_sims <- as.matrix(M2_stanlmer, 
# # #                        pars = "(Intercept)")
# # # u_sims <- as.matrix(M2_stanlmer, 
# # #                     regex_pars = "b\\[\\(Intercept\\) azienda\\:")
# # # uocc_sims <- as.matrix(M2_stanlmer, 
# # #                     regex_pars = "b\\[\\(Intercept\\) Occasion\\:")
# # 
# #  
# # 
# # 
# # 
# # # M3 <- brm(kgcapo~ Occasion+(1|azienda) ,
# # #           data = df, family = gaussian, 
# # #           iter = 8000, cores = 8, seed = 1966)
# # #  
# # # M4 <- brm(kgcapo~ Occasion+(1+Occasion|azienda) ,
# # #           data = df, family = gaussian, 
# # #           iter = 8000, cores = 8, seed = 1966)
# # # 
# # # M5 <- brm(kgcapo~(1+Occasion|azienda) ,
# # #           data = df, family = gaussian, 
# # #           iter = 8000, cores = 8, seed = 1966)
# # # 
# # # M6 <- brm(kgcapo~ Welfare+(1+Occasion|azienda) ,
# # #           data = df, family = gaussian, 
# # #           iter = 8000, cores = 8, seed = 1966)
# # 
# # M7 <- brm(kgcapo~ Welfare+(1|Occasion)+(1|azienda) ,
# #           data = df, family = gaussian, 
# #           iter = 8000, cores = 8, seed = 1966)
# 
# 
# #M7_stanlmer <- stan_lmer(formula = kgcapo~Welfare+(1|azienda)+(1|Occasion),   
#                          data = df,
#                          seed = 349)
# 
# ### usare la variabiel occasion come variabile random...sotto il confronto tra i modelli con 
# ##due utilizzi diversi della variabile è a favore dell'uso random....
# # loo1<- loo(M7_stanlmer,k_threshold = 0.7)
# # 
# # M7a_stanlmer <- stan_lmer(formula = kgcapo~Welfare+(1|azienda)+ Occasion,   
# #                          data = df,
# #                          seed = 349 )
# # 
# # loo2<- loo(M7a_stanlmer,k_threshold = 0.7)
# # 
# # loo_compare(loo1, loo2)
# 
# 
# # M8 <- brm(kgcapo~ Welfare+ Occasion+(1|azienda) ,
# #           data = df, family = gaussian, 
# #           iter = 8000, cores = 8, seed = 1966)
# 
# loo(M1_stanlmer,M2_stanlmer,  moment_match = TRUE)
# 
# kfm <- kfold(M1_stanlmer, K=10)
# kfm2 <- kfold(M2_stanlmer, K=10)
# 
# kf <- loo_compare(kfm, kfm2)
# pp_check(model)
# pp_check(mod)
# 
# 
# 
# 
# 
# az <- sample(unique(df$azienda), 5)
# 
# 
# df %>% na.omit() %>% 
#   mutate(Well = factor(ntile(Welfare, 4))) %>%  
#   bind_cols(as_tibble(fitted(M7))) %>% 
#   #filter(azienda %in% az) %>%   
#   ggplot()+
#   geom_line(aes(x = Occasion, y = kgcapo, group = azienda), size = 1, alpha = .75, color = "dodgerblue2")+
#   geom_line(aes(x = Occasion, y = Estimate, group = azienda), shape = 1, size = 1, stoke =1.5)+
#   facet_wrap(~Well, labeller = as_labeller(c('1' = "Insuff", 
#                                                 '2' = "Basso", 
#                                                 '3' = "Medio", 
#                                                 '4' = "Alto")), nrow = 1)+
#   stat_smooth(aes(x = Occasion, y = Estimate))
# 
# library(patchwork)
# 
# df %>% 
#   ggplot(aes(x = Time, y =kgcapo, group = azienda))+
#   geom_line(size = .75, alpha = .20)
# 
# 
# 
# model <- brm(kgcapo ~  Welfare+hsize+Time+LATTAZIONE+Biosic+para+caev+agal+(1|azienda), 
#          data = df, family = gaussian, 
#          iter = 8000, cores = getOption("mc.cores", 8))
# 
# 
# pd <- p_direction(model, parameters = c( "Welfare", "Biosic", "hsize", "LATTAZIONE",  "para", "caev", "agal"))
# 
# pd <- p_direction(model)
# 
# 
# plot(pd)+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# 
# 
# mod.W <- brm(Welfare ~ Time+  Biosic+(1|azienda), 
#             data = df, family = gaussian)
# 
# plot(pd(mod.W))
# 
# 
# 
# 
# 
# 
# 
# 
# # mod_score <- brm(kgcapo ~ Time+score+(1|azienda), 
# #            data = df, family = gaussian)
# # 
# # pd <- p_direction(mod_score, parameters = "score")
# # plot(pd)+scale_fill_brewer(palette="Blues")+
# #   theme_ipsum_rc()
# # 
# # mod_score1 <- brm(kgcapo ~ Time+score+ Biosic+(1|azienda), 
# #                  data = df, family = gaussian)
# # 
# # pd <- p_direction(mod_score1, parameters = c("score", "Biosic"))
# # plot(pd)+scale_fill_brewer(palette="Blues")+
# #   theme_ipsum_rc()
# 
# 
# 
# 
# -------------------------------------------------------------------------------
# #Old Stuff----------
# 
# # wel <- ben %>% 
# #   left_join(welfscore, by= "azienda") %>% 
# #   mutate(bencat = cut(complben, quantile(complben), include.lowest = TRUE), 
# #          scorecat = cut(score, quantile(score), include.lowest = T))
# # 
# # 
# # 
# # table(wel$bencat, wel$scorecat)
# 
# 
# # prod_latte <- az %>% 
# #   mutate(azienda=casefold(azienda, upper = TRUE),
# #          mese=recode(mese,
# #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# #                      ottobre=10, novembre=11,dicembre=12), 
# #          time=as.Date(paste(anno, mese, 15, sep="-")), 
# #          prelievo = anno+mese) %>%  
# #   drop_na(kgcapo) %>% 
# #   left_join(
# #     (welfscore %>% 
# #         select(azienda, score) %>% 
# #        mutate(
# #          # bencat = cut(score, quantile(score), include.lowest = T), 
# #          #      randomvalue = rnorm(nrow(.), 0,1),
# #          #      rcat = cut(randomvalue, quantile(randomvalue), include.lowest = T), 
# #        azienda=casefold(azienda, upper = TRUE))), by='azienda')
# # 
# # 
# # 
# # 
# # 
# # 
# #   # ggplot(aes(x=time, y = kgcapo))+
# #   # facet_wrap(rcat~., nrow = 1) + stat_smooth()+
# #   # geom_line(aes(x=time, y = kgcapo, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# #   # theme_ipsum_rc()
# # 
# #  
# # ###esamino l'andamento nel tempo della variabile kgcapo delle singole aziende
# #   prod_latte %>% 
# #   select(azienda, time, kgcapo, score, caprelatt, bencat) %>% 
# #   group_by(azienda) %>% 
# #   mutate(inmilk = mean(caprelatt), 
# #          N= n()) %>% 
# #   select(-caprelatt) %>% 
# #   filter(N >= 10) %>% 
# #   arrange(time) %>% 
# #   #pivot_wider(names_from = time, values_from = kgcapo) %>% 
# #   ggplot(aes(x = time, y = kgcapo)) +
# #   geom_point() + geom_line()+
# #   #coord_cartesian(ylim = c(1, 6)) +
# #     stat_smooth(method = "loess", se = F, span = .9)+
# #   theme(panel.grid = element_blank()) +
# #   facet_wrap(~azienda)
# #     
# # ###esamino l'intero set delle traiettore smoothed
# #   
# #   prod_latte %>% 
# #     select(azienda, time, kgcapo, score, caprelatt, bencat) %>% 
# #     group_by(azienda) %>% 
# #     mutate(inmilk = mean(caprelatt), 
# #            N= n()) %>% 
# #     select(-caprelatt) %>% 
# #     filter(N >= 10) %>% 
# #     arrange(time) %>% 
# #     ggplot(aes(x = time, y = kgcapo)) +
# #     stat_smooth(method = "lm", se = F, span = .9, size = 2) +
# #     stat_smooth(aes(group = azienda),
# #                 method = "lm", se = F, span = .9, size = 1/4)
# #     
# #     
# # ###Smooting the empirical trajectory using single-level Bayesian regression
# #   
# #   by_az <- 
# #     prod_latte %>% 
# #     group_by(azienda) %>% 
# #     nest()
# # 
# # library(brms)
# # 
# #   dati <- by_az$data[[1]]
# #   fit2.1 <-
# #     brm(data = dati, 
# #         formula = kgcapo ~ 1 + prelievo,
# #         prior = prior(normal(0, 2), class = b),
# #         iter = 4000, chains = 4, cores = 4,
# #         seed = 2)
# # 
# # 
# # 
# # prod_latte %>%
# #   ggplot(aes(x = time, y = kgcapo)) +
# #   geom_point() + geom_line()+
# #   coord_cartesian(ylim = c(1, 4)) +
# #   theme(panel.grid = element_blank()) +
# #   facet_wrap(~azienda)
# # 
# # 
# # #uso ben##
# # # az %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(kgcapo) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = kgcapo))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = kgcapo, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # 
# # 
# # ##milk
# # ### uso welfscore
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(scc) %>% 
# # #   left_join(
# # #     (welfscore %>% 
# # #        dplyr::select(azienda, score) %>% 
# # #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# # #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# # #   ggplot(aes(x=time, y = log(scc)))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time, y = log(scc), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # 
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(proteine) %>% 
# # #   left_join(
# # #     (welfscore %>% 
# # #        dplyr::select(azienda, score) %>% 
# # #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# # #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# # #   ggplot(aes(x=time, y = proteine))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time, y = proteine, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(grasso) %>% 
# # #   left_join(
# # #     (welfscore %>% 
# # #        dplyr::select(azienda, score) %>% 
# # #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# # #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# # #   ggplot(aes(x=time, y = grasso))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time, y = grasso, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # #  
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(lattosio) %>% 
# # #   left_join(
# # #     (welfscore %>% 
# # #        dplyr::select(azienda, score) %>% 
# # #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# # #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# # #   ggplot(aes(x=time, y = lattosio))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time, y = lattosio, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(caseina) %>% 
# # #   left_join(
# # #     (welfscore %>% 
# # #        dplyr::select(azienda, score) %>% 
# # #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# # #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% 
# # #   ggplot(aes(x=time, y = caseina))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time, y = caseina, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>%
# # #   drop_na(cbt) %>% 
# # #   left_join(
# # #     (welfscore %>% 
# # #        dplyr::select(azienda, score) %>% 
# # #        mutate(bencat = cut(score, quantile(score), include.lowest = T),
# # #               azienda=casefold(azienda, upper = TRUE))), by='azienda') %>% glimpse()
# # #   ggplot(aes(x=time, y = log(cbt)))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time, y = log(cbt), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = log(scc)))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = log(scc), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = proteine))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = proteine, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = grasso))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = grasso, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # 
# # # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = lattosio))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = lattosio, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # 
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = caseina))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = caseina, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = log(cbt)))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = log(cbt), group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # 
# # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = ureaFTIR))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = ureaFTIR, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # 
# # # milk %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = ureapHm))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth()+
# # #   geom_line(aes(x=time.x, y = ureapHm, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # sanit %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = `paratbc(%)`))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth(se = FALSE)+
# # #   geom_line(aes(x=time.x, y =`paratbc(%)`, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # sanit %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #   left_join( (ben %>% 
# # #                 mutate(azienda=casefold(azienda, upper = TRUE),
# # #                        mese=recode(mese,
# # #                                    gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                                    maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                                    ottobre=10, novembre=11,dicembre=12), 
# # #                        time=as.Date(paste(anno, mese, 15, sep="-"))) %>% 
# # #                 filter(anno != 2020 ) %>% 
# # #                 mutate(bencat = cut(complben, quantile(complben), include.lowest = F))
# # #   ), by = "azienda") %>% 
# # #   drop_na(bencat) %>% 
# # #   ggplot(aes(x=time.x, y = `caev(%)`))+  
# # #   facet_wrap(bencat~., nrow = 1) + stat_smooth(se = FALSE)+
# # #   geom_line(aes(x=time.x, y =`caev(%)`, group = azienda), alpha=0.3) + geom_point(alpha = 0.3)+
# # #   theme_ipsum_rc()
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # d2 <-read_sheet(id$id, sheet ="massa" ) %>% 
# # #   mutate(azienda=casefold(azienda, upper = TRUE),
# # #          mese=recode(mese,
# # #                      gennaio=1,febbraio=2,marzo=3,aprile=4,
# # #                      maggio=5, giugno=6, luglio=7, agosto=8, settembre=9,
# # #                      ottobre=10, novembre=11,dicembre=12), 
# # #          time=as.Date(paste(anno, mese, 15, sep="-")))
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(scc) %>% 
# # #   ggplot(aes(x=time, y=scc, group=azienda))+geom_line()+
# # #   scale_y_log10()
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(proteine) %>% 
# # #   ggplot(aes(x=time, y=proteine, group=azienda))+geom_line()
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(caseina) %>% 
# # #   ggplot(aes(x=time, y=caseina, group=azienda))+geom_line()
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(grasso) %>% 
# # #   ggplot(aes(x=time, y=grasso, group=azienda))+geom_line()
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(lattosio) %>% 
# # #   ggplot(aes(x=time, y=lattosio, group=azienda))+geom_line()
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(cbt) %>% 
# # #   ggplot(aes(x=time, y=cbt, group=azienda))+geom_line()+scale_y_log10()
# # # 
# # # d2 %>% 
# # #   group_by(azienda, time) %>% 
# # #   drop_na(ureaFTIR) %>% 
# # #   ggplot(aes(x=time, y=ureaFTIR, group=azienda))+geom_line()+scale_y_log10()+
# # #  
# # # 
# # # 
# # # 
# # # d3 <-read_sheet(id$id, sheet ="san" )
# # # d4 <-read_sheet(id$id, sheet ="par" )
# # # d5 <-read_sheet(id$id, sheet ="diagn" )
# # # 
# # # d6 <-read_sheet(id$id, sheet ="ben" )
# # # 
# # # 
# # # library(GGally)
# # # ggpairs(d6[,4:9])+ theme_bw()
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # library(GGally)
# # # ggpairs(d6[,4:9])+ theme_bw()
# # # 
# # # summary(d6$complben)
# # # sd(d6$complben)^2
# # # 
# # # summary(d6$areaA)
# # # sd(d6$areaA)^2
# # # 
# # # summary(d6$areaB)
# # # sd(d6$areaB)^2
# # # 
# # # summary(d6$gr)
# # # sd(d6$gr)^2
# # # 
# # # 
# # # d4 %>% 
# # #     filter(azienda=="039BG069") %>% 
# # #     group_by(mese, cat) %>% 
# # #     summarise(strongili=mean(`strongiliGE (upg)`, na.rm=T))%>% 
# # #     ggplot(aes(x=mese, y=strongili, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)
# # # 
# # # 
# # # d4 %>% 
# # #   filter(azienda=="039BG069") %>% 
# # #   group_by(mese, cat) %>% 
# # #   summarise(coccidi=mean(`coccidi (upg)`, na.rm=T))%>% 
# # #   ggplot(aes(x=mese, y=coccidi, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)
# # # 
# # # 
# # # 
# # # 
# # # ####SCC grafico
# # # 
# # # d2 %>% 
# # #   group_by(azienda,mese) %>% 
# # #   summarise(scc=geometric.mean(scc, na.rm=T))%>% 
# # #   ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # #   
# # # 
# # # 
# # # 
# # # d2 %>% 
# # #   group_by(azienda,mese) %>% 
# # #   summarise(cbt=geometric.mean(cbt, na.rm=T))%>% 
# # #   ggplot(aes(x=mese, y=cbt, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # # 
# # # 
# # # 
# # # d2 %>% 
# # #   group_by(azienda,mese) %>% 
# # #   summarise(proteine=mean(proteine, na.rm=T))%>% 
# # #   ggplot(aes(x=mese, y=proteine, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # # 
# # # 
# # # d2 %>% 
# # #   group_by(azienda,mese) %>% 
# # #   summarise(grasso=mean(grasso, na.rm=T))%>% 
# # #   ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # # 
# # # 
# # # d2 %>% 
# # #   group_by(azienda,mese) %>% 
# # #   summarise(grasso=mean(grasso, na.rm=T))%>% 
# # #   ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # # dt<-d1 %>% 
# # # #  full_join(d6,  by=c("mese","azienda")) %>% 
# # # #   full_join(d2, by=c("mese","azienda")) %>% 
# # # #    full_join(d3,by=c("mese","azienda")) 
# # # 
# # # 
# # # 
# # # # d2$controllo<-ifelse(d2$mese=="gennaio",1,
# # # #                      ifelse(d2$mese=="febbraio", 2,
# # # #                             ifelse(d2$mese=="marzo", 3,
# # # #                                    ifelse(d2$mese=="aprile",4,
# # # #                                           ifelse(d2$mese=="maggio",5,
# # # #                                                  ifelse(d2$mese=="giugno",6,
# # # #                                                         ifelse(d2$mese=="luglio",7,
# # # #                                                            ifelse(d2$mese=="agosto",8,
# # # #                                                                       ifelse(d2$mese=="settembre",9,
# # # #                                                                              ifelse(d2$mese=="ottobre",10,
# # # #                                                                                     ifelse(d2$mese=="novembre",11,
# # # #                                                                                            ifelse(d2$mese=="dicembre",12,d2$mese))))))))))))
# # # # d2$controllo<-as.numeric(d2$controllo)  
# # # 
# # # #####scc#
# # # d2w<-d2 %>% 
# # #   drop_na(azienda) %>% 
# # #   arrange(mese) %>% 
# # #   select(azienda,mese,scc) %>% 
# # #   group_by(azienda,mese) %>% 
# # #   summarise(scc=mean(scc)) %>% 
# # #   # mutate(id=row_number()) %>% 
# # #   # group_by(azienda,controllo) %>% 
# # #   #arrange(controllo) %>% 
# # #   pivot_wider(names_from=mese,values_from=scc ,values_fn = list(scc = mean))
# # #  
# # #   #select(-11)
# # 
# # # 
# # # 
# # # 
# # # 
# # # dwj<-d1 %>% 
# # #   select(-anno) %>% 
# # #   left_join(d2w,by=c("azienda")) %>% 
# # #   left_join(d6[,-c(1:2)],by=c("azienda")) %>% 
# # #   pivot_longer(13:22,
# # #   names_to = "month", values_to = "scc") %>% 
# # #   mutate(month=as.numeric(month)) %>% 
# # #   group_by(azienda, month) %>% 
# # #   summarise(scc=geometric.mean(scc, na.rm=T)) %>% 
# # #   ggplot(aes(x=month, y=scc))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # #   
# # # ###CBT###
# # # d2w<-d2 %>% 
# # #   select(azienda,cbt) %>% 
# # #   group_by(azienda,controllo) %>% 
# # #   summarise(cbt=mean(cbt)) %>% 
# # #   # mutate(id=row_number()) %>% 
# # #   # group_by(azienda,controllo) %>% 
# # #   #arrange(controllo) %>% 
# # #   pivot_wider(names_from=controllo,values_from=cbt ,values_fn = list(cbt = mean)) %>% 
# # #   select(-11)
# # # 
# # # 
# # # dwj<-d1 %>% 
# # #   select(-mese) %>% 
# # #   left_join(d2w,by=c("azienda")) %>% 
# # #   left_join(d6[,-1],by=c("azienda")) %>% 
# # #   pivot_longer(12:20,
# # #                names_to = "mese", values_to = "cbt") %>% 
# # #   mutate(mese=as.numeric(mese)) %>% 
# # #   group_by(azienda, mese) %>% 
# # #   summarise(proteine=geometric.mean(cbt, na.rm=T)) %>% 
# # #   ggplot(aes(x=mese, y=proteine))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # # 
# # # ###Proteine
# # # d2w<-d2 %>% 
# # # select(azienda,controllo,proteine) %>% 
# # #   group_by(azienda,controllo) %>% 
# # #   summarise(proteine=mean(proteine)) %>% 
# # #   # mutate(id=row_number()) %>% 
# # #   # group_by(azienda,controllo) %>% 
# # #   #arrange(controllo) %>% 
# # #   pivot_wider(names_from=controllo,values_from=proteine ,values_fn = list(proteine = mean)) %>% 
# # #   select(-11)
# # # 
# # # 
# # # dwj<-d1 %>% 
# # #   select(-mese) %>% 
# # #   left_join(d2w,by=c("azienda")) %>% 
# # #   left_join(d6[,-1],by=c("azienda")) %>% 
# # #   pivot_longer(12:20,
# # #                names_to = "mese", values_to = "proteine") %>% 
# # #   mutate(mese=as.numeric(mese)) %>% 
# # #   group_by(azienda, mese) %>% 
# # #   summarise(proteine=mean(proteine, na.rm=T)) %>% 
# # #   ggplot(aes(x=mese, y=proteine))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # # 
# # # ####Grasso
# # # d2w<-d2 %>% 
# # #   select(azienda,controllo,grasso) %>% 
# # #   group_by(azienda,controllo) %>% 
# # #   summarise(grasso=mean(grasso)) %>% 
# # #   # mutate(id=row_number()) %>% 
# # #   # group_by(azienda,controllo) %>% 
# # #   #arrange(controllo) %>% 
# # #   pivot_wider(names_from=controllo,values_from=grasso ,values_fn = list(grasso = mean)) %>% 
# # #   select(-11)
# # # 
# # # 
# # # dwj<-d1 %>% 
# # #   select(-mese) %>% 
# # #   left_join(d2w,by=c("azienda")) %>% 
# # #   left_join(d6[,-1],by=c("azienda")) %>% 
# # #   pivot_longer(12:20,
# # #                names_to = "mese", values_to = "grasso") %>% 
# # #   mutate(mese=as.numeric(mese)) %>% 
# # #   group_by(azienda, mese) %>% 
# # #   summarise(grasso=mean(grasso, na.rm=T)) %>% 
# # #   ggplot(aes(x=mese, y=grasso))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # # 
# # # ###Lattosio
# # # d2w<-d2 %>% 
# # #   select(azienda,controllo,lattosio) %>% 
# # #   group_by(azienda,controllo) %>% 
# # #   summarise(lattosio=mean(lattosio)) %>% 
# # #   # mutate(id=row_number()) %>% 
# # #   # group_by(azienda,controllo) %>% 
# # #   #arrange(controllo) %>% 
# # #   pivot_wider(names_from=controllo,values_from=lattosio ,values_fn = list(lattosio = mean)) %>% 
# # #   select(-11)
# # # 
# # # 
# # # dwj<-d1 %>% 
# # #   select(-mese) %>% 
# # #   left_join(d2w,by=c("azienda")) %>% 
# # #   left_join(d6[,-1],by=c("azienda")) %>% 
# # #   pivot_longer(12:20,
# # #                names_to = "mese", values_to = "lattosio") %>% 
# # #   mutate(mese=as.numeric(mese)) %>% 
# # #   group_by(azienda, mese) %>% 
# # #   summarise(lattosio=mean(lattosio, na.rm=T)) %>% 
# # #   ggplot(aes(x=mese, y=lattosio))+geom_point()+geom_smooth()+facet_wrap(~azienda)
# # 
# # # 
# # # x<-d1 %>% 
# # #   select(-mese) %>% 
# # #   left_join(d2w,by=c("azienda")) %>% 
# # #   left_join(d6[,-1],by=c("azienda")) 
