library(tidyverse)
library(here)
library(binom)
library(readxl)
library(tidybayes)
library(ggridges)
library(viridis)
library(hrbrthemes)

dt <- read_excel("analisi/data/raw/TabellaTesiLivia.xlsx")


library(rstanarm)

mod <- stan_glm(cbind(positivi, (negativi+dubbi)) ~ codice, 
         data = dt,
         family = binomial(link = "logit"))



library(emmeans)
# library(knitr)
# library(kableExtra)
library(xtable)

t<-emmeans(mod, ~codice)

tt <- t %>% as.data.frame() %>%  
  mutate(emmean=round(invlogit(emmean),2),
             lower.HPD = round(invlogit(lower.HPD),2),
             upper.HPD = round(invlogit(upper.HPD),2)) %>%
  select("Azienda" = codice, "Prevalenza"=emmean, "HPD-inf"= lower.HPD, "HPD-sup"= upper.HPD ) %>%
  arrange(desc(Prevalenza))   
   
  print(xtable(tt), type = "html", file = "tt.html")
# kbl() %>% 
  # kable_styling()
  
  
  
p<-gather_emmeans_draws(t) %>% 
  left_join(
    (dt %>% 
       select(codice, tipologia, razza, positivi)), by = "codice"
  )

library(ggdist)
p %>%
  mutate(stato = ifelse(positivi == 0, "Allevamenti Negativi", "Allevamenti Positivi")) %>% 
  filter(stato == "Allevamenti Positivi") %>% 
  #mutate(provincia = substr(codice, start = 4, stop = 5)) %>%   
 mutate("prev"=invlogit(.value))%>%   
  group_by(codice ) %>%

 #  summarise(m=mean(prev),
 #            sd=sd(prev)) %>%
 # ggplot(aes(x = codice, dist="norm", arg1 = m, arg2 = sd, fill = codice ))+
 
 
  ggplot(aes(x = prev, y = fct_reorder(codice, prev), fill = razza)) +
  stat_halfeye(.width = c(0.66, 0.95) ) +
  #geom_density_ridges(panel_scaling=TRUE)+
  # geom_density_ridges_gradient(scale = 10, alpha=0.5)+
  #stat_gradientinterval(position ="dodge", fill_type = "gradient"
  scale_fill_brewer()+
  theme_ipsum() +
  labs(x = "prevalenza", y ="Azienda")+
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16), 
        axis.title.y = element_text(size=16), 
        axis.title.x = element_text(size=16))
  
  
 
  
  



  