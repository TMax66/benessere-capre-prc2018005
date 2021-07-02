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
library(knitr)
library(kableExtra)

t<-emmeans(mod, ~codice)

t %>% as.data.frame() %>%  
  mutate(emmean=invlogit(emmean),
             lower.HPD = invlogit(lower.HPD),
             upper.HPD = invlogit(upper.HPD)) %>%
  select("Azienda" = codice, "Prevalenza"=emmean, "HPD-inf"= lower.HPD, "HPD-sup"= upper.HPD ) %>%
  arrange(desc(Prevalenza))  %>% 
  kbl() %>% 
  kable_styling()
  
  
  
p<-gather_emmeans_draws(t) %>% 
  left_join(
    (dt %>% 
       select(codice, tipologia, razza)), by = "codice"
  )

p %>%
mutate("prev"=invlogit(.value))%>%  
  group_by(codice) %>%
 # filter(tipologia == "semi intensivo") %>% 
  # summarise(m=mean(prev),
  #           sd=sd(prev)) 
  ggplot(aes(x = prev, y=codice, fill = codice)) +
  #geom_density_ridges(panel_scaling=TRUE)+
  geom_density_ridges_gradient(scale = 10, alpha=0.5)+
  
  theme_ipsum() +
  theme(
    legend.position="none",
    
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) 


  