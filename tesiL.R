library(tidyverse)
library(here)
library(binom)
library(readxl)
library(tidybayes)
library(ggridges)


dt <- read_excel("analisi/data/raw/TabellaTesiLivia.xlsx")

library(rstanarm)

mod <- stan_glm(cbind(positivi, (negativi+dubbi)) ~ codice, 
         data = dt,
         family = binomial(link = "logit"),
         prior = student_t(df = 7, 0, 5),
         prior_intercept = student_t(df = 7, 0, 5),
         cores = 2, seed = 12345)



library(emmeans)

t<-emmeans(mod, ~codice)

t %>% as.data.frame() %>%  
  mutate(emmean=invlogit(emmean),
             lower.HPD = invlogit(lower.HPD),
             upper.HPD = invlogit(upper.HPD)) %>%
  select("Azienda" = codice, "Prevalenza"=emmean, "HPD-inf"= lower.HPD, "HPD-sup"= upper.HPD ) %>%
  arrange(desc(Prevalenza))  
  
  
  
p<-gather_emmeans_draws(t)

p %>%
mutate("prev"=invlogit(.value))%>% 
  group_by(codice) %>%
  # summarise(m=mean(prev),
  #           sd=sd(prev)) %>%
  ggplot(aes(x = prev, y=codice)) +
  geom_density_ridges(panel_scaling=TRUE)+
  theme_ridges()+
  # scale_fill_brewer(palette = 7) +
  theme_ridges() + theme(legend.position = "NULL")+labs(x="Prevalenza",y="")

