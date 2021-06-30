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
