library("tidyverse")
library("lubridate")
library("here")
library("hrbrthemes")
library("readxl")
library("eRm")
library("ltm")
library("difR")
options(scipen = 999)

dati <- read_excel("analisi/data/raw/Dataset.xlsx", 
                      sheet = "gruppoITEM-A")
dati <- dati %>% 
  filter(anno==2019)

