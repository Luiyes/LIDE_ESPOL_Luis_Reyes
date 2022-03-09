library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(car)
library(boot)
library(QuantPsyc)
library(ggplot2)
data5 <- read.csv("D:\\carpeta de luis\\documentos  maquina de luis andres\\pasantias\\educacion\\Base_Match_dic18_dic19.csv",sep=";")
salariomenor <- min(data5["p66_dic18"],na.rm = TRUE)
salariomayor <- max(data5["p66_dic18"],na.rm = TRUE)
salariomayor
salariomenor
Ingreso_bajo2018<- data5 %>%
  filter(p66_dic18<386)
ggplot(Ingreso_bajo2018, aes(x=p66_dic18,y=p45_dic18))+
  geom_point()
Ingreso_medio2018<- data5 %>%
  filter(p66_dic18>386 & p66_dic18<600)
Ingreso_medio2018
ggplot(Ingreso_medio2018, aes(x=p66_dic18,y=p45_dic18))+
  geom_point()
modelo <- lm(p66_dic18 ~ p45_dic18, data=data5,na.action = na.exclude)
summary(modelo)