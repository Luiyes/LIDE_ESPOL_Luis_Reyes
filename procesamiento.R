if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(boot)) install.packages("boot", repos = "http://cran.us.r-project.org")
if(!require(QuantPsyc)) install.packages("QuantPsyc", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(car)
library(boot)
library(QuantPsyc)
library(ggplot2)
# google drive
# download url (warning: set to "anyone with the link")
#id <- "1gytmcEX4pFZrgJme9l5pZ7Ko5mJhfVUz"
#url <- paste("https://drive.google.com/uc?export=download&id=", id, sep = "")

# github
# url <- "https://github.com/aquijanoruiz/elquantificador_posts/raw/master/databases/salud/BDD_ENSANUT_2018_STATA_.zip"
##url <- "https://github.com/Luiyes/LIDE_ESPOL_Luis_Reyes/blob/4b07ba13106ddc1eaa73ca1a23be7d5682f4dc2e/ENEMDU-Jan22.zip"
##url

# creates a temporary directory
##td <- tempdir()
# creates a placeholder file
##tf <- tempfile(tmpdir = td, fileext = ".zip")
# downloads the data into the placeholder file
##download.file(url = url, destfile = tf)
##tf
# extracts the files into the temporary directory
##unzip(tf, exdir = td, overwrite = TRUE)
# file path containing the datasets
##fp <- file.path(td, list.files(path = td))
##fp
##library(foreign)
# spss
##data <- read.spss(fp)
# convertir la lista en dataframe
##data5 <- as.data.frame(data)

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

#Modelo de regresion lineal simple
modelo <- lm(p66_dic18 ~ p45_dic18, data=data5,na.action = na.exclude)
summary(modelo)
help(mean)
help(sd)
data6 <- data5 %>%
  mutate(z_scores=(abs (p66_dic18-mean (p66_dic18,na.rm = TRUE)) / sd (p66_dic18,na.rm = TRUE)))
no_outliers_ingreso <- data6 %>%
  filter(z_scores<3)
no_outliers_ingreso
modelo2 <- lm(p66_dic18 ~ p45_dic18, data=no_outliers_ingreso,na.action = na.exclude)
summary(modelo2)
ggplot(no_outliers_ingreso, aes(x=p66_dic18,y=p45_dic18))+
  geom_point()