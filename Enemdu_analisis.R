#Carga de paquetes
if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(boot)) install.packages("boot", repos = "http://cran.us.r-project.org")
if(!require(QuantPsyc)) install.packages("QuantPsyc", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(haven)
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(car)
library(boot)
library(QuantPsyc)
library(ggplot2)

#Carga de data 2021.
id<-"1ZT01QkrRp3RbuuaRG-ZIXA7Yxgodw-w3"
url <- paste("https://drive.google.com/uc?export=download&id=", id, sep = "")


# creates a tempory directory
td <- tempdir()

# creates a placeholder file
tf <- tempfile(tmpdir = td, fileext = ".zip")

# downloads the data into the placeholder file - warning mode = "wb" for windowns
download.file(url = url, destfile = tf, mode = "wb")

# extracts the files into the tempory directory
unzip(tf, exdir = td, overwrite = T)

# file path containing the datasets
fp <- file.path(td, list.files(path = td)[2])

# to verify that the file was downloaded, use: list.files(path = td)

# spss
library(foreign)
Enemdu2021p <- read.spss(fp) 
Enemdu2021p <- as.data.frame(Enemdu2021p)

#Revision de variables a tratar

#Cantidad de personas con estudios de 4to nivel y un empleo pleno
cuarto_niv_empleo <- Enemdu2021p %>% filter(p10a==10) %>% filter(condact==1) %>% count(p10a)
#variable usada en articulo
varcuarto_niv_empleo <- cuarto_niv_empleo$n

#Cantidad de personas con estudios de 3er nivel y un empleo pleno
tercer_niv_empleo <- Enemdu2021p %>% filter(p10a==9) %>% filter(condact==1) %>% count(p10a)
#variable usada en articulo
vartercer_niv_empleo <- tercer_niv_empleo$n

agrp <- Enemdu2021p %>% count(p10a)

ing <- Enemdu2021p  %>% count(ingrl)

expe <- Enemdu2021p  %>% count(p45)

conda <- Enemdu2021p  %>% count(condact)

ocup <- Enemdu2021p  %>% count(p42)
#Procesamiento de data

#Se elimina valores atipicos en ingresos y se crea una nueva columna
Enemdu2021p$ingrl_clean <- ifelse(Enemdu2021p$ingrl %in% c(999999) 
                               | is.na(Enemdu2021p$ingrl), NA, Enemdu2021p$ingrl)

#Promedio de ingresos de personas entre (25-35) con estudios de 4to nivel y un empleo pleno
ingreso_prom_cuarto <- Enemdu2021p %>% filter(p10a==10) %>% filter(condact==1) %>% filter(between(p03, 25,35))
#variable usada en articulo
varingreso_prom_cuarto <- mean(ingreso_prom_cuarto$ingrl_clean)

#Promedio de ingresos de personas entre (25-35) con estudios de 3er nivel y un empleo pleno
ingreso_prom_tercer <- Enemdu2021p %>% filter(p10a==9) %>% filter(condact==1) %>% filter(between(p03, 25,35))
#variable usada en articulo
varingreso_prom_tercer <- mean(ingreso_prom_tercer$ingrl_clean)


#Modelo de regresion lineal simple 1
#Relacion entre ingresos y educacion
modelo1 <- lm(ingrl_clean ~ p10a, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo1)

ggplot(Enemdu2021p, aes(x=ingrl_clean,y=p10a))+
  geom_point()

#Modelo de regresion lineal simple 2
#Relacion entre ingresos y experiencia laboral
modelo2 <- lm(ingrl_clean ~ p45, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo2)

ggplot(Enemdu2021p, aes(x=ingrl_clean,y=p45))+
  geom_point()



#Eliminacion de outliers de ingresos a partir del z-score

Enemdu_zscore <- Enemdu2021p %>%
  mutate(z_scores=(abs (ingrl_clean-mean (ingrl_clean,na.rm = TRUE)) / sd (ingrl_clean,na.rm = TRUE)))
Enemdu_ingr_no_out <- Enemdu_zscore %>%
  filter(z_scores<3)

#Modelo de regresion lineal simple 3
#Relacion entre ingresos y educacion
modelo3 <- lm(ingrl_clean ~ p10a, data=Enemdu_ingr_no_out ,na.action = na.exclude)
summary(modelo3)

ggplot(Enemdu_ingr_no_out, aes(x=ingrl_clean,y=p10a))+
  geom_point()

#Modelo de regresion lineal simple 4
#Relacion entre ingresos y experiencia laboral
modelo4 <- lm(ingrl_clean ~ p45, data=Enemdu_ingr_no_out ,na.action = na.exclude)
summary(modelo4)
ggplot(Enemdu_ingr_no_out, aes(x=ingrl_clean,y=p45))+
  geom_point()

Enemdu2021p %>% filter(p02 == 1) %>% filter(between(p03, 25,35)) %>% 
  group_by(condact) %>% summarize(n = n()) %>% mutate(prob = prop.table(n))

#Se filtran a las Personas con empleo adecuado

Enemdu_empleo2021 <- Enemdu_ingr_no_out %>% filter(condact == 1)


#Modelo de regresion lineal simple 5
#Relacion entre ingresos y educacion
modelo5 <- lm(ingrl_clean ~ p10a, data=Enemdu_empleo2021 ,na.action = na.exclude)
summary(modelo5)

ggplot(Enemdu_empleo2021, aes(x=ingrl_clean,y=p10a))+
  geom_point()

#Modelo de regresion lineal simple 6
#Relacion entre ingresos y experiencia laboral
modelo6 <- lm(ingrl_clean ~ p45, data=Enemdu_empleo2021 ,na.action = na.exclude)
summary(modelo6)
ggplot(Enemdu_empleo2021, aes(x=ingrl_clean,y=p45))+
  geom_point()


#Personas con empleo adecuado y edad para trabajar
Enemdu_emp_edad <- Enemdu_empleo2021 %>% filter(between(p03, 25,35))

#Modelo de regresion lineal simple 7
#Relacion entre ingresos y educacion
modelo7 <- lm(ingrl_clean ~ p10a, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo7)

ggplot(Enemdu_emp_edad, aes(x=ingrl_clean,y=p10a))+
  geom_point()

#Modelo de regresion lineal simple 8
#Relacion entre ingresos y experiencia laboral
modelo8 <- lm(ingrl_clean ~ p45, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo8)
ggplot(Enemdu_emp_edad, aes(x=ingrl_clean,y=p45))+
  geom_point()
#############################################  -
#############################################

#Carga de data de 2019
Enemdu2019p <- read.csv("D:\\carpeta de luis\\documentos  maquina de luis andres\\pasantias\\educacion\\Base_Match_dic18_dic19.csv",sep=";")

Enemdu2019p$ingrl_clean <- ifelse(Enemdu2019p$ingrl_dic19 %in% c(999999) 
                                  | is.na(Enemdu2019p$ingrl_dic19), NA, Enemdu2019p$ingrl_dic19)
ing2 <- Enemdu2019p  %>% count(ingrl_clean)

#Modelo de regresion lineal simple 9
#Relacion entre ingresos y educacion
modelo9 <- lm(ingrl_clean ~ p10a_dic19, data=Enemdu2019p ,na.action = na.exclude)
summary(modelo9)

ggplot(Enemdu2019p, aes(x=ingrl_clean,y=p10a_dic19))+
  geom_point()

#Modelo de regresion lineal simple 10
#Relacion entre ingresos y experiencia laboral
modelo10 <- lm(ingrl_clean ~ p45_dic19, data=Enemdu2019p ,na.action = na.exclude)
summary(modelo10)

ggplot(Enemdu2019p, aes(x=ingrl_clean,y=p45_dic19))+
  geom_point()



#Eliminacion de outliers(z-score) de ingresos(2019)

Enemdu_zscore19 <- Enemdu2019p %>%
  mutate(z_scores=(abs (ingrl_clean-mean (ingrl_clean,na.rm = TRUE)) / sd (ingrl_clean,na.rm = TRUE)))
Enemdu_ingr_no_out19 <- Enemdu_zscore19 %>%
  filter(z_scores<3)

#Modelo de regresion lineal simple 11
#Relacion entre ingresos y educacion
modelo11 <- lm(ingrl_clean ~ p10a, data=Enemdu_ingr_no_out19 ,na.action = na.exclude)
summary(modelo11)

ggplot(Enemdu_ingr_no_out19, aes(x=ingrl_clean,y=p10a_dic19))+
  geom_point()

#Modelo de regresion lineal simple 12
#Relacion entre ingresos y experiencia laboral
modelo12 <- lm(ingrl_clean ~ p45_dic19, data=Enemdu_ingr_no_out19 ,na.action = na.exclude)
summary(modelo12)
ggplot(Enemdu_ingr_no_out19, aes(x=ingrl_clean,y=p45_dic19))+
  geom_point()


#Personas con empleo adecuado  2019

Enemdu_empleo2019 <- Enemdu_ingr_no_out19 %>% filter(CONDACT_dic19 == 1)


#Modelo de regresion lineal simple 13
#Relacion entre ingresos y educacion
modelo13 <- lm(ingrl_clean ~ p10a_dic19, data=Enemdu_empleo2019 ,na.action = na.exclude)
summary(modelo13)

ggplot(Enemdu_empleo2019, aes(x=ingrl_clean,y=p10a_dic19))+
  geom_point()

#Modelo de regresion lineal simple 14
#Relacion entre ingresos y experiencia laboral
modelo14 <- lm(ingrl_clean ~ p45_dic19, data=Enemdu_empleo2019 ,na.action = na.exclude)
summary(modelo14)
ggplot(Enemdu_empleo2019, aes(x=ingrl_clean,y=p45_dic19))+
  geom_point()


#Personas con empleo adecuado y edad en trabajar 2019
Enemdu_emp_edad19 <- Enemdu_empleo2019 %>% filter(between(p03_dic19, 25,35))

#Modelo de regresion lineal simple 15
#Relacion entre ingresos y educacion
modelo15 <- lm(ingrl_clean ~ p10a_dic19, data=Enemdu_emp_edad19 ,na.action = na.exclude)
summary(modelo15)

ggplot(Enemdu_emp_edad19, aes(x=ingrl_clean,y=p10a_dic19))+
  geom_point()

#Modelo de regresion lineal simple 16
#Relacion entre ingresos y experiencia laboral
modelo16 <- lm(ingrl_clean ~ p45_dic19, data=Enemdu_emp_edad19 ,na.action = na.exclude)
summary(modelo16)
ggplot(Enemdu_emp_edad19, aes(x=ingrl_clean,y=p45_dic19))+
  geom_point()











