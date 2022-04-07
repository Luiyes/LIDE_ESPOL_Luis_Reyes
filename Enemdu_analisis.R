
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

#<<<<<<< HEAD
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

#Carga de data 2021
#Enemdu2021p <- as.data.frame(read_sav("D:\\carpeta de luis\\documentos  maquina de luis andres\\pasantias\\enemdu2021\\BDDenemdu_personas_2021_anual.sav"))
agrp22 <- Enemdu2021p %>% count(p10a)
Enemdu2021p %>% count(prov)
Enemdu2021p %>% count(dominio)


#Enemdu2021p_c <- haven::as_factor(Enemdu2021p)

#Revision de variables a tratar
cond <- Enemdu2021p %>% count(condact)
#Cantidad de personas con estudios de 4to nivel y un empleo pleno
cuarto_niv_empleo <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% filter(condact=="Adecuado") %>% count(p10a)

den <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% count(p10a)

#variable usada en articulo
var_porc_cuarto_niv_empleo <- ((cuarto_niv_empleo$n)/(den$n))*100




#Cantidad de personas con estudios de 3er nivel y un empleo pleno
tercer_niv_empleo <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% filter(condact=="Adecuado") %>% count(p10a)

den2 <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% count(p10a)
#variable usada en articulo   
var_porc_tercer_niv_empleo <- ((tercer_niv_empleo$n)/(den2$n))*100

agrp <- Enemdu2021p %>% count(p10a)
agrp2 <- Enemdu2021p %>% count(p10b)

ing <- Enemdu2021p  %>% count(ingrl)

expe <- Enemdu2021p  %>% count(p45)

conda <- Enemdu2021p  %>% count(condact)

ocup <- Enemdu2021p  %>% count(p42)

razones <- Enemdu2021p  %>% count(p09)

año_aprob <- Enemdu2021p  %>% count(p10b)

ob_tit <- Enemdu2021p  %>% count(p12a)

cod_titu <- Enemdu2021p  %>% count(p12b)

c_sex <- Enemdu2021p  %>% count(p02)
#Procesamiento de data


#Se elimina valores atipicos en ingresos y se crea una nueva columna
#<<<<<<< HEAD
Enemdu2021p$ingrl_clean <- ifelse(Enemdu2021p$ingrl %in% c(999999, "Gasta mas de lo que gana","No informa") 
                                  | is.na(Enemdu2021p$ingrl), NA, Enemdu2021p$ingrl)
#=======
Enemdu2021p$ingrl_clean <- ifelse(as.numeric(Enemdu2021p$ingrl) > 5000
                                  | is.na(Enemdu2021p$ingrl)|Enemdu2021p$ingrl=="No informa", NA, Enemdu2021p$ingrl)
#>>>>>>> 06889dc1527f2f36b077cb10c38aacbe69776eab

#Se crea nueva columna para los años de educacion
#<<<<<<< HEAD

Enemdu2021p <- Enemdu2021p %>% mutate(educ_anio = case_when(p10a == "Ninguno" ~ 0, #ninguno
                                                            p10a == "Centro de alfabetización" & between(p10b, 0, 3) ~ 2 * p10b, #centro de alfabetizacion 
                                                            p10a == "Centro de alfabetización" & between(p10b, 4, 10) ~ 3 + p10b, #centro de alfabetizacion
                                                            p10a == "Jardín de Infantes" ~ 1, #jardín de infantes
                                                            p10a == "Primaria" ~ 1 + p10b, #primaria
                                                            p10a == "Educación Básica" ~ p10b, #educacion basica
                                                            p10a == "Secundaria" ~ 7 + p10b, #secundaria
                                                            p10a == "Educación  Media" ~ 10 + p10b, #educacion media
                                                            p10a == "Superior no universitario" ~ 13 + p10b, #superior no universitario
                                                            p10a == "Superior Universitario" ~ 13 + p10b, #superior universitario
                                                            p10a == "Post-grado" ~ 18 + p10b, #postgrado
                                                            TRUE ~ NA_real_))

Enemdu2021p %>% select(p10a, p10b, educ_anio) %>% view()


Enemdu2021p %>% count(educ_anio)
Enemdu2021p %>% filter(p10a=="Post-grado") %>%count(p10b)

#=======
#Enemdu2021p$años_edu <- if(Enemdu2021p$p10a==2 | Enemdu2021p$p10a==3){1} else {if(Enemdu2021p$p10a==4){7} else {if(Enemdu2021p$p10a==6 |Enemdu2021p$p10a==7){13} else{ if(Enemdu2021p$p10a==5){10} else {if(Enemdu2021p$p10a==8){16} else {if(Enemdu2021p$p10a==9){17} else {if(Enemdu2021p$p10a==10){19} else {0}}}}}}}
Enemdu2021p$años_edu <- ifelse(Enemdu2021p$p10a=="Centro de alfabetización" | between(Enemdu2021p$p10b,0,3),2*Enemdu2021p$p10b , ifelse(Enemdu2021p$p10a=="Centro de alfabetización" | between(Enemdu2021p$p10b,4,10),3+Enemdu2021p$p10b, ifelse(Enemdu2021p$p10a=="Jardín de Infantes",1, ifelse(Enemdu2021p$p10a=="Primaria",1+Enemdu2021p$p10b,ifelse(Enemdu2021p$p10a=="Educación Básica",Enemdu2021p$p10b,ifelse(Enemdu2021p$p10a=="Secundaria",7+Enemdu2021p$p10b,ifelse(Enemdu2021p$p10a=="Educación  Media",10+Enemdu2021p$p10b,ifelse(Enemdu2021p$p10a=="Superior no Universitario",13+Enemdu2021p$p10b,ifelse(Enemdu2021p$p10a=="Superior Universitario",13+Enemdu2021p$p10b,ifelse(Enemdu2021p$p10a=="Post-grado",18+Enemdu2021p$p10b,0))))))))))
Enemdu2021p$años_edu
Enemdu2021p %>% count(años_edu)
Enemdu2021p %>% filter(p10a=="Post-grado") %>%count(p10b)

Enemdu2021p %>% filter(is.na(años_edu))

#>>>>>>> 06889dc1527f2f36b077cb10c38aacbe69776eab
#Promedio de ingresos de personas entre (30-35) con estudios de 4to nivel y un empleo pleno
ingreso_prom_cuarto <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% filter(condact=="Adecuado") %>% filter(between(as.numeric(p03), 30,35))
#variable usada en articulo
varingreso_prom_cuarto <- mean(ingreso_prom_cuarto$ingrl_clean)

#Promedio de ingresos de personas entre (30-35) con estudios de 3er nivel y un empleo pleno
ingreso_prom_tercer <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% filter(condact=="Adecuado") %>% filter(between(as.numeric(p03), 30,35))
#variable usada en articulo
varingreso_prom_tercer <- mean(ingreso_prom_tercer$ingrl_clean)


#Modelo de regresion lineal simple 1
#Relacion entre ingresos y educacion
modelo1 <- lm(ingrl_clean ~ p10a, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo1)

view(Enemdu2021p$p10a)
ggplot(Enemdu2021p, aes(x=p10a))+
  geom_bar()

modelo20 <- lm(ingrl_clean ~ educ_anio, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo20)

ggplot(Enemdu2021p, aes(x=ingrl_clean,y=educ_anio))+
  geom_point()

#Modelo de regresion lineal simple 2
#Relacion entre ingresos y experiencia laboral 
modelo2 <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu2021p ,na.action = na.exclude)
summary(modelo2)

ggplot(Enemdu2021p, aes(x=ingrl_clean,y=as.numeric(p45)))+
  geom_point()



#Eliminacion de outliers de ingresos a partir del z-score

Enemdu_zscore <- Enemdu2021p %>%
  mutate(z_scores=(abs (ingrl_clean-mean (ingrl_clean,na.rm = TRUE)) / sd (ingrl_clean,na.rm = TRUE)))
Enemdu_ingr_no_out <- Enemdu_zscore %>%
  filter(z_scores<3)

#Modelo de regresion lineal simple 3
#Relacion entre ingresos y educacion
modelo3 <- lm(ingrl_clean ~ educ_anio, data=Enemdu_ingr_no_out ,na.action = na.exclude)
summary(modelo3)

ggplot(Enemdu_ingr_no_out, aes(x=p10a))+
  geom_bar()

#Modelo de regresion lineal simple 4
#Relacion entre ingresos y experiencia laboral
modelo4 <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu_ingr_no_out ,na.action = na.exclude)
summary(modelo4)
ggplot(Enemdu_ingr_no_out, aes(x=ingrl_clean,y=as.numeric(p45)))+
  geom_point()

Enemdu2021p %>% filter(p02 == "Hombre") %>% filter(between(p03, 30,35)) %>% 
  group_by(condact) %>% summarize(n = n()) %>% mutate(prob = prop.table(n))
Enemdu_empleo2021 <- Enemdu_ingr_no_out %>% filter(condact == "Adecuado")

Enemdu_empleo2021 %>% count(ingrl_clean)

#Modelo de regresion lineal simple 5
#Relacion entre ingresos y educacion
modelo5 <- lm(ingrl_clean ~ educ_anio, data=Enemdu_empleo2021 ,na.action = na.exclude)
summary(modelo5)

ggplot(Enemdu_empleo2021, aes(x=p10a))+
  geom_bar()

#Modelo de regresion lineal simple 6
#Relacion entre ingresos y experiencia laboral
modelo6 <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu_empleo2021 ,na.action = na.exclude)
summary(modelo6)
ggplot(Enemdu_empleo2021, aes(x=ingrl_clean,y=as.numeric(p45)))+
  geom_point()


#Personas con empleo adecuado y edad para trabajar
Enemdu_emp_edad <- Enemdu_empleo2021 %>% filter(between(p03, 30,35))


ggplot(Enemdu_emp_edad,aes(x=dominio))+
  geom_bar()

#Modelo de regresion lineal simple 7
#Relacion entre ingresos y educacion
modelo7 <- lm(ingrl_clean ~ educ_anio, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo7)


ggplot(Enemdu_emp_edad, aes(x=ingrl_clean,y=educ_anio))+
  geom_point()

#Modelo de regresion lineal simple 8
#Relacion entre ingresos y experiencia laboral
modelo8 <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo8)
ggplot(as.data.frame(Enemdu_emp_edad), aes(x=ingrl_clean,y=as.numeric(p45),color=as.factor(p02)))+
  geom_point()

Enemdu_tit_filt <- Enemdu_emp_edad %>% filter(p10a %in% c("Superior no universitario","Superior Universitario","Post-grado"))  

Enemdu_tit_filt %>% count(p10a)
#Modelo de regresion lineal simple 9
#Relacion entre ingresos y educacion
modelo9 <- lm(ingrl_clean ~ educ_anio, data=Enemdu_tit_filt ,na.action = na.exclude)
summary(modelo9)


ggplot(Enemdu_emp_edad, aes(x=ingrl_clean,y=educ_anio))+
  geom_point()

#Modelo de regresion lineal simple 10
#Relacion entre ingresos y experiencia laboral
modelo10 <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu_tit_filt ,na.action = na.exclude)
summary(modelo10)
ggplot(as.data.frame(Enemdu_emp_edad), aes(x=ingrl_clean,y=as.numeric(p45),color=as.factor(p02)))+
  geom_point()


#####HOMBRES

Enemdu_emp_edad_homb <- Enemdu_tit_filt %>% filter(p02=="Hombre")

modelo_hom_edu <- lm(ingrl_clean ~ educ_anio, data=Enemdu_emp_edad_homb ,na.action = na.exclude)
summary(modelo_hom_edu)

ggplot(as.data.frame(Enemdu_emp_edad_homb), aes(x=ingrl_clean,y=educ_anio))+
  geom_point()

modelo_hom_expe <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu_emp_edad_homb ,na.action = na.exclude)
summary(modelo_hom_expe)

ggplot(as.data.frame(Enemdu_emp_edad_homb), aes(x=ingrl_clean,y=as.numeric(p45)))+
  geom_point()


#####Mujeres

Enemdu_emp_edad_muj <- Enemdu_tit_filt %>% filter(p02=="Mujer")

modelo_muj_edu <- lm(ingrl_clean ~ educ_anio, data=Enemdu_emp_edad_muj ,na.action = na.exclude)
summary(modelo_muj_edu)

ggplot(as.data.frame(Enemdu_emp_edad_muj), aes(x=ingrl_clean,y=educ_anio))+
  geom_point()

modelo_muj_expe <- lm(ingrl_clean ~ as.numeric(p45), data=Enemdu_emp_edad_muj ,na.action = na.exclude)
summary(modelo_muj_expe)

ggplot(as.data.frame(Enemdu_emp_edad_muj), aes(x=ingrl_clean,y=as.numeric(p45)))+
  geom_point()

#############################################  










  

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











