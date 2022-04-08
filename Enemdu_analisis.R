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
fp <- file.path(td, list.files(path = td)[3])

# to verify that the file was downloaded, use: list.files(path = td)
list.files(path = td)
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

Enemdu2021p <- Enemdu2021p %>% mutate(ingrl2=as.integer(Enemdu2021p$ingrl))
Enemdu2021p <- Enemdu2021p %>% mutate(ingrl3=as.numeric(Enemdu2021p$ingrl))
Enemdu2021p <- Enemdu2021p %>% mutate(ingrl4=as.character(Enemdu2021p$ingrl))
Enemdu2021p <- Enemdu2021p %>% mutate(ingrl5=as.numeric(Enemdu2021p$ingrl4))
Enemdu2021p <- Enemdu2021p %>% mutate(expe=as.integer(Enemdu2021p$p45))
Enemdu2021p <- Enemdu2021p %>% mutate(expe2=as.integer(as.character(Enemdu2021p$p45)))
Enemdu2021p <- Enemdu2021p %>% mutate(eda=as.integer(Enemdu2021p$p03))
Enemdu2021p <- Enemdu2021p %>% mutate(edad=as.integer(as.character(Enemdu2021p$p03)))

#Se elimina valores atipicos en ingresos y se crea una nueva columna
#<<<<<<< HEAD
Enemdu2021p$ingrl_clean <- ifelse(Enemdu2021p$ingrl %in% c(999999, "Gasta mas de lo que gana","No informa") 
                                  | is.na(Enemdu2021p$ingrl), NA, Enemdu2021p$ingrl)

Enemdu2021p <- Enemdu2021p %>% filter(ingrl5 > 0)

Enemdu2021p <- Enemdu2021p %>% mutate(log_ing=log(ingrl5))

Enemdu2021p %>% select(ingrl, ingrl_clean, ingrl2, log_ing,ingrl3,ingrl4,ingrl5) %>% view()
Enemdu2021p %>% select(p45, expe, expe2) %>% view()
Enemdu2021p %>% select(p03, eda, edad) %>% view()

mostrar <- Enemdu2021p %>% count(ingrl_clean)
class(Enemdu2021p$ingrl)
orden <- sort(Enemdu2021p$as.numeric(ingrl_clean))

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
mos <- Enemdu2021p %>% count(ingrl5)
Enemdu2021p %>% filter(p10a=="Post-grado") %>%count(p10b)

#=======

#>>>>>>> 06889dc1527f2f36b077cb10c38aacbe69776eab
#Promedio de ingresos de personas entre (30-35) con estudios de 4to nivel y un empleo pleno
ingreso_prom_cuarto <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% filter(condact=="Adecuado") %>% filter(between(edad,  30,35))
#variable usada en articulo
varingreso_prom_cuarto <- mean(ingreso_prom_cuarto$ingrl5)

#Promedio de ingresos de personas entre (30-35) con estudios de 3er nivel y un empleo pleno
ingreso_prom_tercer <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% filter(condact=="Adecuado") %>% filter(between(edad, 30,35))
#variable usada en articulo
varingreso_prom_tercer <- mean(ingreso_prom_tercer$ingrl5)


#Modelo de regresion lineal simple 1
#Relacion entre ingresos y educacion
modelo1 <- lm(log_ing ~ p10a, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo1)

view(Enemdu2021p$p10a)
ggplot(Enemdu2021p, aes(x=p10a))+
  geom_bar()

modelo20 <- lm(log_ing ~ educ_anio, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo20)

ggplot(Enemdu2021p, aes(x=ingrl5,y=educ_anio))+
  geom_point()

#Modelo de regresion lineal simple 2
#Relacion entre ingresos y experiencia laboral 
modelo2 <- lm(log_ing ~ expe2, data=Enemdu2021p ,na.action = na.exclude)
summary(modelo2)

ggplot(Enemdu2021p, aes(x=ingrl5,y=expe2))+
  geom_point()



#Eliminacion de outliers de ingresos a partir del z-score

Enemdu_zscore <- Enemdu2021p %>%
  mutate(z_scores=(abs (ingrl5-mean (ingrl5,na.rm = TRUE)) / sd (ingrl5,na.rm = TRUE)))
Enemdu_ingr_no_out <- Enemdu_zscore %>%
  filter(z_scores<3)

#Modelo de regresion lineal simple 3
#Relacion entre ingresos y educacion
modelo3 <- lm(log_ing ~ educ_anio, data=Enemdu_ingr_no_out ,na.action = na.exclude)
summary(modelo3)

ggplot(Enemdu_ingr_no_out, aes(x=log_ing,y=educ_anio))+
  geom_point()

#Modelo de regresion lineal simple 4
#Relacion entre ingresos y experiencia laboral
modelo4 <- lm(ingrl5 ~ expe2, data=Enemdu_ingr_no_out ,na.action = na.exclude)
summary(modelo4)
ggplot(Enemdu_ingr_no_out, aes(x=ingrl5,y=expe2))+
  geom_point()

Enemdu2021p %>% filter(p02 == "Hombre") %>% filter(between(p03, 25,30)) %>% 
  group_by(condact) %>% summarize(n = n()) %>% mutate(prob = prop.table(n))


Enemdu_empleo2021 <- Enemdu_ingr_no_out %>% filter(condact == "Adecuado")

Enemdu_empleo2021 %>% count(ingrl_clean)

#Modelo de regresion lineal simple 5
#Relacion entre ingresos y educacion
modelo5 <- lm(ingrl5 ~ educ_anio, data=Enemdu_empleo2021 ,na.action = na.exclude)
summary(modelo5)

ggplot(Enemdu_empleo2021, aes(x=ingrl5,y=educ_anio))+
  geom_point()


#Modelo de regresion lineal simple 6
#Relacion entre ingresos y experiencia laboral
modelo6 <- lm(ingrl5 ~ expe2, data=Enemdu_empleo2021 ,na.action = na.exclude)
summary(modelo6)
ggplot(Enemdu_empleo2021, aes(x=ingrl5,y=expe2))+
  geom_point()



#Personas con empleo adecuado y edad para trabajar
Enemdu_emp_edad <- Enemdu_empleo2021 %>% filter(between(edad, 30,35))


ggplot(Enemdu_emp_edad,aes(x=dominio))+
  geom_bar()
ggplot(Enemdu_emp_edad,aes(x=p10a))+
  geom_bar()


#Modelo de regresion lineal simple 7
#Relacion entre ingresos y educacion
modelo7 <- lm(ingrl5 ~ educ_anio, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo7)


ggplot(Enemdu_emp_edad, aes(x=ingrl5,y=educ_anio))+
  geom_point()

#Modelo de regresion lineal simple 8
#Relacion entre ingresos y experiencia laboral
modelo8 <- lm(ingrl5 ~ expe2, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo8)
ggplot(Enemdu_emp_edad, aes(x=ingrl5,y=expe2,color=as.factor(p02)))+
  geom_point()

Enemdu_tit_filt <- Enemdu_emp_edad %>% filter(p10a %in% c("Superior no universitario","Superior Universitario","Post-grado")) %>% filter(between(ingrl_clean,0,7000))

Enemdu_tit_filt %>% count(p10a)
#Modelo de regresion lineal simple 9
#Relacion entre ingresos y educacion
modelo9 <- lm(ingrl5 ~ educ_anio, data=Enemdu_tit_filt ,na.action = na.exclude)
summary(modelo9)


ggplot(Enemdu_tit_filt, aes(x=ingrl5,y=educ_anio))+
  geom_point()


#Modelo de regresion lineal simple 10
#Relacion entre ingresos y experiencia laboral
modelo10 <- lm(ingrl5 ~ expe2, data=Enemdu_tit_filt ,na.action = na.exclude)
summary(modelo10)
ggplot(Enemdu_tit_filt, aes(x=ingrl5,y=expe2,color=as.factor(p02)))+
  geom_point()



#Gráfico porcentaje de personas que que tienen estudio
porcentage <- Enemdu_emp_edad %>% group_by(p10a) %>% count() %>% ungroup() %>% mutate(percentage=`n`/sum(`n`)*100)
ggplot(porcentage, aes(x="", y=percentage, fill=p10a))+
  geom_bar(stat="identity", color="white") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5),size=4) +
  coord_polar(theta = "y") + 
  theme_void()



#Grafico porcentaje de nivel de empleo por provincia
porcentage2 <- Enemdu_emp_edad %>% group_by(dominio) %>% count() %>% ungroup() %>% mutate(percentage=`n`/sum(`n`)*100)
porcentage2
ggplot(porcentage2, aes(x="", y=percentage, fill=dominio))+
  geom_bar(stat="identity", color="white") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  theme_void()


#####HOMBRES

Enemdu_emp_edad_homb <- Enemdu_tit_filt %>% filter(p02=="Hombre")

modelo_hom_edu <- lm(ingrl5 ~ educ_anio, data=Enemdu_emp_edad_homb ,na.action = na.exclude)
summary(modelo_hom_edu)


ggplot(Enemdu_emp_edad_homb, aes(x=ingrl5,y=educ_anio))+
  geom_point()

modelo_hom_expe <- lm(ingrl5 ~ expe2, data=Enemdu_emp_edad_homb ,na.action = na.exclude)
summary(modelo_hom_expe)

ggplot(Enemdu_emp_edad_homb, aes(x=ingrl5,y=expe2))+
  geom_point()


#####Mujeres

Enemdu_emp_edad_muj <- Enemdu_tit_filt %>% filter(p02=="Mujer")

modelo_muj_edu <- lm(ingrl5 ~ educ_anio, data=Enemdu_emp_edad_muj ,na.action = na.exclude)
summary(modelo_muj_edu)

ggplot(Enemdu_emp_edad_muj, aes(x=ingrl5,y=educ_anio))+
  geom_point()

modelo_muj_expe <- lm(ingrl5 ~ expe2, data=Enemdu_emp_edad_muj ,na.action = na.exclude)
summary(modelo_muj_expe)

ggplot(Enemdu_emp_edad_muj, aes(x=ingrl5,y=expe2))+
  geom_point()

#############################################  

###MODELO DE REGRESION MULTIPLE

modelo_multiple <- lm(ingrl5 ~ educ_anio + expe2, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple)

modelo_multiple1 <- lm(log_ing ~ educ_anio + expe2, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple1)
## Creacion de columna experiencia al cuadrado para modelo
Enemdu_emp_edad <- Enemdu_emp_edad %>% mutate(expe22=expe2*expe2)


Enemdu_emp_edad %>% select(expe2, expe22) %>% view()

modelo_multiple2 <- lm(ingrl5 ~ educ_anio + expe2 + expe22, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple2)

modelo_multiple3 <- lm(log_ing ~ educ_anio + expe2 + expe22, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple3)

## CREACION DE VARIABLE DUMMY
Enemdu_emp_edad$sex_dummy <- ifelse(Enemdu_emp_edad$p02 %in% c("Hombre"), 1,0)

Enemdu_emp_edad %>% select(p02, sex_dummy) %>% view()

modelo_multiple4 <- lm(ingrl5 ~ educ_anio + expe2 + expe22 + sex_dummy, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple4)

modelo_multiple5 <- lm(log_ing ~ educ_anio + expe2 + expe22 + sex_dummy, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple5)

