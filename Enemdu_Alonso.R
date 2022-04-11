###############################################################
####                   Descargar los datos                 ####
###############################################################

if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")

# Base de datos Enenmdu2021 ----------
# Obtenemos el id de descarga y creamos el url para descargarlo
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
# Carga de data
Enemdu2021p <- read.spss(fp) 
# Conversion de la data en dataframe
Enemdu2021p <- as.data.frame(Enemdu2021p)

###############################################################
####   Limpieza de datos y creacion de nuevas columnas     ####
###############################################################

#Se crea nueva columna de ingresos y se limpia esta variable(ingrl_clean)
Enemdu2021p <- Enemdu2021p %>% mutate(ingrl_clean=as.numeric(as.character(ingrl)))

#Se crea nueva columna de años de experiencia(expe)
Enemdu2021p <- Enemdu2021p %>% mutate(expe=as.integer(as.character(p45)))

#Se crea nueva columna de edades de las personas encuestadas(edad)
Enemdu2021p <- Enemdu2021p %>% mutate(edad=as.integer(as.character(p03)))

#Se filtra la data para no tener ingresos de 0 y poder crear una columna del log
Enemdu2021p <- Enemdu2021p %>% filter(ingrl_clean > 0)

#Se crea una nueva columna, logaritmo de los ingresos(log_ing)
Enemdu2021p <- Enemdu2021p %>% mutate(log_ing=log(ingrl_clean))

## Creacion de columna experiencia al cuadrado para modelo(expe2)
Enemdu2021p <- Enemdu2021p %>% mutate(expe2=expe*expe)

## CREACION DE VARIABLE DUMMY para modelo
Enemdu2021p$sex_dummy <- ifelse(Enemdu2021p$p02 %in% c("Hombre"), 1,0)

#Se crea nueva columna para calcular los años de educacion(educ_anio)
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

#Eliminacion de outliers de columna ingresos a partir del z-score
Enemdu_zscore <- Enemdu2021p %>%
  mutate(z_scores=(abs (ingrl_clean-mean (ingrl_clean,na.rm = TRUE)) / sd (ingrl_clean,na.rm = TRUE)))

#Se filtra la data para eliminar los outliers
Enemdu_ingr_no_out <- Enemdu_zscore %>%
  filter(z_scores<3)

#Se filtra la data para tener a las personas con empleo adecuado
Enemdu_empleo2021 <- Enemdu_ingr_no_out %>% filter(condact == "Adecuado")

#Se filtra la data para obtener Personas con empleo adecuado y edad para trabajar
Enemdu_emp_edad <- Enemdu_empleo2021 %>% filter(between(edad, 30,35))

#Se filtra la data `para obtener solo a personas con educacion superior`
Enemdu_tit_filt <- Enemdu_emp_edad %>% filter(p10a %in% c("Superior no universitario","Superior Universitario","Post-grado")) %>% filter(between(ingrl_clean,0,7000))


###############################################################
####                   Variables a usar                    ####
###############################################################

####                      VARIABLE 1
#Cantidad de personas con estudios de 4to nivel y un empleo pleno(Numerador)
cuarto_niv_empleo <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% filter(condact=="Adecuado") %>% count(p10a)
#Cantidad de personas con estudios de 4to nivel(denominador)
den <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% count(p10a)
#variable usada en articulo(Porcentaje de personas con empleo q tienen titulo de 4to nivel)
var_porc_cuarto_niv_empleo <- ((cuarto_niv_empleo$n)/(den$n))*100

####                      VARIABLE 2
#Cantidad de personas con estudios de 3er nivel y un empleo pleno
tercer_niv_empleo <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% filter(condact=="Adecuado") %>% count(p10a)
#Cantidad de personas con estudios de 3er nivel(denominador)
den2 <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% count(p10a)
#variable usada en articulo(Porcentaje de personas con empleo q tienen titulo de 3er nivel)
var_porc_tercer_niv_empleo <- ((tercer_niv_empleo$n)/(den2$n))*100

####                      VARIABLE 3
#Promedio de ingresos de personas entre (30-35) con estudios de 4to nivel y un empleo pleno
ingreso_prom_cuarto <- Enemdu2021p %>% filter(p10a=="Post-grado") %>% filter(condact=="Adecuado") %>% filter(between(edad,  30,35))
#variable usada en articulo
varingreso_prom_cuarto <- mean(ingreso_prom_cuarto$ingrl_clean)

####                      VARIABLE 4
#Promedio de ingresos de personas entre (30-35) con estudios de 3er nivel y un empleo pleno
ingreso_prom_tercer <- Enemdu2021p %>% filter(p10a=="Superior Universitario") %>% filter(condact=="Adecuado") %>% filter(between(edad, 30,35))
#variable usada en articulo
varingreso_prom_tercer <- mean(ingreso_prom_tercer$ingrl_clean)

###############################################################
####                        Graficos                       ####
###############################################################

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


#GRAFICOS DE DISPERSION CON LINEA DE TENDENCIA(Cual serian los mejores para el articulo)

#EDUCACION CON INGRESOS

ggplot(Enemdu_emp_edad, aes(x=educ_anio,y=log_ing,color=p02))+
  geom_point()

ggplot(Enemdu_emp_edad, aes(x=educ_anio,y=log_ing,fill=p02))+
  geom_point()+
  geom_smooth()

ggplot(Enemdu_emp_edad, aes(x=educ_anio,y=log_ing,color=p02))+
  geom_smooth()

ggplot(Enemdu_emp_edad, aes(x=educ_anio,y=log_ing,color=p02))+
  geom_smooth(method = "lm", se = FALSE)


#EXPERIENCIA CON INGRESOS

ggplot(Enemdu_emp_edad, aes(x=expe,y=log_ing,color=p02))+
  geom_point()

ggplot(Enemdu_emp_edad, aes(x=expe,y=log_ing,fill=p02))+
  geom_point()+
  geom_smooth()

ggplot(Enemdu_emp_edad, aes(x=expe,y=log_ing,color=p02))+
  geom_smooth()

ggplot(Enemdu_emp_edad, aes(x=expe,y=ingrl_clean,color=p02))+
  geom_smooth(method = "lm", se = FALSE)
###############################################################
####                        Modelos                        ####
###############################################################


##################   MODELOS DE REGRESION MULTIPLE  ##################

#MODELO CON INGRESOS(ingresos=b0+b1(años_educacion)+b2(experiencia))
modelo_multiple <- lm(ingrl_clean ~ educ_anio + expe, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple)

#MODELO CON LOGARITMO DE INGRESOS(log(ingresos)=b0+b1(años_educacion)+b2(experiencia))
modelo_multiple1 <- lm(log_ing ~ educ_anio + expe, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple1)

#MODELO CON INGRESOS(ingresos=b0+b1(años_educacion)+b2(experiencia)+b3(experiencia al cuadrado))
modelo_multiple2 <- lm(ingrl_clean ~ educ_anio + expe + expe2, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple2)

#MODELO CON LOGARITMO DE INGRESOS(log(ingresos)=b0+b1(años_educacion)+b2(experiencia)+b3(experiencia al cuadrado))
modelo_multiple3 <- lm(log_ing ~ educ_anio + expe + expe2, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple3)

#MODELO CON INGRESOS(ingresos=b0+b1(años_educacion)+b2(experiencia)+b3(experiencia al cuadrado)+b4(Sexo(hombres==1)))
modelo_multiple4 <- lm(ingrl_clean ~ educ_anio + expe + expe2 + sex_dummy, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple4)

#MODELO FINAL A USAR. YA QUE AL USAR LOGARITMO, LOS COEFICIENTES SON PORCENTAJES
#MODELO CON LOGARITMO DE INGRESOS(log(ingresos)=b0+b1(años_educacion)+b2(experiencia)+b3(experiencia al cuadrado)+b4(Sexo(hombres==1)))
modelo_multiple5 <- lm(log_ing ~ educ_anio + expe + expe2 + sex_dummy, data=Enemdu_emp_edad ,na.action = na.exclude)
summary(modelo_multiple5)

