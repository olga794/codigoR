# LIBRERIAS NECESARIAS
##################################################################
library(reader)
library(readxl)
library(dummies)
library(tidyverse)
library(caret)
library(fastDummies)
# LECTURA DE DATOS archivos de prueba 
##################################################################
## saber el directorio -- getwd()
setwd("/cloud/project/codigo")
# LECTURA DE DATOS 
################################################################
#seleccionar las columnas a leer , cargamos 14 variables 
Base2017 <- read_excel("Base2017.xlsx", sheet = "Hoja 1", 
                       col_types = c("skip", "skip", "text", 
                                     "text", "text", "text", "skip", "skip", 
                                     "text", "skip", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "skip", 
                                     "text", "text", "text", "text", "text", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip"))
View(Base2017)
# VERIFICAR TIPOS DE DATOS CARGADOS
################################################################
head(Base2017)## cabeceras 
str(Base2017)## tipos de datos
names(Base2017)
labels(Base2017)
##CAMBIAR NOMBRE DE VARIABLES 
Base2017[Base2017 == 'GRUPO POBLACIONAL']  <-  'GRUPOP'
names(Base2017)
Base2017$ETNIA

##################################################################

colSums(is.na(Base2017)) ## verificar valores nulos
Base2017 %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})## verificar valores ""
levels(Base2017$SEXO)
summary(Base2017) ## resumen de datos 
pairs(Base2017[6:9],col=Base2017$`57Contactos`) ## relacion existente entre las variables numericas del rango seleccionado
View(Base2017[6:9])
# NORMALIZACION DE CATEGORIAS 
################################################################
##simbolicos
##sexo 
Base2017 <- Base2017 %>% step_dummy(all_nominal())
dummy_cols(Base2017,  select_columns = c("SEXO"))
#<- dummy.data.frame(Base2017)
#Base2017_dm <- step_dummy(all_nominal(), -all_outcomes())
#Base2017_dm <- dummy.data.frame(Base2017,names = all, sep = "_")
#.data.frame(Base2017=Base2017, names= "SEXO", sep="_")
str(Base2017_dm)
names(Base2017_dm)
#Base2017_dm

#Base2017_dm %>% mutate(SEXO = paste("SEXO", SEXO, sep = "_"))


# columna_dummy <- function(dataf, columna) {
#   dataf %>% 
#     mutate_at(columna, ~paste(columna, eval(as.symbol(columna)), sep = "_")) %>% 
#     mutate(valor = 1) %>% 
#     spread(key = columna, value = valor, fill = 0)
# }
# columna_dummy(Base2017_dm, "SEXO")
summary(Base2017_dm)

##objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())
##Numericos

sumarr
# NORMALIZACION DE VARIABLES NUMERICAS 
################################################################