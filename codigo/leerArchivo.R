# LIBRERIAS NECESARIAS
##################################################################
library(reader)
library(readxl)


# LECTURA DE DATOS archivos de prueba 
##################################################################
## saber el directorio -- getwd()
setwd("/cloud/project/codigo")
# LECTURA DE DATOS 
################################################################
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
##View(Base2017)
# VERIFICAR TIPOS DE DATOS CARGADOS
################################################################
head(Base2017)## cabeceras 
str(Base2017)## tipos de datos

colSums(is.na(Base2017)) ## verificar valores nulos
summary(Base2017) ## resumen de datos 
pairs(Base2017[6:9],col=Base2017$`57Contactos`) ## relacion existente entre las variables numericas del rango seleccionado
View(Base2017[6:9])
# NORMALIZACION DE CATEGORIAS 
################################################################



# NORMALIZACION DE VARIABLES NUMERICAS 
################################################################