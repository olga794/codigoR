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
summary(Base2017) ## resumen de datos
dim(Base2017) #dimension de los datos y variables
length(Base2017)# dimension de las variables
head(Base2017)## cabeceras 
str(Base2017)## tipos de datos
names(Base2017)## leer nombres de las variables  
##labels(Base2017)## listar el nombre de las filas y columnas 

##############################################CAMBIAR NOMBRE DE VARIABLES 
names(Base2017)[c(4,6,7,8,9,10,11,12,13,14)] <- 
  c("GRUPOP","CONTACTOS","CONTACTOSSR","TCONTACTOSENF","CONTACTOSMN5A","VIHCONFIR","RECIBETAR","RECIBETRIME","RDOBKDX","RDOCULTIVODX")
##################################################### VERIFICAR VALORES NULOS AUSENTES Y "" VACIOS 
colSums(is.na(Base2017)) ## verificar valores nulos
Base2017 %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})## verificar valores ""
map_dbl(Base2017, .f = function(x){sum(is.na(x))}) # Número de datos ausentes por variable
############################################### CAMBIAR LOS TIPOS DE DATOS
##########CAMBIO DE CH A FACTOR 
Base2017$EDAD <- as.factor(Base2017$EDAD)
levels(Base2017$EDAD)
Base2017$SEXO <- as.factor(Base2017$SEXO)
levels(Base2017$SEXO)
Base2017$ETNIA <- as.factor(Base2017$ETNIA)
levels(Base2017$ETNIA)
Base2017$GRUPOP <- as.factor(Base2017$GRUPOP)
levels(Base2017$GRUPOP)
Base2017$COOMORBILIDAD <- as.factor(Base2017$COOMORBILIDAD)
levels(Base2017$COOMORBILIDAD)
Base2017$VIHCONFIR <- as.factor(Base2017$VIHCONFIR)
levels(Base2017$VIHCONFIR)
Base2017$RECIBETAR <- as.factor(Base2017$RECIBETAR)
levels(Base2017$RECIBETAR)
Base2017$RECIBETRIME <- as.factor(Base2017$RECIBETRIME)
levels(Base2017$RECIBETRIME)
Base2017$RDOBKDX <- as.factor(Base2017$RDOBKDX)
levels(Base2017$RDOBKDX)
Base2017$RDOCULTIVODX <- as.factor(Base2017$RDOCULTIVODX)
levels(Base2017$RDOCULTIVODX)

############################################## DISCRETIZAR VARIABLES DE ESTUDIO  
Base2017 <- Base2017 %>%
  mutate(RDOCULTIVO = case_when(RDOCULTIVODX == "-"  ~ "0",
                                RDOCULTIVODX == "+" ~ "1",
                                RDOCULTIVODX == "++" ~ "1",
                                RDOCULTIVODX == "+++" ~ "1",
                                RDOCULTIVODX == "POSITIVO 1 A 20 COLONIAS" ~ "1"))
Base2017$RDOCULTIVO                               
Base2017$RDOCULTIVO <- as.factor(Base2017$RDOCULTIVO)
levels(Base2017$RDOCULTIVO)


##############################################FRECUENCIAS 
# Tabla de frecuencias 
table(Base2017$RDOCULTIVO)
# Tabla de frecuencias EN PORCENTAJE
prop.table(table(Base2017$RDOCULTIVO)) %>% round(digits = 2)
# Tabla de frecuencias relativas de RESULTADO DE CULTIVO por SEXO
prop.table(table(Base2017$SEXO,Base2017$RDOCULTIVO), margin = 1) %>% round(digits = 2)
# Tabla de frecuencias relativas de RESULTADO DE CULTIVO por CONTACTOS
prop.table(table(Base2017$CONTACTOS, Base2017$RDOCULTIVO), margin = 1) %>% round(digits = 2)
# Tabla de frecuencias relativas de RESULTADO DE CULTIVO por EDAD
prop.table(table(Base2017$EDAD, Base2017$RDOCULTIVO), margin = 1) %>% round(digits = 2)
Base2017$EDAD
# Tabla de frecuencias relativas de RESULTADO DE CULTIVO



############################### EDAD A NUMERICO
Base2017$EDAD <- as.character(Base2017$EDAD)
Base2017$EDAD <- as.double(Base2017$EDAD)
c("SEXO")

################################################# binarización o one hot encoding

Base2017n <- dummy.data.frame(Base2017,names =c("SEXO"),separate(sep = "_"))
#(Base2017, name = c("SEXO"), sep = "_")

# NORMALIZACION DE CATEGORIAS 
################################################################

for(unique_value in unique(Base2017$ETNIA)){
  
  Base2017[paste("ETNIA", unique_value, sep = "_")] <- ifelse(Base2017$ETNIA == unique_value, 1, 0)
}
Base2017$ETNIA <- NULL
dmy <- dummyVars(" ~ .", data = Base2017)

# columna_dummy <- function(dataf, columna) {
#   dataf %>% 
#     mutate_at(columna, ~paste(columna, eval(as.symbol(columna)), sep = "_")) %>% 
#     mutate(valor = 1) %>% 
#     spread(key = columna, value = valor, fill = 0)
# }
# columna_dummy(Base2017_dm, "SEXO")

columna_dummy <- function(dataf, columna,nombre) {
  for(unique_value in unique(columna)){
    
    dataf[paste(nombre, unique_value, sep = "_")] <- ifelse(columna == unique_value, 1, 0)
  }
  columna <- NULL
}

columna_dummy(Base2017,Base2017$RECIBETAR,"RECIBETAR")

Base2017 <- Base2017 %>% step_dummy(all_nominal())
dummy_cols(Base2017,  select_columns = c("SEXO"))
#<- dummy.data.frame(Base2017)
#Base2017_dm <- step_dummy(all_nominal(), -all_outcomes())
#Base2017_dm <- dummy.data.frame(Base2017,names = all, sep = "_")
#.data.frame(Base2017=Base2017, names= "SEXO", sep="_")


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


max = apply(Base2017$RDOCULTIVO , 2 , max)
min = apply(Base2017$RDOCULTIVO, 2 , min)
scaled = as.data.frame(scale(Base2017$RDOCULTIVO, center = min, scale = max - min))

summary(data)
summary(scaled)

dummyVars