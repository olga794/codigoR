# LIBRERIAS NECESARIAS
##################################################################
library(reader)
library(readxl)
library(dummies)
library(tidyverse)
library(caret)
library(fastDummies)
library(neuralnet)
library(ggplot2)
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
################################################## AJUSTAR NOMBRE DE FACTOR
Base2017 <- Base2017 %>%
  mutate(ETNIAT = case_when(ETNIA == "NEGRO,MULATO,AFROCOLOMBIANO" ~ "NEGRO/MULATO/AFROCOLOMBIANO",
                            TRUE ~ ETNIA   ))
Base2017$ETNIA <- NULL
############################################### CAMBIAR LOS TIPOS DE DATOS
##########CAMBIO DE CH A FACTOR 

Base2017$SEXO <- as.factor(Base2017$SEXO)
levels(Base2017$SEXO)
Base2017$ETNIAT <- as.factor(Base2017$ETNIAT)
levels(Base2017$ETNIAT)
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
######EDAD SE CONVIERTE A FACTOR SE VERIFICAN LAS ENTRADAS 
Base2017$EDAD <- as.factor(Base2017$EDAD)
levels(Base2017$EDAD)
######## SE AJUSTAN VALORES ATIPICOS 
Base2017$EDAD <- as.character(Base2017$EDAD)
Base2017 <- Base2017 %>%
  mutate(EDADD = case_when(EDAD == "1/12" ~ "0.833",
                           EDAD == "5/12" ~ "0.416",
                           TRUE ~ EDAD   ))
Base2017$EDADD <- as.double(Base2017$EDADD)
Base2017$EDAD <- NULL

############################################## DISCRETIZAR VARIABLE DE ESTUDIO  
Base2017 <- Base2017 %>%
  mutate(RDOCULTIVO = case_when(RDOCULTIVODX == "-"  ~ "0",
                                RDOCULTIVODX == "+" ~ "1",
                                RDOCULTIVODX == "++" ~ "1",
                                RDOCULTIVODX == "+++" ~ "1",
                                RDOCULTIVODX == "POSITIVO 1 A 20 COLONIAS" ~ "1"))
Base2017$RDOCULTIVO <- as.numeric(Base2017$RDOCULTIVO)
Base2017$RDOCULTIVODX <- NULL

##############################################FRECUENCIAS --REVISAR LAS VARIABLES 
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
##################################################################################################
########### RED NEURONAL #########################################################################
############################################################################################################
str(Base2017)
names(Base2017)
################### NORMALIZAR DATOS NUMERICOS

###ESCALAR LOS DATOS NUMERICOS
BNN <- as.data.frame(scale(Base2017[5:8]))
Base2017 <- Base2017 %>%  mutate(CONTACTOS = scale(Base2017[5]))

max = apply(Base2017 , 2 , max)
min = apply(Base2017, 2 , min)
BaseScaled = as.data.frame(scale(Base2017$CONTACTOS, center = min, scale = max - min))
################### NORMALIZAR VAOLRES CUALITATIVOS 
## formula con nombres a normalizar
nom = names(Base2017)
formNormaC = as.formula(paste(" ~ ", paste(nom[!nom %in% "R"], collapse = " + ")))
base <- as.data.frame(model.matrix( formNormaC, data = Base2017 ))
#######
Base2017NormaC <- model.matrix( formNormaC, data = Base2017 )
head(Base2017NormaC)
str(Base2017NormaC)
summary(Base2017NormaC)
names(Base2017NormaC)
#####
#### prueba
base$`(Intercept)` <- NULL
nom = names(base)
formp = as.formula(paste("RDOCULTIVO ~", paste(nom[!nom %in% "RDOCULTIVO"], collapse = " + ")))
nnUno1 <- neuralnet(form,
                    data = Base2017NormaC,
                    #Un vector de enteros que especifica el número de neuronas ocultas (vértices) en cada capa.
                    hidden = 2,
                    #función diferenciable que se utiliza para el cálculo del error.ce =la entropía cruzada
                    err.fct = "ce",
                    #si se debe aplicar función diferenciable que se utiliza para suavizar el resultado.
                    linear.output= FALSE)
##########################CONTRUIR LA MATRIZ DE INDICE PARA PRUEBAS Y ENTRENAMIENTO 
nrow(Base2017) ## 602
random = round(0.1 * nrow(Base2017), digits = 0) ##60
total_index = 1:nrow(Base2017)
index_test = sample(total_index, size = random)
index_train = setdiff(total_index,index_test)

############################################################# SEPARAR LOS DOS GRUPOS DE  DATOS
BtrainNN = Base2017[index_train,]
BtestNN = Base2017[index_test, ]
dim(BtrainNN)
dim(BtestNN)
######
str(BtrainNN)
names(BtrainNN)
BtrainNN$EDAD <- as.character(BtrainNN$EDAD)
BtrainNN$EDAD <- as.numeric(BtrainNN$EDAD)
#####################CONSTRUIR LA FORMULA 
nom = names(BtrainNN)
form = as.formula(paste("RDOCULTIVO ~", paste(nom[!nom %in% "RDOCULTIVO"], collapse = " + ")))
##############################################################################################
# dummify the data
dmy <- dummyVars(" ~ .", data = BtrainNN)
trsf <- data.frame(predict(dmy, newdata = customers))

BtrainNN <- as.data.frame( scale(BtrainNN[5:8]),data())




nnUno1 <- neuralnet(form,
                 data = mbd,
                 #Un vector de enteros que especifica el número de neuronas ocultas (vértices) en cada capa.
                 hidden = 2,
                 #función diferenciable que se utiliza para el cálculo del error.ce =la entropía cruzada
                 err.fct = "ce",
                 #si se debe aplicar función diferenciable que se utiliza para suavizar el resultado.
                 linear.output= FALSE)
??dummyVars
