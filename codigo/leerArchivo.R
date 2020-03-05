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

############################################### CAMBIAR LOS TIPOS DE DATOS Y AJUSTAR NOMBRES  FACTORES
##########CAMBIO DE CH A FACTOR 

Base2017$SEXO <- as.factor(Base2017$SEXO)
levels(Base2017$SEXO)
Base2017$ETNIA <- as.factor(Base2017$ETNIA)
levels(Base2017$ETNIA)# se verifican los niveles existentes  y se cambian por unos mas apropiados
levels(Base2017$ETNIA) <- c("Indigena", "NeMuAfro","Otro") 
levels(Base2017$ETNIA)
Base2017$GRUPOP <- as.factor(Base2017$GRUPOP)
levels(Base2017$GRUPOP)
levels(Base2017$GRUPOP) <- c("Desplazado", "FarmacoDPTE" , "Gestante" , "HabitanteCalle" , "MadresComun", "Migrante" , "Ninguno", "PersonaDisc","PoblacionCarcel" , "TrabajadorSalud") 
levels(Base2017$GRUPOP)
Base2017$COOMORBILIDAD <- as.factor(Base2017$COOMORBILIDAD)
levels(Base2017$COOMORBILIDAD)
levels(Base2017$COOMORBILIDAD)<- c("Diabetes", "EnfermedadRenal" , "Malnutricion" , "Ninguna" , "NingunaCoom" , "OtrasInmunosupre" ,  "Silicosis" , "VIH_SIDA")
levels(Base2017$COOMORBILIDAD)
Base2017$VIHCONFIR <- as.factor(Base2017$VIHCONFIR)
levels(Base2017$VIHCONFIR)
levels(Base2017$VIHCONFIR)<- c("Neg", "NoRealizado", "Pos", "UsuarioNoAcepta" , "VIHPos_PREVIO")
levels(Base2017$VIHCONFIR)
Base2017$RECIBETAR <- as.factor(Base2017$RECIBETAR)
levels(Base2017$RECIBETAR)
levels(Base2017$RECIBETAR)<- c("No" , "NoAplica", "Si")  
levels(Base2017$RECIBETAR)
Base2017$RECIBETRIME <- as.factor(Base2017$RECIBETRIME)
levels(Base2017$RECIBETRIME)
levels(Base2017$RECIBETRIME)<- c("No" , "NoAplica", "Si")  
levels(Base2017$RECIBETRIME)
Base2017$RDOBKDX <- as.factor(Base2017$RDOBKDX)
levels(Base2017$RDOBKDX)
levels(Base2017$RDOBKDX)<- c("Neg", "Pos" ,  "PosD" , "Pos3" ,"NoRealizado", "PosUnoANueveBAAR")
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
## Tabla de frecuencias 
#table(Base2017$RDOCULTIVO)
## Tabla de frecuencias EN PORCENTAJE
#prop.table(table(Base2017$RDOCULTIVO)) %>% round(digits = 2)
## Tabla de frecuencias relativas de RESULTADO DE CULTIVO por SEXO
#prop.table(table(Base2017$SEXO,Base2017$RDOCULTIVO), margin = 1) %>% round(digits = 2)
## Tabla de frecuencias relativas de RESULTADO DE CULTIVO por CONTACTOS
#prop.table(table(Base2017$CONTACTOS, Base2017$RDOCULTIVO), margin = 1) %>% round(digits = 2)
## Tabla de frecuencias relativas de RESULTADO DE CULTIVO por EDAD
#prop.table(table(Base2017$EDAD, Base2017$RDOCULTIVO), margin = 1) %>% round(digits = 2)
#Base2017$EDAD

str(Base2017)
names(Base2017)
####################################################### NORMALIZAR VAOLRES CUALITATIVOS 
## formula con nombres a normalizar
nom = names(Base2017)
formNormaC = as.formula(paste(" ~ -1 + ", paste(nom, collapse = " + ")))
# se normaliza la base de datos 
base2017DN <- as.data.frame(model.matrix( formNormaC, data = Base2017 ))

str(base2017DN)
names(base2017DN)
summary(base2017DN)
###################################################### NORMALIZAR DATOS NUMERICOS
max = apply(base2017DN , 2 , max)
min = apply(base2017DN, 2 , min)
base2017scaled = as.data.frame(scale(base2017DN, center = min, scale = max - min))
str(base2017scaled)
names(base2017scaled)
summary(base2017scaled)

##################################################################################################
########### RED NEURONAL #########################################################################
##################################################################################################
##################################################################################################
##########################CONSTRUIR LA MATRIZ DE INDICE PARA PRUEBAS Y ENTRENAMIENTO 
nrow(base2017scaled) ## 602 #39
random = round(0.1 * nrow(base2017scaled), digits = 0) ##60
total_index = 1:nrow(base2017scaled)
index_test = sample(total_index, size = random)
index_train = setdiff(total_index,index_test)
############################################################# SEPARAR LOS DOS GRUPOS DE  DATOS
B2017trainNN = base2017scaled[index_train,]
B2017testNN =  base2017scaled[index_test,]
dim(B2017trainNN)
dim(B2017testNN)
#############################################################################################
######################################### CONSTRUIR LA FORMULA 
nom = names(B2017trainNN)
formNN = as.formula(paste("RDOCULTIVO ~", paste(nom[!nom %in% "RDOCULTIVO"], collapse = " + ")))
##############################################################################################
########################################  RED NEURONAL
##############################################################################################
################### SENCILLA   (1 CAPA 2 NEURONAS )
nnUno <- neuralnet(formNN,
                    data = B2017trainNN,
                    #Un vector de enteros que especifica el número de neuronas ocultas (vértices) en cada capa.
                    hidden = 2,
                    #función diferenciable que se utiliza para el cálculo del error.ce =la entropía cruzada
                    err.fct = "ce",
                    #si se debe aplicar función diferenciable que se utiliza para suavizar el resultado.
                    linear.output= FALSE)
################### MOSTRAR 
plot(nnUno)

columnastestNN = ncol(B2017testNN)
names(B2017trainNN)
names(B2017testNN)
ultimo =columnastestNN-1
red = nnUno
predict_testNN = compute(red, B2017testNN[,c(1:ultimo)]) 
RDOCULTIVO.predict  = predict_testNN$net.result*(max(base2017scaled$RDOCULTIVO)-min(base2017scaled$RDOCULTIVO))+min(base2017scaled$RDOCULTIVO)
RDOCULTIVO.real = B2017testNN$RDOCULTIVO*(max(base2017scaled$RDOCULTIVO)-min(base2017scaled$RDOCULTIVO))+min(base2017scaled$RDOCULTIVO)

RDOCULTIVO.predict = ifelse(RDOCULTIVO.predict > 0.5, 1, 0)
datarealpred =cbind.data.frame(RDOCULTIVO.predict,RDOCULTIVO.real)

print("prediccion contra real:")   
print(datarealpred)        

positive <- sum(datarealpred$RDOCULTIVO.real == 1)
negative <- sum(datarealpred$RDOCULTIVO.real == 0)
predicted_positive <- sum(datarealpred$RDOCULTIVO.predict == 1)
predicted_negative <- sum(datarealpred$RDOCULTIVO.predict == 0)
total <- nrow(datarealpred)
matrizConfusionTotal = data.frame(positive, negative,predicted_positive,predicted_negative)

tp<-sum(datarealpred$RDOCULTIVO.real == 1 & datarealpred$RDOCULTIVO.predict == 1)
tn<-sum(datarealpred$RDOCULTIVO.real == 0 & datarealpred$RDOCULTIVO.predict == 0)
fp<-sum(datarealpred$RDOCULTIVO.real == 0 & datarealpred$RDOCULTIVO.predict == 1)
fn<-sum(datarealpred$RDOCULTIVO.real == 1 & datarealpred$RDOCULTIVO.predict == 0)
matrizConfusionInterna = data.frame(tp,tn,fp,fn)
accuracy <- (tp+tn)/total
error_rate <- (fp+fn)/total
sensitivity <- tp/positive
especificity <- tn/negative
precision <- tp/predicted_positive
npv <- tn / predicted_negative
medidas = data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)



r_rmse = round(sqrt(mean((datarealpred$RDOCULTIVO.predict-datarealpred$RDOCULTIVO.real)^2, na.rm = FALSE)),digits = 3)
r_r = round(cor(datarealpred$RDOCULTIVO.predict,datarealpred$RDOCULTIVO.real),digits = 3)
r_r2 = round(r_r^2,digits = 3)



print("puntuaciones and metricas:")
print(paste(r_rmse,r_r,r_r2))

print(paste("Accuracy : ", accuracy(RDOCULTIVO.real, RDOCULTIVO.predict))) 
print(paste("AUC      : ", auc(RDOCULTIVO.real, RDOCULTIVO.predict))) ## area bajo la curva 
print(paste("Precision: ", precision(RDOCULTIVO.real, RDOCULTIVO.predict)))
print(paste("Recall   : ", recall(RDOCULTIVO.real,RDOCULTIVO.predict)))
print(paste("F1       : ", f1(RDOCULTIVO.real, RDOCULTIVO.predict)))
print(paste("RMSE     : ", rmse(RDOCULTIVO.real,RDOCULTIVO.predict)))

pbar$step()
??auc

real <- c (1, 1, 1, 0, 0, 0)
predicho <- c (0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
prueba= auc(real, predicho)
############################################################################################
################### EJEMPLO 2  (2 CAPA  DE 3, 2 NEURONAS )  
nnDos = neuralnet(formNN,
                  data          = B2017trainNN,
                  hidden        = c(3,2),   # numero de neuronas ocultas 
                  threshold     = 0.03,     #
                  algorithm     = "rprop+", # 
                  rep=3                    )#
################### MOSTRAR 
plot(nnDos)
############################################################################################
################### EJEMPLO4   ( CAPA  DE  NEURONAS )  
nnTres = neuralnet(formNN,
                  data          = B2017trainNN,
                  hidden        = c(15,7),   # numero de neuronas ocultas 
                  threshold     = 0.03,     #
                  algorithm     = "rprop+", # 
                  act.fct       = "logistic",
                  rep=3                    )#
################### MOSTRAR 
plot(nnTres)
############################################################################################
############################################################################################
################### EJEMPLO 4  ( CAPA  DE  NEURONAS )  

################### MOSTRAR 
plot(nnUno1)
############################################################################################

