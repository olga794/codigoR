library(caret)
#Base2017$RDOCULTIVO <- as.factor(Base2017$RDOCULTIVO)
#levels(Base2017$RDOCULTIVO)
############################### EDAD A NUMERICO
Base2017$EDAD <- as.character(Base2017$EDAD)
Base2017$EDAD <- as.double(Base2017$EDAD)
c("SEXO")
################ CULTIVO  NUMERICO 
Base2017$RDOCULTIVO <- as.character(Base2017$RDOCULTIVO)
##### ajustar edades fuera de rango convertir a doble 
Base2017$RDOCULTIVO <- as.numeric(Base2017$RDOCULTIVO)

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

###################################################################################
#formula
## Una descripción simbólica del modelo a montar
#data
##un marco de datos que contiene las variables especificadas en formula.
#hidden
##Un vector de enteros que especifica el número de neuronas ocultas (vértices) en cada capa
#threshold
##Un valor numérico que especifica el umbral para las derivadas 
##parciales de la función de error como criterio de detención.
#stepmax
##Los pasos máximos para el entrenamiento de la red neuronal. 
##Alcanzar este máximo conduce a la interrupción del proceso de entrenamiento de la red neuronal.
#rep
##El número de repeticiones para el entrenamiento de la red neuronal.
#startweights
##Un vector que contiene valores iniciales para los pesos. 
##Establecer en NULLinicialización aleatoria.
#learningrate.limit
## un vector o una lista que contiene el límite más bajo y más alto para la tasa de aprendizaje.
##Utilizado solo para RPROP y GRPROP.
#learningrate.factor
##un vector o una lista que contiene los factores de multiplicación para la tasa de aprendizaje superior e inferior. 
##Utilizado solo para RPROP y GRPROP.
#learningrate
##Un valor numérico que especifica la tasa de aprendizaje utilizada por la retropropagación tradicional. 
##Usado solo para propagación hacia atrás tradicional.
#lifesign
##una cadena que especifica cuánto se imprimirá la función durante el cálculo de la red neuronal. 
##'none', 'minimal' o 'full'.
#lifesign.step
##un entero que especifica el tamaño de pasos para imprimir el umbral mínimo en modo de vida útil completa.
#algorithm
##Una cadena que contiene el tipo de algoritmo para calcular la red neuronal. 
#Son posibles los siguientes tipos: 
##'backprop' -- se refiere a la retropropagación
## 'rprop +' --  se refieren a la retropropagación resistente con y sin retroceso de peso
##'rprop-'   --  ""
##'sag' o 'slr'. -- inducen el uso del algoritmo global convergente modificado (grprop)
#########################
##El algoritmo globalmente convergente se basa en la propagación hacia atrás resistente
##sin retroceso de peso y, además, modifica una tasa de aprendizaje, ya sea la tasa de aprendizaje
##asociada con el gradiente absoluto más pequeño (hundimiento) o la tasa de aprendizaje más pequeña (SLR).
## Las tasas de aprendizaje en el algoritmo grprop están limitadas a los límites definidos en learningrate.limit.
######################
#err.fct
##Una función diferenciable que se utiliza para el cálculo del error. 
##Alternativamente, se pueden usar las cadenas 'sse' y 'ce' que representan la suma de los errores al cuadrado y la entropía cruzada.
#act.fct
##Una función diferenciable que se utiliza para suavizar el resultado del producto cruzado de la covariable o las neuronas y los pesos. 
##Además, las cadenas, 'logistic' y 'tanh' son posibles para la función logística e hiperbólica tangente.
#linear.output
##lógico. Si act.fct no se debe aplicar a las neuronas de salida, 
##establezca la salida lineal en VERDADERO, de lo contrario en FALSO.
#exclude
##un vector o una matriz que especifica los pesos, que están excluidos del cálculo. 
##Si se da como un vector, se deben conocer las posiciones exactas de los pesos. 
##Una matriz con n filas y 3 columnas excluirá n pesos, donde la primera columna representa la capa, 
##la segunda columna para la neurona de entrada y la tercera columna para la neurona de salida del peso.
#constant.weights
##un vector que especifica los valores de los pesos que se excluyen del proceso 
##de entrenamiento y se tratan como arreglos.
#likelihood
##lógico. Si la función de error es igual a la función de probabilidad de registro negativa, 
##se calcularán los criterios de información AIC y BIC. Además, el uso de trust.interval es significativo.

##############################################################################
#################################################################################
when <- data.frame(time = c("afternoon", "night", "afternoon",
                            "morning", "morning", "morning",
                            "morning", "afternoon", "afternoon"),
                   day = c("Mon", "Mon", "Mon",
                           "Wed", "Wed", "Fri",
                           "Sat", "Sat", "Fri"))

levels(when$time) <- list(morning="morning",
                          afternoon="afternoon",
                          night="night")
levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                         Fri="Fri", Sat="Sat", Sun="Sun")

## Default behavior:
model.matrix(~day, when)

mainEffects <- dummyVars(~ day + time, data = when)
mainEffects
predict(mainEffects, when[1:3,])

when2 <- when
when2[1, 1] <- NA
predict(mainEffects, when2[1:3,])
predict(mainEffects, when2[1:3,], na.action = na.omit)


interactionModel <- dummyVars(~ day + time + day:time,
                              data = when,
                              sep = ".")
predict(interactionModel, when[1:3,])

noNames <- dummyVars(~ day + time + day:time,
                     data = when,
                     levelsOnly = TRUE)
predict(noNames, when)

head(class2ind(iris$Species))

two_levels <- factor(rep(letters[1:2], each = 5))
class2ind(two_levels)
class2ind(two_levels, drop2nd = TRUE)


###ESCALAR LOS DATOS NUMERICOS


max = apply(Base2017 , 2 , max)
min = apply(Base2017, 2 , min)
BaseScaled = as.data.frame(scale(Base2017$CONTACTOS, center = min, scale = max - min))

################################################## AJUSTAR NOMBRE DE FACTOR
Base2017 <- Base2017 %>%
  mutate(ETNIAT = case_when(ETNIA == "NEGRO,MULATO,AFROCOLOMBIANO" ~ "NEGRO/MULATO/AFROCOLOMBIANO",
                            TRUE ~ ETNIA   ))
Base2017$ETNIA <- NULL

################### NORMALIZAR VAOLRES CUALITATIVOS 
## formula con nombres a normalizar
nom = names(Base2017)
formNormaC = as.formula(paste(" ~ -1 + ", paste(nom, collapse = " + ")))
# se normaliza la base de datos con la matris 

Base2017NormaC <- model.matrix( formNormaC, data = Base2017 )
attributes(Base2017NormaC)
#  se combienta a framae   y se usa para una prueba de la red , sale ok 
base  <- as.data.frame(model.matrix( formNormaC, data = Base2017 ))


#### prueba
nom = names(base)
formp = as.formula(paste("RDOCULTIVO ~", paste(nom[!nom %in% "RDOCULTIVO"], collapse = " + ")))
nnUno1 <- neuralnet(formp,
                    data = base,
                    #Un vector de enteros que especifica el número de neuronas ocultas (vértices) en cada capa.
                    hidden = 2,
                    #función diferenciable que se utiliza para el cálculo del error.ce =la entropía cruzada
                    err.fct = "ce",
                    #si se debe aplicar función diferenciable que se utiliza para suavizar el resultado.
                    linear.output= FALSE)
plot(nnUno1)
##################################  AQUI VAMOS  sali la prueba bien  ,, se continuan  con los datos nuemricos  y dividir los datos dos grupos 

################### NORMALIZAR DATOS NUMERICOS

###ESCALAR LOS DATOS NUMERICOS
BNN <- as.data.frame(scale(Base2017[5:8]))
Base2017 <- Base2017 %>%  mutate(CONTACTOS = scale(Base2017[5]))

max = apply(Base2017 , 2 , max)
min = apply(Base2017, 2 , min)
BaseScaled = as.data.frame(scale(Base2017$CONTACTOS, center = min, scale = max - min))
#####
#### prueba
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



##################################################################

base <- as.data.frame(model.matrix(formNormaC, data = Base2017 ))

base3 <- model.matrix(formNormaC, data = Base2017, contrasts = list(GRUPOP = "contr.sum") )

base9 <- model.matrix(formNormaC, data = Base2017,  contrasts.ar =  list(GRUPOP =contrasts(Base2017$GRUPOP, contrasts=F) ))
attributes(base9)
attributes(base3)
attributes(base)
lapply(Base2017$GRUPOP, contrasts, contrasts = FALSE)
base4 <- model.matrix(formNormaC, data = Base2017, 
                      contrasts.arg = lapply(Base2017, is.factor, contrasts , contrasts = F))

contrasts(Base2017$GRUPOP)
options(contrasts=c("contr.sum","contr.poly"))

Nivelgp <- levels(Base2017$GRUPOP)
contr.sum(Nivelgp)
contr.treatment(Nivelgp)
contr.poly(Nivelgp)
contr.helmert(Nivelgp)


#######
Base2017NormaC <- model.matrix( formNormaC, data = Base2017 )
head(Base2017NormaC)
str(Base2017NormaC)
summary(Base2017NormaC)
names(Base2017NormaC)


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
??model.matrix








############################################################################################
################### EJEMPLO  ( CAPA  DE  NEURONAS )  

################### MOSTRAR 
plot(nnUno1)
############################################################################################


##################################  
############################################################################################
################### EJEMPLO  ( CAPA  DE  NEURONAS )  

################### MOSTRAR 
plot(nnUno1)
############################################################################################


datarealpred2 =cbind.data.frame(nuevop,nuevor)
confusionMatrix(datarealpred2)
CFM=  confusionMatrix(nuevor, nuevop)
print(CFM,mode = "everything",
      digits = max(3, getOption("digits") - 3),printStats = TRUE)
CFM$overall
CFM$positive
CFM$byClass
dataMETRI = data.frame(CFM$overall[1],CFM$byClass[2],CFM$byClass[4],CFM$byClass[3],CFM$byClass[7])

install.packages('e1071', dependencies=TRUE)
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
accuracy <- round((tp+tn)/total,digits = 3)
precision <- tp/(tp+fp)
sensitivity <- tp/(tp+fn)
especificity <- tn/(tn+fp)
F1 <- (2*precision*sensitivity)/(precision+sensitivity)


medidaspk = data.frame(accuracy,precision,sensitivity,especificity,F1)

??confusionMatrix
error_rate <- (fp+fn)/total



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
print(paste("Especificity : ", specificity(nuevor,nuevop)))
print(paste("RMSE     : ", rmse(RDOCULTIVO.real,RDOCULTIVO.predict)))







