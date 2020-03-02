
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

