
require(bnlearn)
require(Metrics)



##################################################################
##################################################################
##################################################################
# LECTURA DE DATOS 
##################################################################
##################################################################
##################################################################

data = read.table("F:/Downscaling/CHIRPS_GLOBAL_MENSUAL/chirps e ideam final/data.csv", header = T)

data= data[,c(2:8)]

head(data)


##################################################################
##################################################################
##################################################################
##################################################################
# CREAR GRUPOS ALEATORIOS DE LOS DATOS (10 GRUPOS)
##################################################################
##################################################################
##################################################################

rand10 =round(0.1 * nrow(data), digits = 0)

total_index =1:nrow(data)

RandList10 =list()
for(i in 1:10){
  if(i<10){
    index_10 = sample(total_index, size = rand10)
    RandList10[[i]] = index_10
    total_index = setdiff(total_index,index_10)
  }else{
    RandList10[[i]] = total_index
  }
}

summary(RandList10)

##################################################################
##################################################################
##################################################################
# 10 EXPERIMENTOS PARA LA VALIDACIÓN CRUZADA (10% DE LOS DATOS PARA VALIDACION Y 90% PARA ENTRENAMIENTO)
##################################################################
##################################################################
##################################################################

result_exp = data.frame()

data_10_real = list()
data_10_pred = list()
for(exp in 1:10){
  
  # CREAR DATASET
  cP_RandList10 = RandList10
  
  index_test = cP_RandList10[[exp]]
  
  cP_RandList10[[exp]]=NULL
  index_train = unlist(cP_RandList10)
  
  max = apply(data , 2 , max)
  min = apply(data, 2 , min)
  scaled = as.data.frame(scale(data, center = min, scale = max - min))
  
  trainNN = scaled[index_train, ]
  testNN = scaled[index_test, ]
  
  n = names(trainNN)

  time_ini = Sys.time()
  


  res = hc(trainNN)                 # learn BN structure on training set data 
  fitted = bn.fit(res, trainNN)     # learning of parameters
  #plot(res)
  
  predict_testNN = predict(fitted, "ideam", testNN)
  
  time_fin = Sys.time()
  
  
  ideam.predict  = (predict_testNN * (max(data$ideam) - min(data$ideam))) + min(data$ideam)
  ideam.real = (testNN$ideam * (max(data$ideam) - min(data$ideam))) + min(data$ideam)
  
  datarealpred =cbind.data.frame(ideam.predict,ideam.real)
  
  data_10_real[[exp]] = ideam.real
  data_10_pred[[exp]] = ideam.predict
  
  
  r_rmse = round(rmse(datarealpred$ideam.predict,datarealpred$ideam.real),digits = 3)
  r_mae = round(mae(datarealpred$ideam.predict,datarealpred$ideam.real),digits = 3)
  r_bias = round(bias(datarealpred$ideam.predict,datarealpred$ideam.real),digits = 3)
  
  r_r = round(cor(datarealpred$ideam.predict,datarealpred$ideam.real),digits = 3)
  r_r2 = round(r_r^2,digits = 3)
  
  result_exp=rbind.data.frame(result_exp,cbind.data.frame(r_rmse,r_r,r_r2))
  
  names(datarealpred) = c("Prediccion","Real")
  

  cat("Exp:",exp,",",r_rmse,",",r_mae,",",r_bias,",",r_r,",",r_r2,",",format(time_fin-time_ini,usetz = TRUE),"\n")
  
  
}









