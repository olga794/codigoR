library(plotrix) #taylor


require(e1071)
require(Metrics)

##################################################################
##################################################################
##################################################################
# PRECIPITACIÓN
##################################################################
##################################################################
##################################################################

setwd("C:/Users/WILMER/Google Drive/Magister/tesis/Resultados/PREC/rcp")

data = data.frame()
ARRR = list.files()

for(e in ARRR[600]){
  
  t = read.table(e, header = TRUE)
  #t= rbind.data.frame(t,read.table(list.files(pattern = "_400@") , header = TRUE))
  head(t)
  
  Año = as.numeric(format(as.Date(t$fecha),"%Y"))
  Mes = as.numeric(format(as.Date(t$fecha),"%m"))
  t = cbind.data.frame(Año,Mes,t)
  #head(t)
  
  nt = t[,c("Año","Mes","prec","rcp45.psl","rcp45.hur_500",
           "rcp45.hur_850","rcp45.zg_500","rcp45.zg_850","rcp45.pr")]

  #head(nt)
  
  names(nt)= c("Año","Mes","Precipitación","psl","hur_500","hur_850","zg_500","zg_850","prec")
  
  #nt = t[,c("Año","Mes","prec","rcp26.pr")]
  #names(nt)= c("Año","Mes","Precipitación","prec")
  
  max = apply(nt , 2 , max, na.rm=TRUE)
  min = apply(nt, 2 , min, na.rm=TRUE)
  nt = na.omit(nt)
  
  summary(nt)
  
  data = rbind.data.frame(nt)
  
  #head(data)
  
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
  
  #summary(RandList10)
  
  ##################################################################
  ##################################################################
  ##################################################################
  # 10 EXPERIMENTOS PARA LA VALIDACIÓN CRUZADA (10% DE LOS DATOS PARA VALIDACION Y 90% PARA ENTRENAMIENTO)
  ##################################################################
  ##################################################################
  ##################################################################
  
  #head(data)
  #plot(data[,6:7])
  
  
  scaled = as.data.frame(scale(data, center = min, scale = max - min))
  
  summary(scaled)
  
  result_exp = data.frame()

  
  kernel = list("linear","polynomial","radial","sigmoid")
  
  for(aq in 1:length(kernel)){
  
  c=0
  data___ = data.frame()
  for(exp in 1:10){
    #exp=1
    # CREAR DATASET
    cP_RandList10 = RandList10
    
    index_test = cP_RandList10[[exp]]
    
    cP_RandList10[[exp]]=NULL
    index_train = unlist(cP_RandList10)
    
    
    trainNN = scaled[index_train, ]
    testNN = scaled[index_test, ]
    
    
    n = names(trainNN)
    f = as.formula(paste("Precipitación ~", paste(n[!n %in% "Precipitación"], collapse = " + ")))
    
    
    

    modelo <- svm(f,
                  data= trainNN,
                  kernel= kernel[[aq]]
    )
    
    predict_testNN = predict(modelo, testNN[,-3])
    
    
    precip_predict  = (predict_testNN * (max[3] - min[3])) + min[3]
    precip_real = (testNN$Precipitación * (max[3] - min[3])) + min[3]
    

    
    
    MAE = round(mae(precip_real,precip_predict),digits = 3)
    MAPE = round(mape(precip_real,precip_predict),digits = 3)
    MSE = round(mse(precip_real,precip_predict),digits = 0)
    RMSE = round(rmse(precip_real,precip_predict),digits = 3)
    R = round(cor(precip_real,precip_predict),digits = 3)
    R2 = round((R*R),digits = 3)
    arquitectura = paste0(arqu[[aq]],collapse = "_")
    
    cerror = cbind.data.frame(id_=gsub(".csv","",e),arquitectura,exp,MAE,MAPE,MSE,RMSE,R,R2)
    
    data___= rbind.data.frame(data___,cerror)
    
    print(cerror)
    
    if(c==0){
      taylor.diagram(precip_real,precip_real, xlab="Desviación Estándar",
         ylab="Desviación Estándar",main = "Sigmoid, Precipitación")
      taylor.diagram(precip_real,precip_predict,add=TRUE,col="blue",pch = 20)
      
    }else{

      taylor.diagram(precip_real,precip_predict,add=TRUE,col="blue",pch = 20) #500x500
    }
    c=c+1
    
    
  }
  
 
  #write.table(data___[,c(-1,-2)],"clipboard",row.names = FALSE, col.names = FALSE, sep = "\t",dec = ",")
}
  
}


#legend(90,120,legend=c("Real","Predicción"),pch=19,col=c("red","blue"),box.lty=0)

























##################################################################
##################################################################
##################################################################
# TEMPERATURA
##################################################################
##################################################################
##################################################################

setwd("C:/Users/WILMER/Google Drive/Magister/tesis/Resultados/TEMP/rcp")

data = data.frame()
ARRR = list.files()

for(e in ARRR[200]){
  
  
  t = read.table(e, header = TRUE)
  #t= rbind.data.frame(t,read.table(list.files(pattern = "_400@") , header = TRUE))
  head(t)
  
  Año = as.numeric(format(as.Date(t$fecha),"%Y"))
  Mes = as.numeric(format(as.Date(t$fecha),"%m"))
  t = cbind.data.frame(Año,Mes,t)
  head(t)
  
  nt = t[,c("Año","Mes","temp","rcp45.psl","rcp45.hur_500",
            "rcp45.hur_850","rcp45.zg_500","rcp45.zg_850","rcp45.tas")]
  
  
  
  #nt = t[,c("Año","Mes","prec","rcp26.pr")]
  
  max = apply(nt , 2 , max, na.rm=TRUE)
  min = apply(nt, 2 , min, na.rm=TRUE)
  nt = na.omit(nt)
  
  summary(nt)
  
  data = rbind.data.frame(nt)
  
  #head(data)
  
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
  
  #summary(RandList10)
  
  ##################################################################
  ##################################################################
  ##################################################################
  # 10 EXPERIMENTOS PARA LA VALIDACIÓN CRUZADA (10% DE LOS DATOS PARA VALIDACION Y 90% PARA ENTRENAMIENTO)
  ##################################################################
  ##################################################################
  ##################################################################
  
  #head(data)
  #plot(data[,6:7])
  
  
  scaled = as.data.frame(scale(data, center = min, scale = max - min))
  
  summary(scaled)
  
  result_exp = data.frame()
  

  
  kernel = list("linear","polynomial","radial","sigmoid")
  
  for(aq in 1:length(kernel)){
  
  c=0
  data___=data.frame()
  for(exp in 1:10){
    #exp=6
    # CREAR DATASET
    cP_RandList10 = RandList10
    
    index_test = cP_RandList10[[exp]]
    
    cP_RandList10[[exp]]=NULL
    index_train = unlist(cP_RandList10)
    
    
    trainNN = scaled[index_train, ]
    testNN = scaled[index_test, ]
    
    
    n = names(trainNN)
    f = as.formula(paste("temp ~", paste(n[!n %in% "temp"], collapse = " + ")))
    
    modelo <- svm(f,
                  data= trainNN,
                  kernel= kernel[[aq]]
    )
    
   
    predict_testNN = predict(modelo, testNN[,-3])
    
    
    precip_predict  = (predict_testNN * (max[3] - min[3])) + min[3]
    precip_real = (testNN$temp * (max[3] - min[3])) + min[3]
   
    MAE = round(mae(precip_real,precip_predict),digits = 3)
    MAPE = round(mape(precip_real,precip_predict),digits = 3)
    MSE = round(mse(precip_real,precip_predict),digits = 0)
    RMSE = round(rmse(precip_real,precip_predict),digits = 3)
    R = round(cor(precip_real,precip_predict),digits = 3)
    R2 = round((R*R),digits = 3)
    arquitectura = paste0(arqu[[aq]],collapse = "_")
    
    cerror = cbind.data.frame(id_=gsub(".csv","",e),arquitectura,exp,MAE,MAPE,MSE,RMSE,R,R2)
    
    print(cerror)
    data___= rbind.data.frame(data___,cerror)
    
    if(c==0){
      taylor.diagram(precip_real,precip_predict, xlab="Desviación Estándar",
                      ylab="Desviación Estándar",main = "Sigmoid,Temperatura",col="blue")
      taylor.diagram(precip_real,precip_real,add=TRUE,col="red",pch = 20)
    }else{
      taylor.diagram(precip_real,precip_predict,add=TRUE,col="blue",pch = 20) #500x500
    }
    c=c+1
    
    
  }
}
  
  #write.table(data___[,c(-1,-2)],"clipboard",row.names = FALSE, col.names = FALSE, sep = "\t",dec = ",")
  
  
  
  
}







#legend(90,120,legend=c("Real","Predicción"),pch=19,col=c("red","blue"),box.lty=0)




