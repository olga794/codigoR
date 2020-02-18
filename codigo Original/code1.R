
library(neuralnet)
library(ggplot2)

setwd("C:/Users/oscar.bedoya/Desktop/Univalle/18-02/NeuralNetworks")
data = read.table("data.csv", header = T, sep=" ")
data = data[1:1000,c(2:8)]
#data = data[,c(2:8)]
dim(data)

random = round(0.1 * nrow(data), digits = 0)
total_index = 1:nrow(data)
index_test = sample(total_index, size = random)
index_train = setdiff(total_index,index_test)

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

summary(data)
summary(scaled)
  
trainNN = scaled[index_train, ]
testNN = scaled[index_test, ]
dim(trainNN)
dim(testNN)
n = names(trainNN)
f = as.formula(paste("ideam ~", paste(n[!n %in% "ideam"], collapse = " + ")))
   
NN = neuralnet(f,
                  data          = trainNN,
                  hidden        = c(3,2),
                  threshold     = 0.03,  
                  algorithm     = "rprop+",
                  rep=3 
)
plot(NN)
  
predict_testNN = compute(NN, testNN[,c(1:6)])    
ideam.predict  = (predict_testNN$net.result * (max(data$ideam) - min(data$ideam))) + min(data$ideam)
ideam.real = (testNN$ideam * (max(data$ideam) - min(data$ideam))) + min(data$ideam)
  
datarealpred =cbind.data.frame(ideam.predict,ideam.real)
    
r_rmse = round(sqrt(mean((datarealpred$ideam.predict-datarealpred$ideam.real)^2, na.rm = FALSE)),digits = 3)
r_r = round(cor(datarealpred$ideam.predict,datarealpred$ideam.real),digits = 3)
r_r2 = round(r_r^2,digits = 3)

print(paste(r_rmse,r_r,r_r2))
  
result_exp=rbind.data.frame(result_exp,cbind.data.frame(r_rmse,r_r,r_r2))
  
plot(datarealpred)
fit <- lm(datarealpred$ideam.real ~ datarealpred$ideam.predict) 
abline(fit)          
