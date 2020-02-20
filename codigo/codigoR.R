# LIBRERIAS NECESARIAS
##################################################################
library(reader)
library(readxl)
library(neuralnet)
library(ggplot2)

# LECTURA DE DATOS 
##################################################################
## saber el directorio -- getwd()
setwd("/cloud/project/codigo")

datasets <- read_excel("Base2017.xlsx", sheet = "Hoja 1")
View(datasets)

readxl_example()


##datasets2 <- readxl_example("Base2017.xlsx")
read_excel(datasets)
# Specify sheet either by position or by name
read_excel(datasets, 2)
##misDatos <- read_excel(file.choose(), sheet = 1)

XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=2, rep=5))
plot(net.xor, rep="best")

#
Var1 <- rpois(100,0.5)
Var2 <- rbinom(100,2,0.6)
Var3 <- rbinom(100,1,0.5)
SUM <- as.integer(abs(Var1+Var2+Var3+(rnorm(100))))
sum.data <- data.frame(Var1,Var2,Var3, SUM)
print(net.sum <- neuralnet( SUM~Var1+Var2+Var3, sum.data, hidden=1,
                            act.fct="tanh"))
main <- glm(SUM~Var1+Var2+Var3, sum.data, family=poisson())
full <- glm(SUM~Var1*Var2*Var3, sum.data, family=poisson())
prediction(net.sum, list.glm=list(main=main, full=full))