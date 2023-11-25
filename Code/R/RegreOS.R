# Regression

library(readxl)
library(skedastic)

setwd("C:/Users/User/Downloads")
data <- read_excel("data.xlsx")
# data1 <- read_excel("data.xlsx")
# --------------------

total <- nrow(data)
insample <- 70
forecasts <- total - insample - 1

resultados <- matrix(0,nrow = forecasts,ncol = 16)
j<-3
for(h in 1:14){
for(i in 1:forecasts){
regressao <- lm(data$prem[2:(i+1+insample-1)]~as.matrix(data[1:(i+insample-1),j]))
forecast <- as.numeric(regressao[["coefficients"]][1]) + as.numeric(regressao[["coefficients"]][1])*as.numeric(data[i+1+insample-1,j])
real <- data$prem[(i+2+insample-1)]
resultados[i,h+1] <- forecast
resultados[i,1] <- real
}
  j <- j+1
}
resultados[,16] <- data$HA[(insample+1):(total-1)]

rquadrados = matrix(0,nrow = 14,ncol = 4)
for(i in 1:14){
  
  rquadrados[i,1]= 1-(sum((((resultados[,1]-resultados[,i+1])^2)))/sum(((resultados[,1]-resultados[,16])^2)))
  rquadrados[i,2]= 1-(sum((((resultados[,1]-resultados[,i+1])^2)*data$Fear[(insample+1):276]))/sum(((resultados[,1]-resultados[,16])^2)*data$Fear[(insample+1):276]))
  rquadrados[i,3]= 1-(sum((((resultados[,1]-resultados[,i+1])^2)*data$Greed[(insample+1):276]))/sum(((resultados[,1]-resultados[,16])^2)*data$Greed[(insample+1):276]))
  rquadrados[i,4]= 1-(sum((((resultados[,1]-resultados[,i+1])^2)*data$Neutral[(insample+1):276]))/sum(((resultados[,1]-resultados[,16])^2)*data$Neutral[(insample+1):276]))
  
}
rquadrados = rquadrados*100

write.csv(rquadrados, 'r2_OS_normal.csv')
