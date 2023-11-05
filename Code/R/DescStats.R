install.packages("readxl")
install.packages("skedastic")
install.packages("moments")



# Regression

library(readxl)
library(skedastic)
library(moments)

# setwd("C:/Users/Leandro/Desktop/Livre-Docente/Codigos/sp500analysis")
sp_tech <- read_excel("C:/Users/limam/Documents/GitHub/Datathon_2023/Dados/sp.xlsx")

# --------------------

resultados <- matrix(0,nrow = 15,ncol = 6)

for(i in 1:15){
  resultados[i,1] <- mean(as.matrix(sp_tech[,i+5]))
  resultados[i,2] <- max(as.matrix(sp_tech[,i+5]))
  resultados[i,3] <- min(as.matrix(sp_tech[,i+5]))
  resultados[i,4] <- sd(as.matrix(sp_tech[,i+5]))
  resultados[i,5] <- skewness(as.matrix(sp_tech[,i+5]))
  resultados[i,6] <- kurtosis(as.matrix(sp_tech[,i+5]))
}
