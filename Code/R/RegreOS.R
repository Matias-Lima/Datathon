
# Regression

library(readxl)
library(skedastic)

setwd("C:/Users/Leandro/Desktop/Livre-Docente/Codigos/sp500analysis")
sp_tech <- read_excel("sp_tech.xlsx")

# --------------------

total <- nrow(sp_tech)
insample <- 181 # until dec 1965 like Neely
forecasts <- total - insample - 1

resultados <- matrix(0,nrow = forecasts,ncol = 16)
j<-7
for(h in 1:14){
for(i in 1:forecasts){
regressao <- lm(sp_tech$premium[2:(i+1+insample-1)]~as.matrix(sp_tech[1:(i+insample-1),j]))
forecast <- as.numeric(regressao[["coefficients"]][1]) + as.numeric(regressao[["coefficients"]][1])*as.numeric(sp_tech[i+1+insample-1,j])
real <- sp_tech$premium[(i+2+insample-1)]
resultados[i,h+1] <- forecast
resultados[i,1] <- real
}
  j <- j+1
}
resultados[,16] <- sp_tech$rHA[(insample+2):total]



