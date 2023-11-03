
# Regression

library(readxl)
library(skedastic)

setwd("C:/Users/Leandro/Desktop/Livre-Docente/Codigos/sp500analysis")
sp_tech <- read_excel("sp_tech.xlsx")

# --------------------

resultados <- matrix(0,nrow = 14,ncol = 4)
t <- nrow(sp_tech)
n <- 14 # number of technical indicators
j <- 7
residuos <- matrix(0,nrow = t-1,ncol = 14)

for(i in 1:14){
  regressao <- lm(sp_tech$premium[2:t]~as.matrix(sp_tech[1:(t-1),j]))
  j = j + 1
  mybootlm <- bootlm(regressao,sampmethod = "wild")
  a <- matrix(0,nrow = 1000,ncol = 2)
    for(t in 1:1000){
      a[t,1:2]<-mybootlm[[t]][["beta.hat"]]
    }
  coef <- as.numeric(regressao[["coefficients"]][2])
  stat <- coef/sd(a[,2]) 
  pvalor <- dt(stat,n-2)
  r2 <- summary(regressao)[["r.squared"]]
  
  resultados[i,] <- c(coef,stat,pvalor,r2)
  residuos[,i] <- regressao[["residuals"]]
  rm(regressao);rm(r2);rm(pvalor);rm(stat);rm(coef)
  
  
}
rm(i);rm(j);rm(a);rm(mybootlm);rm(t);rm(n)

# --------------------------------------------------