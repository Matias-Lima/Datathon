
# ------------------------------------------------------------------------------

# Technical indicators series construction code

# ------------------------------------------------------------------------------

# Load price and volume data...

setwd("C:/Users/Leandro/Desktop/Livre-Docente/Codigos/sp500analysis")

library(readxl)
dados <- read_excel("sp.xlsx")
precos <- as.matrix(dados$price)
vol <- as.matrix(dados$volume)
N <- nrow(precos)

# Compute MA signals...
# --------------------------------------

s <- c(1,2,3)
l <- c(9,12)

MA <- matrix(0,nrow = N,ncol = length(s)*length(l))
h <- 1
for(i in 1:length(s)){
  for(j in 1:length(l)){
    for(t in max(l):N){
      if(mean(precos[(t-s[i]+1):t,1]) >= mean(precos[(t-l[j]+1):t,1])){
        MA[t,h] <- 1
      }
    }
    h <- h + 1
  }
}
rm(h);rm(i);rm(j);rm(l);rm(s);rm(t)


# Compute MOM signals...
# --------------------------------------

m <- c(9,12)

MOM = matrix(0,nrow = N,ncol = length(m))
h = 1
for(i in 1:length(m)){
  for(t in max(m):N){
    if(precos[t,1] >= precos[t-m[i]+1,1]){
      MOM[t,h] <- 1
    }
  }
  h <- h + 1
}
rm(h);rm(i);rm(t);rm(m)


# Compute VOL signals...
# --------------------------------------

s <- c(1,2,3)
l <- c(9,12)

obv <- matrix(0,nrow = N,ncol = 1)
for(i in 2:N){
  if((precos[i,1]-precos[i-1,1])>=0){
    d = 1
  } else {
    d = -1
  }
  obv[i,1] <- obv[i-1,1] + vol[i,1]*d
  rm(d)
}
rm(i)

VOL <- matrix(0,nrow = N,ncol = length(s)*length(l))
h <- 1
for(i in 1:length(s)){
  for(j in 1:length(l)){
    for(t in max(l):N){
      if(mean(obv[(t-s[i]+1):t,1]) >= mean(obv[(t-l[j]+1):t,1])){
        VOL[t,h] <- 1
      }
    }
    h <- h + 1
  }
}
rm(h);rm(i);rm(j);rm(l);rm(s);rm(t)

rm(precos);rm(vol);rm(obv)

dados[,4:9] <- MA
dados[,10:11] <- MOM
dados[,12:17] <- VOL

rm(MA);rm(MOM);rm(VOL)

writexl::write_xlsx(dados[13:N,],"Tech.xlsx") # first 12 observations are discarded 

# ------------------------------------------------------------------------------

