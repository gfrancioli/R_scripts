# separo da pasta principal os arquivos que nao estao zerados
setwd("~/Embrapa/dados_precip/estacoes_all")
input <- dir('estacoes_all/', pattern = '.csv', full.names = TRUE)
tam <- length(input)
dados < -NULL
dados2 < -NULL
aux <- NULL
for (i in 1:tam) {
  dados<- read.csv(input[i])
  if(max(dados$valor) == 0){
    next
  }else{
    dados2<-c(dados2,i)
    aux<- rbind(aux,dados)
  }
}

