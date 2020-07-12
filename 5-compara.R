library(stringr)
# A ideia aqui é fazer uma funcao so para as tres variaveis e evitar repeticao de codigo
# Encontrei alguns problemas em relacao aos dataframes, por conta da diferenca de linhas entres as variaveis
# Tive a ideia de igualar o numero das linhas do dataframe e preencher de 'NA' quando nao tinha mais valores
# Assim pude criar o dataframe com as colunas iguais. Não foi uma das melhores ideias, mas é funcional e simples de desfazer caso tenhamos outra saida

datamin <- NULL
data0406 <- NULL
data0608 <- NULL
datamax <- NULL
diario <- NULL
mensal <- NULL
decendial <- NULL

# Funcao que faz as comparacoes e separa cada estacao para um vetor desejado
# Ao final cria-se um dataframe com a juncao desses vetores
compara <- function(col){
  lista_r2 <- read.csv("Analises_R2.csv")
  
  # Sequencia de comparacoes 
  for (i in 1:nrow(lista_r2)) {
      
    if(lista_r2[i,col] < .4)
      datamin <- c(datamin, paste(lista_r2$Estacoes[i]))
      
    if(lista_r2[i,col] >= .4 && lista_r2[i,col] <= .6)
      data0406 <- c(data0406, paste(lista_r2$Estacoes[i]))
    
    if(lista_r2[i,col] > .6 && lista_r2[i,col] <= .8)
      data0608 <- c(data0608, paste(lista_r2$Estacoes[i]))
    
    if(lista_r2[i,col] >= .88)
      datamax <- c(datamax, paste(lista_r2$Estacoes[i]))
  
  }
  
  #Aqui igualo as linhas das colunas e preencho com 'NA' os valores não existentes
  datamin[(length(datamin) + 1):673] <- NA
  data0406[(length(data0406) + 1):673] <- NA
  data0608[(length(data0608) + 1):673] <- NA
  datamax[(length(datamax) + 1):673] <- NA
  
  #Criacao do dataframe 
  df <- data.frame(Datamin = datamin, 
                   Data04_06 = data0406, 
                   Data06_08 = data0608, 
                   Datamax = datamax)
  
  #Retorno o dataframe 
  return(df)

} 

# Mesma funcao do script 2
pega_codigo <- function(arq) {
  partes <- unlist(strsplit(arq, '_'))
  cod <- ifelse(partes[1] == 'G',
                paste(partes[1], partes[2], sep = '_'),
                partes[1])
  return(cod)
}

# Funcao que busca o iduf das estacoes de cada dataframe(diario, mensal e decendial)
busca <- function(arq){
  estacoes <- read.csv("estacoes_conprees_BR_tudo.csv")
  aux <- NULL
  aux <- as.character(arq)
  aux <- unlist(lapply(aux, pega_codigo))
  aux <- estacoes$iduf[match(aux, estacoes$codigoorigem)]
  return(aux)
}

# Funcao que cria os dataframes com os Ids de cada variavel(diario, mensal e decendial)
make_df <- function(nome){
  auxmin <- busca(nome$Datamin)
  aux0406 <- busca(nome$Data04_06)
  aux0608 <- busca(nome$Data06_08)
  auxmax <- busca(nome$Datamax)
  
  df <- data.frame(IDmin = auxmin,
                   ID0406 = aux0406, 
                   ID0608 = aux0608, 
                   IDmax = auxmax)
  return(df)
}

#Atribuo os respectivos dataframes
diario <- compara(4)
decendial <- compara(5)
mensal <- compara(6) 

# Separação dos nomes e do codigo de origem para filtragem
ID_diario <- make_df(diario)
ID_decendial <- make_df(decendial)
ID_mensal <- make_df(mensal)