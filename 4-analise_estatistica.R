library(stringr)

#Funcao de calculo do decendio e de atualizacao do vetor de nomes
decendio <- function(d) {
  mes <- lubridate::month(d)
  dia <- lubridate::day(d)
  
  dec_n <- ifelse(dia <= 10, 1, 
                  ifelse(dia <= 20, 2, 3))
  
  dec <- (mes - 1)*3 + dec_n
  return(dec)
}

#Preparacao e carregamento dos arquivos completos
lista_comparadas <- dir('estacoes_com_gpm/', pattern = '.csv',
                        full.names = TRUE)
nomes <- dir('estacoes_com_gpm/', pattern = '.csv')
nomes <- str_replace(nomes, '.csv', '')

#Crio vetores auxiliares para guardar os resultados
r2_day <- NULL
r2_dec <- NULL
r2_month <- NULL
r2_chuva <- NULL
estacao_chuva <- NULL
gpm_chuva <- NULL

#For para percorrer as estacoes
for (i in 1:length(lista_comparadas)) {
  estacao <- read.csv(lista_comparadas[i])
  
  #Coluna X inutilizada por nao conter nenhuma informacao necessaria
  estacao$X <- NULL
  
  #Agregacao de decendios
  decendial <- aggregate(estacao[2:3],
                         by = list(
                           dec = decendio(estacao$data),
                           ano = strftime(estacao$data, '%Y')),
                         FUN = sum)
  #Agregacao de meses
  mes <- aggregate(estacao[2:3],
                   by = list(
                     mes = strftime(estacao$data, '%Y-%m')),
                   FUN = sum)
  
  #Junção dos dados dos dias em que choveu
  dados_chuva <- estacao[estacao$prec_estacao != 0, ]
  
  #Calculo do r2 para as variaveis do data-frame
  #Chuva
  r2 <- round(summary(lm(dados_chuva[,3] ~ dados_chuva[,2]))$adj.r.squared, digits = 3)
  r2_chuva <- c(r2_chuva,r2)
  
  #Diario
  r2 <- round(summary(lm(estacao$prec_gpm ~ estacao$prec_estacao))$adj.r.squared, digits = 3)
  r2_day <- c(r2_day, r2)
  
  #Decendial
  r2 <- round(summary(lm(decendial$prec_gpm ~ decendial$prec_estacao))$adj.r.squared, digits = 3)
  r2_dec <- c(r2_dec, r2)
  
  #Mensal
  r2 <- round(summary(lm(mes$prec_gpm ~ mes$prec_estacao))$adj.r.squared, digits = 3)
  r2_month <- c(r2_month, r2)
  
}
#Criacao do dataframe para exportacao dos dados e atualizacao da lista de nomes
analise <- data.frame(Estacoes = nomes,
                      R2_Chuva = r2_chuva,
                      R2_Diario = r2_day,
                      R2_Decendial = r2_dec,
                      R2_Mensal = r2_month)
  
#Exportacao do dataframe para um csv unico de r2
write.csv(analise, file = "Analises_R2.csv")

