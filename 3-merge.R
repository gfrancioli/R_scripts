
library(stringr)
#Preparacao e carregamento dos arquivos
lista_estacoes <- dir('estacoes_nao_zeradas/', pattern = '.csv',
                      full.names = TRUE)
nomes <- dir('estacoes_nao_zeradas/', pattern = '.csv')
dados_gpm <- readRDS("gpm/Dados_gpm.RDS")

#For percorrendo as estacoes
for (i in 1:length(lista_estacoes)) {
  estacao <- read.csv(lista_estacoes[i])
  estacao$idestacao <- NULL
  
  # O nome das colunas e mudado para facilitar o merge
  names(estacao) <- c("data", "prec_estacao")
  
  # O nome da coluna no data-frame do gpm tambem e mudado 
  col_estacao <- names(dados_gpm[i])
  gpm_na_estacao <- dados_gpm[c(col_estacao, 'data')]
  names(gpm_na_estacao) <- c('prec_gpm', 'data')

  estacao$data <- as.Date(estacao$data)
  estacao_gpm <- merge(estacao, gpm_na_estacao)
  
  #Verificacao e retirada de arquivos que nao possuem dados suficientes para analise
  if(is.na(estacao_gpm$prec_gpm) || nrow(estacao_gpm) < 731) {
    write.csv(estacao_gpm, file = paste0('estacoes_sem_dados/', nomes[i]))
    nomes<- dir('estacoes_nao_zeradas/', pattern = '.csv')
    next
  }
  #Extracao em outra pasta para uso na analise de r2
  write.csv(estacao_gpm, file = paste0('estacoes_com_gpm/', nomes[i]))
}
