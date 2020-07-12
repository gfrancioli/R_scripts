library(raster)

# separacao dos arquivos 
estacoes_ok <- dir('estacoes_nao_zeradas/', pattern = '.csv')

pega_codigo <- function(arq) {
  partes <- unlist(strsplit(arq, '_'))
  cod <- ifelse(partes[1] == 'G',
                paste(partes[1], partes[2], sep = '_'),
                partes[1])
  return(cod)
}

codigos <- unlist(lapply(estacoes_ok, pega_codigo))
arqtudo <- read.csv('estacoes_conprees_BR_tudo.csv', encoding = 'UTF-8')

#ordenacao das estacoes de acordo com a variavel codigos
data_ordenado <- match(codigos, arqtudo$codigoorigem)

lat <- arqtudo[data_ordenado, 'latitude']
lon <- arqtudo[data_ordenado, 'longitude']

# separacao e definicao de arquivos raster brick
gpm2016 <- raster::brick('gpm/gpm_2016_9am.nc')
gpm2017 <- raster::brick('gpm/gpm_2017_9am.nc')
points2016 <- raster::extract(gpm2016, cbind(lon, lat))
points2017 <- raster::extract(gpm2017, cbind(lon, lat))

# transformacao em dataframe
dados2016 <- t(points2016)/10
dados2016 <- as.data.frame(dados2016)
dados2016$data <- seq.Date(from = as.Date('2016-01-01'),
                        to = as.Date('2016-12-31'),
                        by = 'day')

dados2017 <- t(points2017)/10
dados2017 <- as.data.frame(dados2017)
dados2017$data <- seq.Date(from = as.Date('2017-01-01'),
                           to = as.Date('2017-12-31'),
                           by = 'day')

# junto os dois dfs em um so para facilitar a visualizacao no futuro
dados_gpm <- rbind(dados2016, dados2017)
dados_gpm <- as.data.frame(dados_gpm)
saveRDS(dados_gpm, file = "gpm/Dados_gpm.RDS")
