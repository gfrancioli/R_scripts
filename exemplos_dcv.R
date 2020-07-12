# Exemplo de regressão linear
x <- 1:100
y <- runif(100)

plot(x, y)

summary(lm(y ~x))

y <- x * 2 - 3 + 1000*runif(100)
plot(x, y)

reg <- summary(lm(y ~ x))

## calcular decendio
decendio <- function(d) {
  mes <- lubridate::month(d)
  dia <- lubridate::day(d)
  
  dec_n <- ifelse(dia <= 10, 1, 
                  ifelse(dia <= 20, 2, 3))
  
  dec <- (mes - 1)*3 + dec_n
  return(dec)
}


# como agregar dados
mensal <- aggregate(estacao_gpm[2:3],
                    by = list(mes = strftime(estacao_gpm$data, '%Y-%m')), FUN = sum)

decendial <- aggregate(estacao_gpm[2:3],
                       by = list(dec = decendio(estacao_gpm$data),
                              ano = strftime(estacao_gpm$data, '%Y')),
                       FUN = sum)

# gerando alguns gráficos
plot(decendial$prec_estacao, t = 'l')
plot(decendial$prec_estacao, t = 'b')
lines(decendial$prec_gpm, col = 'red', t = 'b')

# graf. de dispersão
plot(decendial$prec_estacao, decendial$prec_gpm)
points(estacao_gpm$prec_estacao, estacao_gpm$prec_gpm, col = 'red')
points(mensal$prec_estacao, mensal$prec_gpm, col = 'blue')

