library(tidyverse)

lista_r2 <- read.csv("Analises_R2.csv")
lista_r2$X <- NULL

estacoes <- read.csv("estacoes_conprees_BR_tudo.csv")

# Mesma funcao do script 2
pega_codigo <- function(arq) {
  partes <- unlist(strsplit(arq, '_'))
  cod <- ifelse(partes[1] == 'G',
                paste(partes[1], partes[2], sep = '_'),
                partes[1])
  return(cod)
}

# lapply retorna uma lista. Usa o unlist para converter em vetor
lista_r2$Estacoes <- as.character(lista_r2$Estacoes)
lista_r2$codigo <- unlist(lapply(lista_r2$Estacoes, pega_codigo))

# Juntando os dados por código original da estação
dados <- merge(lista_r2, 
               estacoes[c('codigoorigem', 'iduf', 'idmunicipio',
                          'latitude', 'longitude')],
               by.x = 'codigo',
               by.y = 'codigoorigem')


# agora com os dados juntos, fica fácil extrair as estatísticas
# para isso podemos usar o aggregate e a função summary

aggregate(dados$R2_Chuva, by = list(dados$iduf), summary)

# para contar quantos estão abaixo de 0.4, por exemplo
# escrevemos uma função e passamos para o aggregate

aggregate(dados$R2_Chuva, by = list(dados$iduf), function(x) sum(x < 0.4))

# mas pode ser que cada estado tenha um número diferente de estações
# o que torna a simples contagem pouco útil
# vejamos o porcentual de estações tem em cada estado com r2 < 0.4
aggregate(dados$R2_Chuva, by = list(dados$iduf), function(x) sum(x < 0.4) / length(x))

# Usando o aggregate podemos ir criando columa por coluna.
# Mas da mais trabalho. Nesse caso acho melhor usar um pacote chamado tidyverse
# que tem umas funções para fazer manipulaçõa de dados
# ele usa uma sintaxe diferente, mas vale a pena aprender
# https://r4ds.had.co.nz/

dados_resumo <- dados %>%
  group_by(iduf) %>%
  summarise(
    estacoes = n(),
    r2_chuva_mediano = median(R2_Chuva),
    r2_diario_mediano = median(R2_Diario),
    r2_decendial_mediano = median(R2_Decendial),
    r2_mensal_mediano = median(R2_Mensal),
    r2_diario_04 = sum(R2_Diario <= 0.4) / estacoes,
    r2_diario_08 = sum(R2_Diario >= 0.8) / estacoes,
    r2_decendial_04 = sum(R2_Decendial <= 0.4) / estacoes,
    r2_decendial_08 = sum(R2_Decendial >= 0.8) / estacoes,
    r2_mensal_04 = sum(R2_Mensal <= 0.4) / estacoes,
    r2_mensal_08 = sum(R2_Mensal >= 0.8) / estacoes,
  )


# tidyverse traz junto o GGPlot, que permite fazer uns gráficos bem legais
# aqui os gráficos estão meio porcos. O melhor seria colocar a sigla dos estados
# mas dá pra ver que o R2 aumenta a medida que fazermos a agregação temporal

ggplot(dados) +
  geom_boxplot(aes(y = R2_Diario, x = as.factor(iduf), group = iduf))

ggplot(dados) +
  geom_boxplot(aes(y = R2_Mensal, x = as.factor(iduf), group = iduf))


# comparando R2 entre períodos de agregacao
dados_long <- dados %>%
  select(codigo, R2_Diario, R2_Decendial, R2_Mensal, iduf, latitude, longitude) %>%
  pivot_longer(c(R2_Diario, R2_Decendial, R2_Mensal),
               names_to = 'agregacao',
               names_prefix = 'R2_',
               values_to = 'r2')


dados_long <- dados_long %>%
  mutate(agregacao = factor(agregacao, levels = c('Diario', 'Decendial', 'Mensal'),
                            ordered = TRUE))

ggplot(dados_long) +
  geom_boxplot(aes(x = agregacao, y = r2)) +
  labs(x = 'Período de agregação',
       y = 'R² estação vs. GPM IMERG Late Run')

# gerando um mapa
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

# pegando o naturalearth com omapa base
# mas pode fir de outro pacote, como brazilmap (não conheco detalhes)
library(rnaturalearth)
library(sf)

# para usar o ne_states é preciso instalar o pacote rnaturalearthhires
# install.packages("rnaturalearthhires",
#                  repos = "http://packages.ropensci.org",
#                  type = "source")
br <- ne_states(country = 'Brazil', returnclass = 'sf')

# O naturalearth não traz os estados. Talvez outros pacotes tragam
# dá pra ver que temos umas estações no litoral com R2 baixo
# mas acho que pode ser problema da estação não sei dizer
ggplot(br) +
  geom_sf() +
  geom_point(data = dados_long %>%
               filter(agregacao == 'Mensal'),
             aes(x = longitude, y = latitude, col = r2))

ggplot(br) +
  geom_sf() +
  geom_point(data = dados_long %>%
               filter(agregacao == 'Decendial'),
             aes(x = longitude, y = latitude, col = r2))


ggplot(br) +
  geom_sf() +
  geom_point(data = dados_long %>%
               filter(agregacao != 'Mensal'),
             aes(x = longitude, y = latitude, col = r2), size = 0.7) +
  facet_grid(~agregacao)

ggplot(br) +
  geom_sf() +
  geom_point(data = dados,
             aes(x = longitude, y = latitude, col = R2_Decendial))

ggplot(br) +
  geom_sf() +
  geom_point(data = dados,
             aes(x = longitude, y = latitude, col = R2_Diario))
