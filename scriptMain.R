# Carregar pacote necessário
install.packages('readr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages("plotrix")
library(readr)
library(dplyr)
library(ggplot2)
library(plotrix)
#-----------------------------------------------------------------------

# Carregar o arquivo CSV
LeitoOcupacao_2022 <- read_csv('esus-vepi.LeitoOcupacao_2022.csv')

#-----------------------------------------------------------------------

# Visualizar estrutura do DataFrame
str(LeitoOcupacao_2022)

# Obter resumo estatístico das colunas numéricas
summary(LeitoOcupacao_2022)

# Verificar valores únicos em colunas categóricas
unique(LeitoOcupacao_2022$estado)
unique(LeitoOcupacao_2022$municipio)

# Contar valores nulos em cada coluna
sapply(LeitoOcupacao_2022, function(x) sum(is.na(x)))

# Visualizar distribuições de dados
hist(LeitoOcupacao_2022$ocupacaoSuspeitoCli, main="Distribuição de Ocupação Suspeito Clínico", xlab="Ocupação Suspeito Clínico")

# Analisar dados de ocupação
summary(LeitoOcupacao_2022$ocupacaoSuspeitoCli)
summary(LeitoOcupacao_2022$ocupacaoSuspeitoUti)
summary(LeitoOcupacao_2022$ocupacaoConfirmadoCli)
summary(LeitoOcupacao_2022$ocupacaoConfirmadoUti)

#-----------------------------------------------------------------------
# Analisar tendências por estado e município
table(LeitoOcupacao_2022$estado)
table(LeitoOcupacao_2022$municipio)
#-----------------------------------------------------------------------

# Explorar datas de notificação
LeitoOcupacao_2022$dataNotificacao <- as.Date(LeitoOcupacao_2022$dataNotificacao)
summary(LeitoOcupacao_2022$dataNotificacao)
# Remover valores NA da ocupação por estado
ocupacao_por_estado <- LeitoOcupacao_2022[!is.na(LeitoOcupacao_2022$ocupacaoSuspeitoCli), ]

#-----------------------------------------------------------------------

# Selecionar os estados de interesse
estados_interesse <- c("São Paulo", "Minas Gerais", "Rio de Janeiro", "Amapá", "Roraima")

# Filtrar o dataset apenas para os estados de interesse
LeitoOcupacao_interesse <- LeitoOcupacao_2022[LeitoOcupacao_2022$estado %in% estados_interesse,]

# Criar a tabela de frequências para os estados de interesse
freq_por_estado_interesse <- table(LeitoOcupacao_interesse$estado)[order(names(table(LeitoOcupacao_interesse$estado)))]


# Criar histograma da ocupação suspeita clínica por estado para os estados de interesse
barplot(freq_por_estado_interesse, col = "lightgreen",
        xlab = "", ylab = "Frequência",
        main = "Histograma da Ocupação Suspeito Clínico por Estado",
        names.arg = names(freq_por_estado_interesse),  # Manter nomes do eixo x
        las = 1,  # Rotacionar os nomes dos estados para melhor visualização
        cex.names = 0.8)  # Reduzir o tamanho dos nomes dos estados


#-----------------------------------------------------------------------
# Selecionar os estados da região Sul
estados_sul <- c("Paraná", "Rio Grande do Sul", "Santa Catarina")

# Filtrar o dataset apenas para os estados da região Sul
LeitoOcupacao_sul <- LeitoOcupacao_2022[LeitoOcupacao_2022$estado %in% estados_sul,]

# Criar a tabela de frequências para os estados de interesse
freq_por_estado_interesse_sul <- table(LeitoOcupacao_sul$estado)[order(names(table(LeitoOcupacao_sul$estado)))]

# Histograma para a região Sul
barplot(freq_por_estado_interesse_sul , col = "lightgreen",
        xlab = "Estado", ylab = "Frequência",
        main = "Histograma da Ocupação Suspeito Clínico - Sul",
        names.arg = unique(LeitoOcupacao_sul$estado),
        las = 1, cex.names = 0.8)

freq_por_estado_interesse_sul

# Mostrando os valores de frequências para referência
print(freq_por_estado_interesse_sul)

# Dados de casos confirmados por estado na região Sul
casos_sul <- c(10281, 6521, 19548)  # Substitua esses valores pelos dados corretos se necessário

# Calcular o total de casos confirmados na região Sul
total_casos_sul <- sum(casos_sul)

# Calcular a média de casos confirmados por estado na região Sul
media_casos_sul <- mean(casos_sul)

# Calcular a mediana de casos confirmados por estado na região Sul
mediana_casos_sul <- median(casos_sul)

# Calcular a moda de casos confirmados por estado na região Sul
moda_casos_sul <- which.max(table(casos_sul))
if (length(moda_casos_sul) == 0) {
  moda_casos_sul <- NA
}

# Calcular o desvio padrão de casos confirmados por estado na região Sul
desvio_padrao_casos_sul <- sd(casos_sul)

# Calcular a variância de casos confirmados por estado na região Sul
variancia_casos_sul <- var(casos_sul)

# Calcular o coeficiente de variação de casos confirmados por estado na região Sul
coeficiente_variacao_casos_sul <- sd(casos_sul) / mean(casos_sul) * 100

# Mostrar os resultados
cat("Total de casos confirmados na região Sul:", total_casos_sul, "\n")
cat("Média de casos confirmados na região Sul:", media_casos_sul, "\n")
cat("Mediana de casos confirmados na região Sul:", mediana_casos_sul, "\n")
cat("Moda de casos confirmados na região Sul:", moda_casos_sul, "\n")
cat("Desvio padrão de casos confirmados na região Sul:", desvio_padrao_casos_sul, "\n")
cat("Variância de casos confirmados na região Sul:", variancia_casos_sul, "\n")
cat("Coeficiente de variação de casos confirmados na região Sul:", coeficiente_variacao_casos_sul, "%\n")

# Plotar gráficos

# Histograma
hist(casos_sul, main="Histograma de Casos Confirmados - Sul",
     xlab="Casos Confirmados", col="lightblue", border="black")

# Boxplot
boxplot(casos_sul, main="Boxplot de Casos Confirmados - Sul",
        ylab="Casos Confirmados", col="lightblue")

# Gráfico de barras
barplot(casos_sul, main="Gráfico de Barras de Casos Confirmados - Sul",
        names.arg=c("Paraná", "Rio Grande do Sul", "Santa Catarina"),
        xlab="Estado", ylab="Casos Confirmados", col="lightblue", las=1, cex.names = 0.7)

# Dados dos estados da região Sul
estados_sul <- c("Paraná", "Rio Grande do Sul", "Santa Catarina")
siglas_sul <- c("PR", "RS", "SC")

# Dados dos casos confirmados na região Sul
casos_sul <- c(10281, 6521, 19548)
total_casos_sul <- sum(casos_sul)

# Probabilidades de uma pessoa ter COVID-19 em cada estado do Sul
probabilidades_sul <- (casos_sul / total_casos_sul) * 100

# Mostrar probabilidades em porcentagem
siglas_sul <- c("PR", "RS", "SC")
for (i in 1:length(siglas_sul)) {
  cat("Probabilidade de uma pessoa ter COVID-19 em", siglas_sul[i], ":", round(probabilidades_sul[i], 2), "%\n")
}

# Probabilidades de casos confirmados por estado em porcentagem
probabilidades_sul <- c(25.56, 16.22, 48.22) # Usando os resultados calculados anteriormente

# Cores para cada barra
cores <- c("lightblue", "lightgreen", "lightcoral")

# Criar o gráfico de barras
barplot(probabilidades_sul, 
        horiz = TRUE, 
        names.arg = rep("", length(estados_sul)),  # Não mostrar os nomes dos estados
        col = cores, 
        border = "black", 
        xlab = "Probabilidade (%)",
        main = "Probabilidade de Casos - Região Sul",
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados
# Dados de probabilidade
probabilidades_sul <- c(28.30, 17.94, 53.76)

# Calcular estatísticas descritivas
minimo <- min(probabilidades_sul)
maximo <- max(probabilidades_sul)
Q1 <- quantile(probabilidades_sul, 0.25)
mediana <- median(probabilidades_sul)
Q3 <- quantile(probabilidades_sul, 0.75)
IQR <- Q3 - Q1

# Calcular valores atípicos
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
valores_atipicos <- probabilidades_sul[probabilidades_sul < limite_inferior | probabilidades_sul > limite_superior]

# Resultados
cat("Análise Descritiva das Probabilidades de Casos Confirmados - Região Sul\n")
cat("Primeiro Quartil (Q1):", Q1, "\n")
cat("Mediana (Q2):", mediana, "\n")
cat("Terceiro Quartil (Q3):", Q3, "\n")
cat("Mínimo:", minimo, "\n")
cat("Máximo:", maximo, "\n")
cat("Possíveis valores atípicos:", if(length(valores_atipicos) > 0) valores_atipicos else "Nenhum", "\n")


# Dados dos casos confirmados nos estados do Sul
casos_sul <- c(10281, 6521, 19548)

# Teste t para comparar os estados
t_test_sul <- t.test(casos_sul, rep(casos_sul[1], 3))

# Mostrar resultados do teste t
cat("Resultados do Teste t para o Sul\n")
cat("Estatística t:", t_test_sul$statistic, "\n")
cat("p-valor:", t_test_sul$p.value, "\n")
cat("Intervalo de confiança:", t_test_sul$conf.int, "\n")

# Interpretação do resultado
if (t_test_sul$p.value < 0.05) {
  cat("Há uma diferença significativa no número de casos entre os estados comparados (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa no número de casos entre os estados comparados (p >= 0.05).\n")
}



#-----------------------------------------------------------------------
# Selecionar os estados da região Norte
estados_norte <- c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins")

# Filtrar o dataset apenas para os estados da região Norte
LeitoOcupacao_norte <- LeitoOcupacao_2022[LeitoOcupacao_2022$estado %in% estados_norte,]

# Criar a tabela de frequências para os estados da região Norte
freq_por_estado_norte <- table(LeitoOcupacao_norte$estado)[order(names(table(LeitoOcupacao_norte$estado)))]

# Criar histograma da ocupação suspeita clínica por estado para a região Norte
barplot(freq_por_estado_norte, col = "lightgreen",
        xlab = "", ylab = "Frequência",
        main = "Histograma da Ocupação Suspeito Clínico por Estado - Norte",
        names.arg = names(freq_por_estado_norte),  # Manter nomes do eixo x
        las = 1,  # Rotacionar os nomes dos estados para melhor visualização
        cex.names = 0.8)  # Reduzir o tamanho dos nomes dos estados

freq_por_estado_norte

# Dados de casos confirmados por estado na região Norte
casos_norte <- c(407, 395, 4395, 2761, 3142, 10, 1721)

# Calcular o total de casos confirmados na região Norte
total_casos_norte <- sum(casos_norte)

# Calcular a média de casos confirmados por estado na região Norte
media_casos_norte <- mean(casos_norte)

# Calcular a mediana de casos confirmados por estado na região Norte
mediana_casos_norte <- median(casos_norte)

# Calcular a moda de casos confirmados por estado na região Norte
moda_casos_norte <- which.max(table(casos_norte))
if (length(moda_casos_norte) == 0) {
  moda_casos_norte <- NA
}

# Calcular o desvio padrão de casos confirmados por estado na região Norte
desvio_padrao_casos_norte <- sd(casos_norte)

# Calcular a variância de casos confirmados por estado na região Norte
variancia_casos_norte <- var(casos_norte)

# Calcular o coeficiente de variação de casos confirmados por estado na região Norte
coeficiente_variacao_casos_norte <- sd(casos_norte) / mean(casos_norte) * 100

# Mostrar os resultados
cat("Total de casos confirmados na região Norte:", total_casos_norte, "\n")
cat("Média de casos confirmados na região Norte:", media_casos_norte, "\n")
cat("Mediana de casos confirmados na região Norte:", mediana_casos_norte, "\n")
cat("Moda de casos confirmados na região Norte:", moda_casos_norte, "\n")
cat("Desvio padrão de casos confirmados na região Norte:", desvio_padrao_casos_norte, "\n")
cat("Variância de casos confirmados na região Norte:", variancia_casos_norte, "\n")
cat("Coeficiente de variação de casos confirmados na região Norte:", coeficiente_variacao_casos_norte, "%\n")

# Plotar gráficos

# Histograma
hist(casos_norte, main="Histograma de Casos Confirmados - Norte",
     xlab="Casos Confirmados", col="lightblue", border="black")

# Boxplot
boxplot(casos_norte, main="Boxplot de Casos Confirmados - Norte",
        ylab="Casos Confirmados", col="lightblue")

# Gráfico de barras
barplot(casos_norte, main="Gráfico de Barras de Casos Confirmados - Norte",
        names.arg=c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"),
        xlab="Estado", ylab="Casos Confirmados", col="lightblue", las=1, cex.names = 0.7)

# Dados dos casos confirmados por estado na região Norte
casos_norte <- c(407, 395, 4395, 2761, 3142, 10, 1721)

# Total de casos confirmados na região Norte
total_casos_norte <- sum(casos_norte)

# Calcula as probabilidades em porcentagem
probabilidades_norte <- casos_norte / total_casos_norte * 100

# Mostra as probabilidades em porcentagem
for (i in 1:length(casos_norte)) {
  cat("Probabilidade de uma pessoa ter COVID-19 em", estados_norte[i], ":", round(probabilidades_norte[i], 2), "%\n")
}
# Dados dos estados da região Norte
estados_norte <- c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins")
siglas_norte <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")

# Probabilidades de casos confirmados por estado em porcentagem
probabilidades_norte <- c(3.17, 3.08, 34.25, 21.52, 24.49, 0.08, 13.41)

# Cores para cada barra
cores <- c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink", "lightsalmon", "lightcyan")

# Criar o gráfico de barras
barplot(probabilidades_norte, 
        horiz = TRUE, 
        names.arg = rep("", length(estados_norte)),  # Não mostrar os nomes dos estados
        col = cores, 
        border = "black", 
        xlab = "Probabilidade (%)",
        main = "Probabilidade de Casos - Região Norte",
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados

# Adicionar as siglas dos estados ao lado dos nomes
mtext(siglas_norte, side = 2, at = barplot(probabilidades_norte, plot = FALSE), line = -0.5, cex = 0.7)


# Dados dos casos confirmados nos estados do Norte
casos_norte <- c(407, 395, 4395, 2761, 3142, 10, 1721)

# Teste t para comparar os estados
t_test_norte <- t.test(casos_norte[1:2], casos_norte[3:7])

# Mostrar resultados do teste t
cat("Resultados do Teste t para a Região Norte\n")
cat("Estatística t:", t_test_norte$statistic, "\n")
cat("p-valor:", t_test_norte$p.value, "\n")
cat("Intervalo de confiança:", t_test_norte$conf.int, "\n")

# Interpretação do resultado
if (t_test_norte$p.value < 0.05) {
  cat("Há uma diferença significativa no número de casos entre os estados comparados (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa no número de casos entre os estados comparados (p >= 0.05).\n")
}

# Dados dos casos confirmados nos estados do Norte
casos_norte <- c(407, 395, 4395, 2761, 3142, 10, 1721)

# Calculando as estatísticas descritivas
Q1 <- quantile(casos_norte, 0.25)
mediana <- median(casos_norte)
Q3 <- quantile(casos_norte, 0.75)
minimo <- min(casos_norte)
maximo <- max(casos_norte)

# Imprimir os resultados
cat("Primeiro Quartil (Q1):", Q1, "\n")
cat("Mediana (Q2):", mediana, "\n")
cat("Terceiro Quartil (Q3):", Q3, "\n")
cat("Mínimo:", minimo, "\n")
cat("Máximo:", maximo, "\n")




#-------------------------------------------------------------------------------------------------
# Selecionar as siglas dos estados da região Nordeste
siglas_nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")

# Criar histograma da ocupação suspeita clínica por estado para a região Nordeste com siglas
barplot(freq_por_estado_nordeste, col = "lightgreen",
        xlab = "Ocupação Suspeito Clínico", ylab = "Frequência",
        main = "Histograma da Ocupação Suspeito Clínico por Estado - Nordeste",
        names.arg = siglas_nordeste,  # Usar siglas dos estados no eixo x
        las = 1,  # Manter orientação horizontal dos textos
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados

freq_por_estado_nordeste

# Dados da região Nordeste
casos_nordeste <- c(2092, 7664, 10897, 4213, 1925, 13904, 2914, 4262, 2417)

# Total de casos na região Nordeste
total_casos_nordeste <- sum(casos_nordeste)

# Média de casos na região Nordeste
media_casos_nordeste <- mean(casos_nordeste)

# Mediana de casos na região Nordeste
mediana_casos_nordeste <- median(casos_nordeste)

# Moda de casos na região Nordeste
moda_casos_nordeste <- {
  moda <- names(sort(-table(casos_nordeste)))[1]
  if(table(casos_nordeste)[moda] == 1) {
    moda <- "Não há valor que se repete com mais frequência"
  }
  moda
}

# Desvio padrão na região Nordeste
desvio_padrao_casos_nordeste <- sd(casos_nordeste)

# Variância na região Nordeste
variancia_casos_nordeste <- var(casos_nordeste)

# Coeficiente de variação na região Nordeste
coeficiente_variacao_casos_nordeste <- (desvio_padrao_casos_nordeste / media_casos_nordeste) * 100

# Resultados
cat("Total de casos na região Nordeste:", total_casos_nordeste, "\n")
cat("Média de casos na região Nordeste:", media_casos_nordeste, "\n")
cat("Mediana de casos na região Nordeste:", mediana_casos_nordeste, "\n")
cat("Moda de casos na região Nordeste:", moda_casos_nordeste, "\n")
cat("Desvio padrão na região Nordeste:", desvio_padrao_casos_nordeste, "\n")
cat("Variância na região Nordeste:", variancia_casos_nordeste, "\n")
cat("Coeficiente de variação na região Nordeste:", coeficiente_variacao_casos_nordeste, "%\n")

# Boxplot
boxplot(dados_nordeste, main="Boxplot de Ocupação Suspeita Clínica por Estado - Nordeste",
        ylab="Ocupação Suspeita Clínica", col="lightgreen")

# Plotar gráficos

# Histograma
barplot(dados_nordeste, col = "lightgreen",
        xlab = "Estado", ylab = "Ocupação Suspeita Clínica",
        main = "Ocupação Suspeita Clínica por Estado - Nordeste",
        names.arg = estados_nordeste,  # Usar siglas dos estados no eixo x
        las = 1,  # Manter orientação horizontal dos textos
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados

# Dados de ocupação suspeita clínica por estado para o Nordeste
dados_nordeste <- c(2092, 7664, 10897, 4213, 1925, 13904, 2914, 4262, 2417)
estados_nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")

# Calcular a moda
moda_nordeste <- estados_nordeste[which.max(dados_nordeste)]

# Calcular a média
media_nordeste <- mean(dados_nordeste)

# Calcular a mediana
mediana_nordeste <- median(dados_nordeste)

# Calcular o desvio padrão
desvio_padrao_nordeste <- sd(dados_nordeste)

# Calcular a variância
variancia_nordeste <- var(dados_nordeste)

# Calcular o coeficiente de variação
coeficiente_variacao_nordeste <- sd(dados_nordeste) / mean(dados_nordeste) * 100

# Mostrar os resultados
cat("Moda de ocupação suspeita clínica por estado no Nordeste:", moda_nordeste, "\n")
cat("Média de ocupação suspeita clínica por estado no Nordeste:", media_nordeste, "\n")
cat("Mediana de ocupação suspeita clínica por estado no Nordeste:", mediana_nordeste, "\n")
cat("Desvio padrão de ocupação suspeita clínica por estado no Nordeste:", desvio_padrao_nordeste, "\n")
cat("Variância de ocupação suspeita clínica por estado no Nordeste:", variancia_nordeste, "\n")
cat("Coeficiente de variação de ocupação suspeita clínica por estado no Nordeste:", coeficiente_variacao_nordeste, "%\n")

# Dados de casos confirmados por estado na região Nordeste
casos_nordeste <- c(2092, 7664, 10897, 4213, 1925, 13904, 2914, 4262, 2417)

# Calcular o total de casos confirmados na região Nordeste
total_casos_nordeste <- sum(casos_nordeste)

# Probabilidade de uma pessoa ter COVID-19 em cada estado
probabilidades_nordeste <- casos_nordeste / total_casos_nordeste * 100

# Mostrar probabilidades em porcentagem
for (i in 1:length(casos_nordeste)) {
  cat("Probabilidade de uma pessoa ter COVID-19 em", estados_nordeste[i], ":", round(probabilidades_nordeste[i], 2), "%\n")
}

# Probabilidades de uma pessoa ter COVID-19 em cada estado do Nordeste
probabilidades_nordeste <- c(4.16, 15.24, 21.67, 8.38, 3.83, 27.65, 5.79, 8.48, 4.81)
estados_nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")

# Cores para cada estado
cores_nordeste <- c("lightblue", "lightgreen", "lightcoral", "lightyellow",
                    "lightpink", "lightcyan", "lightsalmon", "lightseagreen", "lightskyblue")

probabilidades_nordeste
# Criar o gráfico de barras
barplot(probabilidades_nordeste, 
        horiz = TRUE, 
        names.arg = estados_nordeste, 
        col = cores_nordeste, 
        border = "black", 
        xlab = "Probabilidade (%)",
        main = "Probabilidade de Casos - Nordeste",
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados

# Dados dos casos confirmados nos estados do Norte
casos_norte <- c(407, 395, 4395, 2761, 3142, 10, 1721)

# Teste t para comparar os estados
t_test_norte <- t.test(casos_norte[1:2], casos_norte[3:7])

# Mostrar resultados do teste t
cat("Resultados do Teste t para a Região Norte\n")
cat("Estatística t:", t_test_norte$statistic, "\n")
cat("p-valor:", t_test_norte$p.value, "\n")
cat("Intervalo de confiança:", t_test_norte$conf.int, "\n")

# Interpretação do resultado
if (t_test_norte$p.value < 0.05) {
  cat("Há uma diferença significativa no número de casos entre os estados comparados (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa no número de casos entre os estados comparados (p >= 0.05).\n")
}




#-------------------------------------------------------------------------------------------------
# Selecionar os estados da região Sudeste
estados_sudeste <- c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo")

# Filtrar o dataset apenas para os estados da região Sudeste
LeitoOcupacao_sudeste <- LeitoOcupacao_2022[LeitoOcupacao_2022$estado %in% estados_sudeste,]

# Criar a tabela de frequências para os estados da região Sudeste
freq_por_estado_sudeste <- table(LeitoOcupacao_sudeste$estado)[order(names(table(LeitoOcupacao_sudeste$estado)))]

# Criar vetor com siglas dos estados da região Sudeste
siglas_sudeste <- c("ES", "MG", "RJ", "SP")

# Criar histograma da ocupação suspeita clínica por estado para a região Sudeste
barplot(freq_por_estado_sudeste, col = "lightgreen",
        xlab = "Estado", ylab = "Frequência",
        main = "Histograma da Ocupação Suspeito Clínico por Estado - Sudeste",
        names.arg = siglas_sudeste,  # Usar siglas dos estados no eixo x
        las = 1,  # Manter orientação horizontal dos textos
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados
print(freq_por_estado_sudeste)

# Dados de casos confirmados por estado na região Sudeste
casos_sudeste <- c(4392, 53030, 17014, 68388)

# Calcular o total de casos confirmados na região Sudeste
total_casos_sudeste <- sum(casos_sudeste)

# Calcular a média de casos confirmados por estado na região Sudeste
media_casos_sudeste <- mean(casos_sudeste)

# Calcular a mediana de casos confirmados por estado na região Sudeste
mediana_casos_sudeste <- median(casos_sudeste)

# Calcular a moda de casos confirmados por estado na região Sudeste
moda_casos_sudeste <- which.max(table(casos_sudeste))
if (length(moda_casos_sudeste) == 0) {
  moda_casos_sudeste <- NA
}

# Calcular o desvio padrão de casos confirmados por estado na região Sudeste
desvio_padrao_casos_sudeste <- sd(casos_sudeste)

# Calcular a variância de casos confirmados por estado na região Sudeste
variancia_casos_sudeste <- var(casos_sudeste)

# Calcular o coeficiente de variação de casos confirmados por estado na região Sudeste
coeficiente_variacao_casos_sudeste <- (desvio_padrao_casos_sudeste / media_casos_sudeste) * 100

# Mostrar os resultados
cat("Total de casos confirmados na região Sudeste:", total_casos_sudeste, "\n")
cat("Média de casos confirmados na região Sudeste:", media_casos_sudeste, "\n")
cat("Mediana de casos confirmados na região Sudeste:", mediana_casos_sudeste, "\n")
cat("Moda de casos confirmados na região Sudeste:", moda_casos_sudeste, "\n")
cat("Desvio padrão de casos confirmados na região Sudeste:", desvio_padrao_casos_sudeste, "\n")
cat("Variância de casos confirmados na região Sudeste:", variancia_casos_sudeste, "\n")
cat("Coeficiente de variação de casos confirmados na região Sudeste:", coeficiente_variacao_casos_sudeste, "%\n")

# Plotar gráficos

# Histograma
hist(casos_sudeste, main="Histograma de Casos Confirmados - Sudeste",
     xlab="Casos Confirmados", col="lightblue", border="black")

# Boxplot
boxplot(casos_sudeste, main="Boxplot de Casos Confirmados - Sudeste",
        ylab="Casos Confirmados", col="lightblue")

# Gráfico de barras
barplot(casos_sudeste, main="Gráfico de Barras de Casos Confirmados - Sudeste",
        names.arg=c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo"),
        xlab="Estado", ylab="Casos Confirmados", col="lightblue", las=1, cex.names = 0.7)

# Probabilidade de uma pessoa ter COVID-19 em cada estado
probabilidades <- casos_sudeste / total_casos_sudeste * 100

# Mostrar probabilidades em porcentagem
for (i in 1:length(siglas_sudeste)) {
  cat("Probabilidade de uma pessoa ter COVID-19 em", siglas_sudeste[i], ":", round(probabilidades[i], 2), "%\n")
}

# Inferência: Teste t para comparação entre Espírito Santo e Minas Gerais, Rio de Janeiro e São Paulo
t_test <- t.test(casos_sudeste[1:2], casos_sudeste[3:4])

# Mostrar resultados do teste t
cat("Resultados do Teste t\n")
cat("Estatística t:", t_test$statistic, "\n")
cat("p-valor:", t_test$p.value, "\n")
cat("Intervalo de confiança:", t_test$conf.int, "\n")

# Interpretação do resultado
if (t_test$p.value < 0.05) {
  cat("Há uma diferença significativa no número de casos entre os estados comparados (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa no número de casos entre os estados comparados (p >= 0.05).\n")
}

# Probabilidades de casos confirmados por estado em porcentagem
probabilidades <- c(47.88, 37.13, 11.91, 3.08)
estados <- c("SP", "MG", "RJ", "ES")

# Cores para cada barra
cores <- c("lightblue", "lightgreen", "lightcoral", "lightyellow")

# Criar o gráfico de barras
barplot(probabilidades, 
        horiz = TRUE, 
        names.arg = estados, 
        col = cores, 
        border = "black", 
        xlab = "Probabilidade (%)",
        main = "Probabilidade de Casos - Sudeste",
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados


# Probabilidades de uma pessoa ter COVID-19 em cada estado do Nordeste
probabilidades_nordeste <- c(4.16, 15.24, 21.67, 8.38, 3.83, 27.65, 5.79, 8.48, 4.81)
estados_nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")

# Probabilidade em Pernambuco (PE)
prob_pe <- probabilidades_nordeste[6]

# Probabilidade média nos outros estados do Nordeste
prob_media_outros <- mean(probabilidades_nordeste[-6])

# Teste t para comparar a probabilidade em Pernambuco com a média dos outros estados
t_test_nordeste <- t.test(prob_pe, prob_media_outros)

# Mostrar resultados do teste t
cat("Resultados do Teste t\n")
cat("Estatística t:", t_test_nordeste$statistic, "\n")
cat("p-valor:", t_test_nordeste$p.value, "\n")
cat("Intervalo de confiança:", t_test_nordeste$conf.int, "\n")

# Interpretação do resultado
if (t_test_nordeste$p.value < 0.05) {
  cat("Há uma diferença significativa na probabilidade de uma pessoa ter COVID-19 em Pernambuco em relação à média dos outros estados do Nordeste (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa na probabilidade de uma pessoa ter COVID-19 em Pernambuco em relação à média dos outros estados do Nordeste (p >= 0.05).\n")
}

# Probabilidades de uma pessoa ter COVID-19 em cada estado do Nordeste
probabilidades_nordeste <- c(4.16, 15.24, 21.67, 8.38, 3.83, 27.65, 5.79, 8.48, 4.81)

# Inferência: Teste t para comparação entre dois grupos de estados
# Vamos comparar PE e CE com BA e MA
grupo_1 <- c(27.65, 21.67)  # PE e CE
grupo_2 <- c(15.24, 8.38)    # BA e MA

t_test <- t.test(grupo_1, grupo_2)

# Mostrar resultados do teste t
cat("Resultados do Teste t\n")
cat("Estatística t:", t_test$statistic, "\n")
cat("p-valor:", t_test$p.value, "\n")
cat("Intervalo de confiança:", t_test$conf.int, "\n")

# Interpretação do resultado
if (t_test$p.value < 0.05) {
  cat("Há uma diferença significativa na probabilidade de casos de COVID-19 entre os grupos de estados comparados (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa na probabilidade de casos de COVID-19 entre os grupos de estados comparados (p >= 0.05).\n")
}



#-------------------------------------------------------------------------------------------------
# Selecionar os estados da região Centro-Oeste
estados_centro_oeste <- c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul")

# Filtrar o dataset apenas para os estados da região Centro-Oeste
LeitoOcupacao_centro_oeste <- LeitoOcupacao_2022[LeitoOcupacao_2022$estado %in% estados_centro_oeste,]

# Criar a tabela de frequências para os estados da região Centro-Oeste
freq_por_estado_interesse_centro_oeste <- table(LeitoOcupacao_centro_oeste$estado)[order(names(table(LeitoOcupacao_centro_oeste$estado)))]

# Histograma para a região Centro-Oeste
barplot(freq_por_estado_interesse_centro_oeste, col = "lightgreen",
        xlab = "Ocupação Suspeito Clínico", ylab = "Frequência",
        main = "Histograma da Ocupação Suspeito Clínico - Centro-Oeste",
        names.arg = estados_centro_oeste,
        las = 1, cex.names = 0.7)
freq_por_estado_interesse_centro_oeste
estados_centro_oeste

# Dados dos casos confirmados nos estados do Centro-Oeste
casos_centro_oeste <- c(2358, 32325, 2417, 12523)

# Teste t para comparar os estados
t_test_centro_oeste <- t.test(casos_centro_oeste[1:2], casos_centro_oeste[3:4])

# Mostrar resultados do teste t
cat("Resultados do Teste t para a Região Centro-Oeste\n")
cat("Estatística t:", t_test_centro_oeste$statistic, "\n")
cat("p-valor:", t_test_centro_oeste$p.value, "\n")
cat("Intervalo de confiança:", t_test_centro_oeste$conf.int, "\n")

# Interpretação do resultado
if (t_test_centro_oeste$p.value < 0.05) {
  cat("Há uma diferença significativa no número de casos entre os estados comparados (p < 0.05).\n")
} else {
  cat("Não há uma diferença significativa no número de casos entre os estados comparados (p >= 0.05).\n")
}




# Dados de casos confirmados por estado na região Centro-Oeste
casos_centro_oeste <- c(2358, 32325, 2417, 12523)

# Calcular o total de casos confirmados na região Centro-Oeste
total_casos_centro_oeste <- sum(casos_centro_oeste)

# Calcular a média de casos confirmados por estado na região Centro-Oeste
media_casos_centro_oeste <- mean(casos_centro_oeste)

# Calcular a mediana de casos confirmados por estado na região Centro-Oeste
mediana_casos_centro_oeste <- median(casos_centro_oeste)

# Calcular a moda de casos confirmados por estado na região Centro-Oeste
moda_casos_centro_oeste <- which.max(table(casos_centro_oeste))
if (length(moda_casos_centro_oeste) == 0) {
  moda_casos_centro_oeste <- NA
}

# Calcular o desvio padrão de casos confirmados por estado na região Centro-Oeste
desvio_padrao_casos_centro_oeste <- sd(casos_centro_oeste)

# Calcular a variância de casos confirmados por estado na região Centro-Oeste
variancia_casos_centro_oeste <- var(casos_centro_oeste)

# Calcular o coeficiente de variação de casos confirmados por estado na região Centro-Oeste
coeficiente_variacao_casos_centro_oeste <- sd(casos_centro_oeste) / mean(casos_centro_oeste) * 100

# Mostrar os resultados
cat("Total de casos confirmados na região Centro-Oeste:", total_casos_centro_oeste, "\n")
cat("Média de casos confirmados na região Centro-Oeste:", media_casos_centro_oeste, "\n")
cat("Mediana de casos confirmados na região Centro-Oeste:", mediana_casos_centro_oeste, "\n")
cat("Moda de casos confirmados na região Centro-Oeste:", moda_casos_centro_oeste, "\n")
cat("Desvio padrão de casos confirmados na região Centro-Oeste:", desvio_padrao_casos_centro_oeste, "\n")
cat("Variância de casos confirmados na região Centro-Oeste:", variancia_casos_centro_oeste, "\n")
cat("Coeficiente de variação de casos confirmados na região Centro-Oeste:", coeficiente_variacao_casos_centro_oeste, "%\n")

# Plotar gráficos

# Histograma
hist(casos_centro_oeste, main="Histograma de Casos Confirmados - Centro-Oeste",
     xlab="Casos Confirmados", col="lightgreen", border="black")

# Boxplot
boxplot(casos_centro_oeste, main="Boxplot de Casos Confirmados - Centro-Oeste",
        ylab="Casos Confirmados", col="lightgreen")

# Gráfico de barras
barplot(casos_centro_oeste, main="Gráfico de Barras de Casos Confirmados - Centro-Oeste",
        names.arg=estados_centro_oeste,
        xlab="Estado", ylab="Casos Confirmados", col="lightgreen", las=1, cex.names = 0.7)

# Probabilidades e estados do Centro-Oeste
probabilidades_centro_oeste <- c(6.50, 89.20, 6.67, 34.63)  # Substitua com as probabilidades corretas
estados_centro_oeste <- c("DF", "GO", "MT", "MS")

# Cores para cada barra
cores_centro_oeste <- c("lightblue", "lightgreen", "lightcoral", "lightyellow")

# Criar o gráfico de barras
barplot(probabilidades_centro_oeste, 
        horiz = TRUE, 
        names.arg = estados_centro_oeste, 
        col = cores_centro_oeste, 
        border = "black", 
        xlab = "Probabilidade (%)",
        main = "Probabilidade de Casos - Centro-Oeste",
        cex.names = 0.7)  # Reduzir o tamanho dos nomes dos estados

# Dados de casos confirmados por estado na região Centro-Oeste
casos_centro_oeste <- c(2358, 32325, 2417, 12523)

# Calcular o total de casos confirmados na região Centro-Oeste
total_casos_centro_oeste <- sum(casos_centro_oeste)

# Calcular as probabilidades de cada estado
probabilidades_centro_oeste <- casos_centro_oeste / total_casos_centro_oeste * 100

# Mostrar as probabilidades calculadas
cat("Probabilidades da região Centro-Oeste:\n")
for (i in 1:length(probabilidades_centro_oeste)) {
  cat(names(casos_centro_oeste)[i], "(", estados_centro_oeste[i], "):", probabilidades_centro_oeste[i], "%\n")
}

# Dados dos casos confirmados nos estados do Centro-Oeste
casos_centro_oeste <- c(2358, 32325, 2417, 12523)

# Calculando as estatísticas descritivas
Q1 <- quantile(casos_centro_oeste, 0.25)
mediana <- median(casos_centro_oeste)
Q3 <- quantile(casos_centro_oeste, 0.75)
minimo <- min(casos_centro_oeste)
maximo <- max(casos_centro_oeste)

# Imprimindo os resultados
cat("Primeiro Quartil (Q1):", Q1, "casos\n")
cat("Mediana (Q2):", mediana, "casos\n")
cat("Terceiro Quartil (Q3):", Q3, "casos\n")
cat("Mínimo:", minimo, "casos\n")
cat("Máximo:", maximo, "casos\n")


#-------------------------------------------------------------------------------------------------







