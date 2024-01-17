## Instalando os pacotes necessários para a análise
install.packages("ggplot2")
install.packages("dplyr")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("readxl")
install.packages("kableExtra")
install.packages("factoextra")
install.packages("gridExtra")

## Carregando os pacotes necessários para a análise
library(ggplot2)
library(dplyr)
library(knitr)
library(rmarkdown)
library(readxl)
library(kableExtra)
library(factoextra)
library(gridExtra)

## Lendo os dados do arquivo Excel
dados <- read_excel("C:\\Users\\bruna.gomes\\Desktop\\BI\\Base Teste Sazo.xlsx")

## Exibindo as linhas dos dados em formato tabular
kable(head(dados, 5000), col.names = c("Customer CNPJ", "jan_21", "fev_21", "mar_21", "abr_21", "mai_21", "jun_21", "jul_21", "ago_21", "set_21", "out_21", "nov_21", "dez_21", "jan_22", "fev_22", "mar_22", "abr_22", "mai_22", "jun_22", "jul_22", "ago_22", "set_22", "out_22", "nov_22", "dez_22", "jan_23", "fev_23", "mar_23", "abr_23", "mai_23", "jun_23", "jul_23", "ago_23", "set_23", "out_23", "nov_23", "dez_23")) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## Selecionando apenas as variáveis quantitativas
dados.quanti <- dados[, -1]

## Definindo o número máximo de clusters
k.max <- 17

## Definindo os dados para análise
data <- dados.quanti

## Calculando a Soma dos Quadrados Dentro dos Clusters (WSS) para diferentes valores de k
wss <- sapply(1:k.max, function(k) {
  kmeans(data, k, nstart = 50, iter.max = 17)$tot.withinss
})

## Exibindo os valores de WSS para diferentes valores de k
wss

## Visualizando o gráfico Elbow para determinar o número ideal de clusters
fviz_nbclust(data, kmeans, method = "wss")

