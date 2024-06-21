# Baixar bibliotecas
install.packages("dplyr")
install.packages("lmtest")
install.packages("car")
install.packages("sandwich")
install.packages("openxlsx")
library(readr)
library(ggplot2)
library(readxl)
library(dplyr)
library(lmtest)
library(car)
library(sandwich)
library(openxlsx)

#Ler a base de dados
dados <- read.xlsx("Downloads/dadosfinais.xlsx")

# Convertendo PIBmunicipal para log
#ln
dados$ln_PIBmunicipal <- log(dados$PIBmunicipal)

# Remover linhas com NA
dados <- na.omit(dados)

# Convertendo colunas numéricas para garantir a execução correta das regressões
dados <- dados %>%
  mutate(across(c(`taxadesocup`, `popsupcompleto`, `PIBmunicipal`, 'ln_PIBmunicipal',  `IDHM`), as.numeric))

# Remover outliers
remove_extreme_rows <- function(dados) {
  for (col in names(dados)) {
    if (is.numeric(dados[[col]])) {
      lower_bound <- quantile(dados[[col]], probs = 0.01, na.rm = TRUE)
      upper_bound <- quantile(dados[[col]], probs = 0.99, na.rm = TRUE)
      dados <- dados[dados[[col]] >= lower_bound & dados[[col]] <= upper_bound, ]
    }
  }
  return(dados)
}

dados <- remove_extreme_rows(dados)

#dummy de estado
dados$Sigla <- as.factor(dados$Sigla)

#descritivas após remoção de outliers
summary(dados$taxadesocup)
summary(dados$popsupcompleto)
summary(dados$ln_PIBmunicipal)
summary(dados$IDHM)
summary(dados$Sigla)

#Verificando correlação---------------------------------------------------------------------------------------
cor(dados$popsupcompleto, dados$ln_PIBmunicipal)
cor(dados$popsupcompleto, dados$IDHM)
cor(dados$popsupcompleto, dados$Sigla)

#Regressões---------------------------------------------------------------------------------------------------
# Regressão simples entre desocupação e população com superior completo
regressao_simples <- lm(taxadesocup ~ popsupcompleto, data = dados)
summary(regressao_simples)
bptest(regressao_simples)
coeftest(regressao_simples, vcov. = vcovHC, type = "HC1")

# Gráfico da regressão linear
plot(dados$popsupcompleto, dados$taxadesocup, 
     main = "Regressão Linear entre Desocupação e População com Superior Completo",
     xlab = "Percentual da População com Superior Completo",
     ylab = "Taxa de Desocupação",
     pch = 19, col = "blue")
abline(regressao_simples, col = "red", lwd = 2)

# Teste de viés das variáveis omitidas usando a função 'resettest' do pacote 'lmtest'
resettest(regressao_simples)

# Regressão múltipla 1
regressao_multipla1 <- lm(`taxadesocup` ~ `popsupcompleto` + `ln_PIBmunicipal`, data = dados)
summary(regressao_multipla1)
bptest(regressao_multipla1) #é hetero
coeftest(regressao_multipla1, vcov. = vcovHC, type = "HC1")

# Regressão múltipla2
regressao_multipla2 <- lm(`taxadesocup` ~ `popsupcompleto` + `ln_PIBmunicipal` + `IDHM`, data = dados)
summary(regressao_multipla2)
bptest(regressao_multipla2) #é hetero
coeftest(regressao_multipla2, vcov. = vcovHC, type = "HC1")

# Regressão múltipla3
regressao_multipla3 <- lm(`taxadesocup` ~ `popsupcompleto` + `ln_PIBmunicipal` + `IDHM` + `Sigla`, data = dados)
summary(regressao_multipla3)
bptest(regressao_multipla3) #é hetero
coeftest(regressao_multipla3, vcov. = vcovHC, type = "HC1")

#Teste F para a significância conjunta de popsupcompleto e IDHM
Null_hyp<-c("popsupcompleto", "IDHM")
linearHypothesis(regressao_multipla3, Null_hyp, vcov. =hccm(regressao_multipla3, type = "hc1"))##significante até a menos de 1%
