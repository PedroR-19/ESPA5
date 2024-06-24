install.packages("readr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("vcd")

library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(vcd)


#importando as bases de dados
dados <- read.csv("INFLUD24-17-06-2024.csv", sep = ";")


#TRATANDO OS DADOS
head(dados24)
str(dados)
dados$SG_UF_NOT <- factor(dados$SG_UF_NOT, levels = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"))
dados$CO_REGIONA <- as.character(dados$CO_REGIONA)


#análise geral
summary(dados)

#quantas semanas após o sintomas o paciente foi atendido
hist(dados$SEM_PRI, 
     breaks = "Sturges",  # Método automático para determinar os intervalos dos bins
     col = "lightblue",  # Cor de preenchimento dos bins
     border = "white",   # Cor da borda dos bins
     main = "Distribuição da Quantidade de Semanas com Sintomas de Falta de Respiração",  # Título do histograma
     xlab = "Número de Semanas",  # Rótulo do eixo x
     ylab = "Frequência"  # Rótulo do eixo y
)

#houveram quantas ocorrencias em cada estado

pacientes_por_estado <- table(dados$SG_UF_NOT)
dados_estado <- data.frame(SG_UF_NOT = names(pacientes_por_estado),
                           Quantidade = as.numeric(pacientes_por_estado))
print(dados_estado)

#gráfico de disperção para expressar quantas ocorrencias houveram em cada estado
ggplot(dados_estado, aes(x = SG_UF_NOT, y = Quantidade)) +
  geom_point(size = 3, color = "blue") +  # Pontos azuis para representar os dados
  labs(title = "Quantidade de Pacientes por Estado",  # Título do gráfico
       x = "Estado", y = "Quantidade de Pacientes") +  # Rótulos dos eixos
  theme_minimal() +  # Estilo do tema minimalista
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajuste do ângulo dos rótulos no eixo x

#probabilidade do estado do paciente ser SP ou RJ

frequencia_estados <- table(dados$SG_UF_NOT)
prob_sp <- frequencia_estados["SP"] / sum(frequencia_estados)
prob_rj <- frequencia_estados["RJ"] / sum(frequencia_estados)


#qual a probabilidade dos pacientes que são de SP e RJ irem para o hospital só depois de 3 semanas de sintomas.

pacientes_sp_apos_3semanas <- sum(dados$SG_UF_NOT == "SP" & dados$SEM_PRI > 3)
pacientes_rj_apos_3semanas <- sum(dados$SG_UF_NOT == "RJ" & dados$SEM_PRI > 3)

total_sp <- sum(dados$SG_UF_NOT == "SP")
total_rj <- sum(dados$SG_UF_NOT == "RJ")

prob_sp <- pacientes_sp_apos_3semanas / total_sp
prob_rj <- pacientes_rj_apos_3semanas / total_rj

cat("Probabilidade de pacientes de SP irem ao hospital após 3 semanas de sintomas:", prob_sp, "\n")
cat("Probabilidade de pacientes de RJ irem ao hospital após 3 semanas de sintomas:", prob_rj, "\n")
