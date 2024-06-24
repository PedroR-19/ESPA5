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


#Qual a probabilidade das pessoas que demoram mais de 3 semanas e vem de SP serem homens?
pacientes_sp_mais_3semanas <- subset(dados, SG_UF_NOT == "SP" & SEM_PRI > 3 & CS_SEXO == "M")

total_sp_mais_3semanas <- sum(dados$SG_UF_NOT == "SP" & dados$SEM_PRI > 3)

probabilidade_sp_homem_mais_3semanas <- nrow(pacientes_sp_mais_3semanas) / total_sp_mais_3semanas



#homens e mulheres quem são de SP de demoraram mais de 3 semanas para irem ao hospital após os sintomas

pacientes_sp_mais_3semanas <- subset(dados, SG_UF_NOT == "SP" & SEM_PRI > 3)

contagem_sexo <- table(pacientes_sp_mais_3semanas$CS_SEXO)

sexos <- c("M", "F")

contagem_sexo <- contagem_sexo[sexos]
contagem_sexo[is.na(contagem_sexo)] <- 0

df_contagem <- data.frame(Sexo = names(contagem_sexo), Contagem = as.vector(contagem_sexo))

ggplot(data = df_contagem, aes(x = Sexo, y = Contagem, fill = Sexo)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidade de Homens e Mulheres de SP que Demoraram Mais de 3 Semanas para ir ao Hospital",
       x = "Sexo",
       y = "Quantidade de Pacientes") +
  scale_fill_manual(values = c("M" = "blue", "F" = "pink"))

#TESTE DE HIPOTEZE Hipótese: Existe uma diferença significativa na quantidade média de semanas com sintomas (SEM_PRI) entre pacientes de São Paulo (SP) e do Rio de Janeiro (RJ) que foram hospitalizados por SRAG.
#Hipótese Nula (H0): A média de SEM_PRI para pacientes de SP é igual à média de SEM_PRI para pacientes de RJ.
#Hipótese Alternativa (H1): A média de SEM_PRI para pacientes de SP é diferente da média de SEM_PRI para pacientes de RJ.

dados_sp <- subset(dados, SG_UF_NOT == "SP")
dados_rj <- subset(dados, SG_UF_NOT == "RJ")

teste_t <- t.test(dados_sp$SEM_PRI, dados_rj$SEM_PRI)
print(teste_t)

if (teste_t$p.value < 0.05) {
  cat("Rejeitamos a hipótese nula: há uma diferença significativa na quantidade média de semanas com sintomas entre pacientes de SP e RJ.\n")
} else {
  cat("Não rejeitamos a hipótese nula: não há diferença significativa na quantidade média de semanas com sintomas entre pacientes de SP e RJ.\n")
}
