#################### REGRESSÃO LOGÍSTICA MULTINOMIAL
#################### Script adaptado de Fernanda Pereira (Unifesp) e Vitor Alcantara (USP) 
#################### Última versão: 26/07/2024

######## PASSO 1. Pacotes ########

install.packages("pacman")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("misty")
install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("Factoshiny")
install.packages("knitr")
install.packages("hrbrthemes")
install.packages("ggridges")
install.packages("forcats")
install.packages("mlogit")

library(mlogit)
library(ggridges)
library(ggplot2)
library(knitr)
library(dplyr)
library(misty)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(pacman)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

pacman::p_load(rio, psych, descr, gridExtra, ggpubr, scales, 
               patchwork, effects, reshape2, foreign, sjPlot,
               pROC,ROCR,stargazer, caret,car, nnet, AER, 
               lmtest, gtsummary, DescTools)

######## PASSO 2. Banco de dados ########

# 2.1 SAEB 2021
s21 <- read.csv2("C:\\Users\\guilh\\Desktop\\SAEB2021_ALUNO.csv", 
                 stringsAsFactors=F)

# 2.2 Selecionar apenas questionários respondidos
dados <- dplyr::filter(s21, IN_PREENCHIMENTO_QUESTIONARIO %in% "1")

# 2.3 Selecionar apenas questionários de São Paulo
dados <- dplyr::filter(dados, ID_UF %in% "35")

# 2.4 Selecionar as variáveis de interesse para o modelo
dados_reg <- select(.data=dados,
                     TX_RESP_Q21,  # Expectativa chr
                     TX_RESP_Q07,  # Escolaridade da mãe chr
                     TX_RESP_Q04,  # Cor ou raça chr
                     TX_RESP_Q01,  # sexo chr
                     IN_PUBLICA,   # Categoria administrativa atual
                     TX_RESP_Q17,  # Categoria administrativa predominante
                     INSE_ALUNO)   # NSE

# 2.5 Renomear as variáveis
dados_reg <- rename(.data = dados_reg,
                    "expectativa"=TX_RESP_Q21,
                    "mãe"=TX_RESP_Q07,
                    "raça"=TX_RESP_Q04,
                    "sexo"=TX_RESP_Q01,
                    "categoria_administrativa"=IN_PUBLICA,
                    "categoria"=TX_RESP_Q17,
                    "NSE"=INSE_ALUNO)

# 2.6 Converter NSE para variável numérica
dados_reg$NSE <- as.numeric(dados_reg$NSE) 
class(dados_reg$NSE)

# 2.7 Filtrar valores de variáveis
dados_reg <- dplyr::filter(.data=dados_reg,
                            expectativa %in% c("A", "B", "C", "D"), # Tiramos C por ser muito desbalanceado?
                            mãe %in% c("A", "B", "C", "D", "E"),
                            raça %in% c("A", "B", "C", "D", "E"),
                            sexo %in% c("A", "B"),
                            categoria_administrativa %in% c("0", "1"),
                            categoria %in% c("A", "B", "C"))

# 2.8 Eliminar valores nulos
anyNA(dados_reg)
dados_reg <- na.omit(dados_reg)
anyNA(dados_reg)

# NOTA: Até aqui, temos 225.821 estudantes e 7 variáveis

# 2.9 Renomear os valores de cada categoria
dados_reg$expectativa <- dplyr::recode(dados_reg$expectativa,
                                        "A"="Estudar",
                                        "B"="Trabalhar",
                                        "C"="Estudar e Trabalhar",
                                        "D"="Não sabe")

dados_reg$raça <- dplyr::recode(dados_reg$raça,
                                 "A"="Branca",  # Somamos brancos e amarelos
                                 "B"="PPI",     # Somamos pretos, pardos e indígenas
                                 "C"="PPI",
                                 "D"="Branca",
                                 "E"="PPI") 

dados_reg$mãe <- dplyr::recode(dados_reg$mãe,
                                "A"="Fund-",     # Não completou o 5º ano do EF
                                "B"="Fund-",     # EF incompleto
                                "C"="Fund+",     # EF completo
                                "D"="Médio+",    # EM completo
                                "E"="Superior+") # Superior completo 

dados_reg$sexo <- dplyr::recode(dados_reg$sexo,
                                 "A"="Masculino", 
                                 "B"="Feminino") 

# 2.9 Solucionar o problema das categorias administrativas
table(dados_reg$categoria_administrativa)
# Privada = 553, Pública = 225.268
table(dados_reg$categoria)
# Privada = 6.657, Pública e privada = 30.899, Pública = 188.265

dados_reg$categoria <- dplyr::recode(dados_reg$categoria,
                                     "A"="Pública",
                                     "B"="Privada",
                                     "C"="Privada")

dados_reg$categoria_administrativa <- dplyr::recode(dados_reg$categoria_administrativa,
                                      "1"="Público", 
                                      "0"="Privado") 

# Usaremos "categoria" ao invés de "categoria admin"

# 2.10 Criar uma variável de NSE

dados_reg <- mutate(.data = dados_reg,
                     faixasNSE = case_when(
                       NSE < 4.36 ~ "I",
                       NSE >= 4.36 & NSE < 5.44 ~ "II",
                       NSE >= 5.44 & NSE < 6.52 ~ "III",
                       NSE >= 6.52 ~ "IV"))

# 2.11 Transformamos em Factor
glimpse(dados_reg)
dados_reg <-strings2factors(dados_reg)
glimpse(dados_reg)

######## PASSO 3. Frequências ########

summary(dados_reg)
describe(dados_reg$NSE)

######## PASSO 4. Checagem das categorias de referência ########

levels(dados_reg$expectativa)  # Estudar e Trabalhar = categoria de referência
dados_reg$expectativa <- relevel(dados_reg$expectativa, ref="Estudar e Trabalhar")
levels(dados_reg$raça)      # Branca = categoria de referência
levels(dados_reg$mãe)       # Superior+ = categoria de referência
dados_reg$mãe <- relevel(dados_reg$mãe, ref="Superior+")
levels(dados_reg$faixasNSE) # Alta = categoria de referência
levels(dados_reg$categoria) # Privada = categoria de referência
levels(dados_reg$sexo)      # Masculino = categoria de referência
dados_reg$sexo <- relevel(dados_reg$sexo, ref="Masculino")
levels(dados_reg$faixasNSE)
dados_reg$faixasNSE <- relevel(dados_reg$faixasNSE, ref="IV")

######## PASSO 5. Checagem dos pressupostos ########

# A. A variável dependente é nominal? SIM

# B. Independência das observações (sem medidas repetidas): SIM

# C. Testar um modelo de regressão linear para testar multicolinearidade
m <- lm(as.numeric(expectativa) ~ sexo + mãe + raça + categoria + faixasNSE, data=dados_reg)
car::vif(m) # O resultado significa que não possui multicolinearidade

# D. Independência de alternativas irrelevantes (teste Hausman-McFadden):
# p.s.: Se não existisse uma das alternativas, teríamos a mesma coisa.

######## PASSO 6. Modelo ########

# Construção do modelo e modelo nulo (usando pacote nnet)
mod <- multinom(expectativa ~ sexo + mãe + raça + categoria + faixasNSE, data=dados_reg, model=TRUE)
mod0 <- multinom(expectativa ~ 1, data=dados_reg, model=TRUE) # Previsor

# Ajuste de modelo 
anova(mod, mod0)  # p<0.05 O modelo nulo é, logo, diferente do alternativo! Bom.

# Efeitos gerais
car::Anova(mod, type = "II", test="Wald")
# Se o Pr(>Chisq) for maior que 0.5, não é bom preditor... mas todos aqui são!

# Efeitos específicos
summary(mod)

# Obtenção dos valores de p por Wald (lmtest)
lmtest::coeftest(mod)
# 1) Coeficiente!
# 2) Ignoramos intercepts
# 3) Coeficientes positivos que aquele valor, quando comparados com a referência,
# ...signfica que tem mais chances de assinalar no valor ao seu lado do que a de 
# ...referência

# Obtenção das razões de chance com IC 95% (usando log-likelihood)
exp(coef(mod)) 
# As chances de assinalar Trabalhar é 1,29 vezes maior para uma pessoa preta do que
# para uma pessoa branca

exp(confint(mod)) #IC

# Tabela completa (pacote gtsummary)
gtsummary::tbl_regression(mod, exponentiate = FALSE)
gtsummary::tbl_regression(mod, exponentiate = TRUE)

######## PASSO 7. OPCIONAL: Modelos alternativos ########

# e.g., sem cor ou raça e sem sexo
mod2 <- multinom(expectativa ~ mãe + categoria + faixasNSE + sexo, data=dados_reg, model=T)

anova(mod2, mod0) # Ajuste do modelo 
DescTools::PseudoR2(mod2, which = "Nagelkerke")
Anova(mod2, type="II", test="Wald") # Efeitos gerais
summary(mod2)
lmtest::coeftest(mod2)
exp(coef(mod2))
exp(confint(mod2)) #IC
gtsummary::tbl_regression(mod2, exponentiate = TRUE)

######## PASSO 8. OPCIONAL: Comparando modelos ########

# AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)

# Qui-quadrado
anova(mod2, mod, test = "Chisq")

######## PASSO 9. Tabelas e gráficos ########

# 9.1 Pelo modelo 1
tab <- table(Observado =dados_reg$expectativa, Previsto = predict(mod))
prop.table(tab)*100

acuracia = sum(diag(tab)) / sum(tab)
acuracia

caret::confusionMatrix(predict(mod2), dados_reg$expectativa)
caret::confusionMatrix(predict(mod), dados_reg$expectativa)

# 9.2 Pelo modelo 2
tab <- table(Observado =dados_reg$expectativa, Previsto = predict(mod2))
prop.table(tab)*100

acuracia = sum(diag(tab)) / sum(tab)
acuracia

caret::confusionMatrix(predict(mod2), dados_reg$expectativa)
caret::confusionMatrix(predict(mod), dados_reg$expectativa)


# 9.3 Predições do primeiro modelo
predictions <- predict(mod, type = "probs", se=T)
dados_prev <- cbind(dados_reg[c("mãe", "raça", "sexo", "categoria_administrativa", "categoria", "NSE", "faixasNSE")], predictions)

dados_prev_long <- reshape2::melt(dados_prev,
                                       id.vars = c("mãe", "raça", "sexo", "categoria_administrativa", "categoria", "NSE", "faixasNSE"),
                                       value.name = "Probabilidades",
                                       variable.name="Expectativa")

# 9.4 Plotagens do primeiro modelo 
reg_categoria <- ggplot(dados_prev_long, aes(x= NSE, y = Probabilidades, color = categoria))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_categoria)

reg_sexo <- ggplot(dados_prev_long, aes(x= NSE, y = Probabilidades, color = sexo))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_sexo)

reg_raça <- ggplot(dados_prev_long, aes(x= NSE, y = Probabilidades, color = raça))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_raça)

reg_mãe <- ggplot(dados_prev_long, aes(x= NSE, y = Probabilidades, color = mãe))+
  geom_smooth(method="gam", linewidth=0.8)+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  scale_color_discrete(breaks=c("Superior+", 'Médio+', 'Fund+', 'Fund-'))+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_mãe)

reg_nse <- ggplot(dados_prev_long, aes(x= NSE, y = Probabilidades, color = faixasNSE))+
  geom_smooth(method="gam", linewidth=0.8)+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  scale_color_discrete(breaks=c("IV", 'III', 'II', 'I'))+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_nse)


# 9.5 Predições do segundo modelo
predictions2 <- predict(mod2, type = "probs", se=T)
dados_prev2 <- cbind(dados_reg[c("mãe", "raça", "sexo", "categoria_administrativa", "categoria", "NSE", "faixasNSE")], predictions)

dados_prev_long2 <- reshape2::melt(dados_prev2,
                                  id.vars = c("mãe", "raça", "sexo", "categoria_administrativa", "categoria", "NSE", "faixasNSE"),
                                  value.name = "Probabilidades",
                                  variable.name="Expectativa")

# 9.6 Plotagens do segundo modelo 
reg_categoria2 <- ggplot(dados_prev_long2, aes(x= NSE, y = Probabilidades, color = categoria))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_categoria2)

reg_sexo2 <- ggplot(dados_prev_long2, aes(x= NSE, y = Probabilidades, color = sexo))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_sexo)

reg_raça <- ggplot(dados_prev_long2, aes(x= NSE, y = Probabilidades, color = raça))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_raça)

reg_mãe <- ggplot(dados_prev_long2, aes(x= NSE, y = Probabilidades, color = mãe))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_mãe)

reg_nse <- ggplot(dados_prev_long2, aes(x= NSE, y = Probabilidades, color = faixasNSE))+
  geom_smooth(method="gam", linewidth=0.5)+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
plot(reg_nse)
