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
install.packages("GDAtools")
install.packages("frequency")
install.packages("explor")
install.packages("haven")

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
library(explor)
library(GDAtools)
library(frequency)
library(readxl)

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
dados_acm <- select(.data=dados,
                    TX_RESP_Q21,  # Expectativa chr
                    TX_RESP_Q07,  # Escolaridade da mãe chr
                    TX_RESP_Q04,  # Cor ou raça chr
                    TX_RESP_Q01,  # sexo chr
                    IN_PUBLICA,   # CATEGORIA QUE SERÁ ELIMINADA
                    TX_RESP_Q17,  # Categoria administrativa predominante
                    INSE_ALUNO)   # NSE

# 2.5 Renomear as variáveis
dados_acm <- rename(.data = dados_acm,
                    "expectativa"=TX_RESP_Q21,
                    "mãe"=TX_RESP_Q07,
                    "raça"=TX_RESP_Q04,
                    "sexo"=TX_RESP_Q01,
                    "categoria"=TX_RESP_Q17,
                    "NSE"=INSE_ALUNO)

# 2.6 Converter NSE para variável numérica
dados_acm$NSE <- as.numeric(dados_acm$NSE) 
class(dados_acm$NSE)

# 2.7 Filtrar valores de variáveis
dados_acm <- dplyr::filter(.data=dados_acm,
                           expectativa %in% c("A", "B", "C", "D"), # Tiramos C por ser muito desbalanceado?
                           mãe %in% c("A", "B", "C", "D", "E"),
                           raça %in% c("A", "B", "C", "D", "E"),
                           sexo %in% c("A", "B"),
                           IN_PUBLICA %in% c("0", "1"),
                           categoria %in% c("A", "B", "C"))

# 2.8 Eliminar valores nulos
anyNA(dados_acm)
dados_acm <- na.omit(dados_acm)
anyNA(dados_acm)

# NOTA: Até aqui, temos 225.821 estudantes e 7 variáveis

# 2.9 Renomear os valores de cada categoria
dados_acm$expectativa <- dplyr::recode(dados_acm$expectativa,
                                       "A"="Estudar",
                                       "B"="Trabalhar",
                                       "C"="Estudar e Trabalhar",
                                       "D"="Não sabe")

dados_acm$raça <- dplyr::recode(dados_acm$raça,
                                "A"="Branca",  # Somamos brancos e amarelos
                                "B"="PPI",     # Somamos pretos, pardos e indígenas
                                "C"="PPI",
                                "D"="Branca",
                                "E"="PPI") 

dados_acm$mãe <- dplyr::recode(dados_acm$mãe,
                               "A"="Fund-",     # Não completou o 5º ano do EF
                               "B"="Fund-",     # EF incompleto
                               "C"="Fund+",     # EF completo
                               "D"="Médio+",    # EM completo
                               "E"="Superior+") # Superior completo 

dados_acm$sexo <- dplyr::recode(dados_acm$sexo,
                                "A"="Masculino", 
                                "B"="Feminino") 

dados_acm <- mutate(.data = dados_acm,
                     NSE = case_when(
                       NSE < 4.36 ~ "I",
                       NSE >= 4.36 & NSE < 5.44 ~ "II",
                       NSE >= 5.44 & NSE < 6.52 ~ "III",
                       NSE >= 6.52 ~ "IV"))


# 2.9 Solucionar o problema das categorias administrativas
table(dados_acm$IN_PUBLICA)
# Privada = 553, Pública = 225.268
table(dados_acm$categoria)
# Privada = 6.657, Pública e privada = 30.899, Pública = 188.265

dados_acm$categoria <- dplyr::recode(dados_acm$categoria,
                                     "A"="Pública",
                                     "B"="Privada",
                                     "C"="Privada")
# Retirar IN_PUBLICA
dados_acm <- select(.data=dados_acm,
                    expectativa,  # Expectativa chr
                    mãe,  # Escolaridade da mãe chr
                    raça,  # Cor ou raça chr
                    sexo,  # sexo chr
                    categoria,  # Categoria administrativa predominante
                    NSE)   # NSE

# Fatorizar
dados_acm <-strings2factors(dados_acm)
glimpse(dados_acm)
summary(dados_acm)


######## PASSO 3. Análise ########

# substituir res.ACM_s21_NSE_SUP
res.dados_acm <- MCA(dados_acm,
                           quali.sup = 1,
                           graph = FALSE)

# Segunda etapa: Análise dos resultados estatísticos
res.dados_acm$eig      # Resultados dos eixos
res.dados_acm$var$eta2 # Resultados das colunas
res.dados_acm$var$cos2 # Resultados das variáveis
res.dados_acm$var$v.test
res.dados_acm$var$coord
res.dados_acm$var$contrib


# Terceira etapa: Construção de tabelas

# Criando tabelas no R
tab1_s21 <- rownames_to_column(as.data.frame(res.dados_acm$eig))
tab2_s21 <- rownames_to_column(as.data.frame(res.dados_acm$var$eta2))
tab3_s21 <- rownames_to_column(as.data.frame(res.dados_acm$var$contrib))

# Renomeando as colunas
names(tab1_s21)[1] <- "Dimensão"
names(tab1_s21)[2] <- "Eigenvalue"
names(tab1_s21)[3] <- "Variância (%)"
names(tab1_s21)[4] <- "Variância acumulada (%)"
names(tab2_s21)[1] <- "Colunas"
names(tab3_s21)[1] <- "Variáveis"

# Exportar!
write.xlsx(tab1_s21, file="FSL0540_SAEB21_Tab1.xlsx")
write.xlsx(tab2_s21, file="FSL0540_SAEB21_Tab2.xlsx")
write.xlsx(tab3_s21, file="FSL0540_SAEB21_Tab3.xlsx")

# Quarta etapa: Gráficos!


SAEB2021_ACM <- fviz_mca_var(res.dados_acm,   #SAEB2021
                             col.var = "contrib", # Cor muda conforme contribuição da variáv.
                             repel=T, # Afasta os rótulos originais para + visual.
                             axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)

SAEB2021_ACM

# OU

explor(res.dados_acm)


