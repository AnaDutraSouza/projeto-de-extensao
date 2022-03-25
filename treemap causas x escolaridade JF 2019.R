library(tidyverse)
library(dplyr)
library(treemap)
library(read.dbc)
library(ggplot2)

#### A importação da base de dados foi feita atraves do pacote read.dbc

setwd("C:\\Users\\anadu\\OneDrive\\Área de Trabalho\\Economia\\dados")

library(read.dbc)
dados_2019 <- read.dbc("DOMG2019.dbc")

### Selecionando as variáveis de interesse
dados_sim_2019 <- dados_2019 %>%
  select(CODMUNNATU, CAUSABAS, ESC, SEXO) %>%
  na.omit()

### Selecionando apenas o municipio de juiz de fora, ccodigo IBGE: 3136702
jf_sim_2019 <- dados_sim_2019[dados_sim_2019$CODMUNNATU == "313670",]

### Raking causas de mortalidade mais frequentes
freq <- table(jf_sim_2019$CAUSABAS)
sortfreq <- sort(freq, decreasing = T)
sortfreq

### Lendo a coluna escolaridade como numérica
esc <- as.numeric(as.character(jf_sim_2019$ESC))
jf_sim_2019 <- cbind(jf_sim_2019, esc)

### Excluindo colunas duplicadas no data.frame
jf_sim_dois$mun <- NULL
jf_sim_2019$ESC <- NULL

### Decodificando a variável sexo
jf_sim_2019$SEXO <- factor(jf_sim_2019$SEXO, label = c("Masculino", "Feminino"), levels= c(1,2))

##### I10 - Hipertensão primária
hipertensao_2019 <- jf_sim_2019[jf_sim_2019$CAUSABAS == "I10",]
treemap_hipertensao_2019 <- treemap(hipertensao_2019, index = "esc", vSize = "esc", title = "Hipertensão",border.col = "white")

##### I219 - infarto #######
infarto_2019 <- jf_sim_2019[jf_sim_2019$CAUSABAS == "I219",]

treemap_infarto_2019 <- treemap(infarto_2019, index = "esc", vSize = "esc", title = "Infarto", border.col = "white",type = "index",fun.aggregate = "weighted.mean" )

##### J189 - Pneumonia
pneumonia_2019 <- jf_sim_2019[jf_sim_2019$CAUSABAS == "J189",]

treemap_pneumonia_2019 <- treemap(pneumonia_2019, index = "esc", vSize= "esc", title = "Pneumonia", border.col = "white")

##### G309 - Alzheimer
alzheimer_2019 <- jf_sim_2019[jf_sim_2019$CAUSABAS == "G309",]
treemap_alzheimer_201 <- treemap(alzheimer_2019, index = "esc", vSize = "esc", title = "Alzheimer", border.col = "white")

### A419 - Septicemia
septicemia_2019 <- jf_sim_2019[jf_sim_2019$CAUSABAS == "A419",]
treemap_septicemia_2019 <- treemap(septicemia_2019, index = "esc", vSize = "esc", title = "Septicemia", border.col = "white")

### I10, I219, J189, G309, A419
