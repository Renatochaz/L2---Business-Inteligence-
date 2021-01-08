### Load librarys and file ###
library (tidyverse)
library (lubridate)
library(readxl)
library(readr)

### READ DB - Vendas por Grupo 2019 ###
                                   
Vendas_por_grupo_2019 <- read_excel("G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Lobo Brasil/[DADOS]Lobo Brasil Olavo/Relatoriórios faturamento/XLS faturamento/Vendas por grupo 2019.Xls", skip = 3)

# Usar colunas 2,6,7
db_vendas_grupo <- Vendas_por_grupo_2019 %>% select(2,6:7)

# Format numeric to 2 decimals < TO DO
#db_vendas_grupo[2:3==] <- formattable(db_vendas_grupo[2:3], digits = 2, format = "f")

# Rename columns, sort places and add new columns
db_vendas_grupo <- db_vendas_grupo %>%
  rename(
    item = 1,
    qtd_vendida = 2,
    faturamento = 3)

db_vendas_grupo$grupo <- 0
db_vendas_grupo$faturamento_porcentagem <- 0

db_vendas_grupo <- db_vendas_grupo[c(4,1,2,3,5)]

# Define groups and drop useless rows
# 1 = Petiscos e porções, 2 = Pratos, 3 = Bebidas, 4 = Sobremesas
db_vendas_grupo[c(5:30,37:39,46:47),1] <- "Petiscos/Porções"
db_vendas_grupo[c(54:56,58:98),1] <- "Pratos"
db_vendas_grupo[c(105:114,116:120,123:138,141:151,154:156,159:161,164:168,170:178,181:192,195:212,215:220,223,225:249),1] <- "Bebidas"
db_vendas_grupo[c(256:267),1] <- "Sobremesas"

db_vendas_grupo <- subset(db_vendas_grupo, grupo != "0")

# round decimals and comput faturamento per group
db_vendas_grupo$qtd_vendida <- as.numeric(db_vendas_grupo$qtd_vendida)
db_vendas_grupo$faturamento <- as.numeric(db_vendas_grupo$faturamento)
db_vendas_grupo$qtd_vendida <- round(db_vendas_grupo$qtd_vendida, digits = 2)

pets <- sum(db_vendas_grupo$grupo == "Petiscos/Porções")
prat <- sum(db_vendas_grupo$grupo == "Pratos")
beb <- sum(db_vendas_grupo$grupo == "Bebidas")
sob <- sum(db_vendas_grupo$grupo == "Sobremesas")

fat_pets <- sum(db_vendas_grupo$faturamento[1:31])
fat_prat <- sum(db_vendas_grupo$faturamento[32:75])
fat_beb <- sum(db_vendas_grupo$faturamento[76:199])
fat_sob <- sum(db_vendas_grupo$faturamento[200:211])


for (i in 1:nrow(db_vendas_grupo)) {
  
  if (db_vendas_grupo$grupo[i] == "Petiscos/Porções"){
    db_vendas_grupo$faturamento_porcentagem[i] <- db_vendas_grupo$faturamento[i]/fat_pets * 100
  }
}

for (i in 1:nrow(db_vendas_grupo)) {
  
  if (db_vendas_grupo$grupo[i] == "Pratos"){
    db_vendas_grupo$faturamento_porcentagem[i] <- db_vendas_grupo$faturamento[i]/fat_prat * 100
  }
}

for (i in 1:nrow(db_vendas_grupo)) {
  
  if (db_vendas_grupo$grupo[i] == "Bebidas"){
    db_vendas_grupo$faturamento_porcentagem[i] <- db_vendas_grupo$faturamento[i]/fat_beb * 100
  }
}

for (i in 1:nrow(db_vendas_grupo)) {
  
  if (db_vendas_grupo$grupo[i] == "Sobremesas"){
    db_vendas_grupo$faturamento_porcentagem[i] <- db_vendas_grupo$faturamento[i]/fat_sob * 100
  }
}

db_vendas_grupo$faturamento_porcentagem <- round(db_vendas_grupo$faturamento_porcentagem, digits = 2)

# saving

write.csv(db_vendas_grupo,paste0('G:/Meu Drive/Tactin/',
                               'Trabalhos - Análise de Dados/Lobo Brasil/',
                               '[TABELAS]Prontas R/Vendas_por_grupo.csv'), row.names = FALSE)
