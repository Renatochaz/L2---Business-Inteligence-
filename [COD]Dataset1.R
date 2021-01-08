### Load librarys and file ###
library (tidyverse)
library (lubridate)
library(readxl)
library(readr)
library(dplyr)


# vector month names

meses <- c("JANEIRO", "FEVEREIRO", "MARÇO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")

# DB principal

fat_agrup <- 0

# Read dbs and aggregate

for (i in 1:12) {
  
db_transi <- read_excel(paste0('G:/Meu Drive/Tactin/',
                              'Trabalhos - Análise de Dados/Lobo Brasil/',
                              '[DADOS]Lobo Brasil Olavo/',
                              'Relatoriórios faturamento/XLS faturamento/',
                              'FATURAMENTO ',meses[i],' 2019',".xls"), 
                        col_names = c("numero", "dia", "valor", "acrescimo", "A_D"), 
                        col_types = c("skip", "skip", "text", 
                                      "skip", "skip", "skip", "skip", "skip", 
                                      "text", "skip", "skip", "skip", 
                                      "numeric", "skip", "skip", "text", 
                                      "skip", "skip", "skip", "text", 
                                      "skip", "skip"))

fat_agrup <- rbind(fat_agrup, db_transi)

}

# Get dates vector

dias_func <- fat_agrup[,2]
dias_func[is.na(dias_func)] <- 0
dias_func <- as.data.frame(as.numeric(dias_func$dia))
dias_func <- subset(dias_func, `as.numeric(dias_func$dia)` != "0")
dias_func <- as.Date(dias_func$`as.numeric(dias_func$dia)`, origin= "1899-12-30")


# NA to 0

fat_agrup <- fat_agrup[ ,c(1,3:5)]
fat_agrup[is.na(fat_agrup)] <- 0


# Create date vector for matching information
#dias <- as.POSIXct(strptime(c("03/01/2019","04/01/2019","05/01/2019","06/01/2019","08/01/2019","09/01/2019","10/01/2019","11/01/2019","12/01/2019","13/01/2019","15/01/2019","16/01/2019","17/01/2019","18/01/2019","19/01/2019","20/01/2019","22/01/2019","23/01/2019","24/01/2019","25/01/2019","26/01/2019","27/01/2019","29/01/2019","30/01/2019","31/01/2019"),
#                            "%d/%m/%Y"), tz = "UTC")


# Remove 0 rows and other useless rows

fat_agrup <- subset(fat_agrup, numero != "0" & numero !="Canc= Venda cancelada" & numero != "Número")

# Code to insert dates in FATURAMENTO (main) table

fat_agrup$data <- 0
fat_agrup$data <- as.Date(fat_agrup$data, origin = "1899-12-30")
cont <- 1
cont_pedidos <- 0
qtd_ped_dia <- 0

for (i in 1:length(dias_func)) {
  
  for (j in cont:nrow(fat_agrup)) {
 
  if (fat_agrup$numero[j] != "Listados:") {
    fat_agrup$data[j] <- dias_func[i]
    cont <- cont +1
    cont_pedidos <- cont_pedidos +1
  } else {
    qtd_ped_dia[i] <- cont_pedidos
    cont_pedidos <- 0
    cont <- cont +1
    break()
  }
  }
}


# removing "listados" rows, adding total row AND changing acrescimo to add descount

fat_agrup <- subset(fat_agrup, numero != "Listados:")
fat_agrup$acrescimo <- as.numeric(fat_agrup$acrescimo)
fat_agrup$acrescimo <- ifelse (fat_agrup$A_D == "D", fat_agrup$acrescimo * (-1), fat_agrup$acrescimo)
fat_agrup$Total <- fat_agrup$valor + fat_agrup$acrescimo

# Load Db's with information of sales in CRÉDITO, DÉBITO e DINHEIRO

credito <- read_excel(paste0('G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[DADOS]Lobo Brasil Olavo/Relatoriórios faturamento/XLS faturamento/',
                             'VENDAS EM CREDITO ANO 2019',".XLS"), col_names = FALSE,
                      col_types = c("skip", "skip", "text","skip","skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "numeric","skip", "skip", "skip", "skip",
                                    "numeric", "skip"))

debito<- read_excel(paste0('G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[DADOS]Lobo Brasil Olavo/Relatoriórios faturamento/XLS faturamento/',
                             'VENDAS EM DEBITO ANO 2019',".XLS"), col_names = FALSE,
                      col_types = c("skip", "skip", "text","skip","skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "numeric","skip", "skip", "skip", "skip",
                                    "numeric", "skip"))

dinheiro <- read_excel(paste0('G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[DADOS]Lobo Brasil Olavo/Relatoriórios faturamento/XLS faturamento/',
                             'VENDAS EM dinheiro ANO 2019',".XLS"), col_names = FALSE,
                      col_types = c("skip", "skip", "text","skip","skip", "skip", "skip", "skip", 
                                    "skip", "skip", "skip", "numeric","skip", "skip", "skip", "skip",
                                    "numeric", "skip"))

                      
# Change NA to 0 and initialize new columns in principal table
credito[is.na(credito)] <- 0
debito[is.na(debito)] <- 0
dinheiro[is.na(dinheiro)] <- 0

fat_agrup$Valor_Credito <- 0
fat_agrup$Valor_Debito <- 0
fat_agrup$Valor_Dinheiro <- 0

fat_agrup$Porc_Credito <- 0
fat_agrup$Porc_Debito <- 0
fat_agrup$Porc_Dinheiro <- 0

# code for inserting information of paying in credito, debito and dinheiro
for (i in 1:nrow(fat_agrup)) {
  
  for (j in 1:nrow(credito)) {
    
  if (fat_agrup$numero[i] == credito$...1[j]) {
    
    fat_agrup$Valor_Credito[i] <- credito$...3[j+1]
    
  } else {
    
    next()
  }
  }
}
  
for (i in 1:nrow(fat_agrup)) {
  
  for (j in 1:nrow(debito)) {
    
    if (fat_agrup$numero[i] == debito$...1[j]) {
      
      fat_agrup$Valor_Debito[i] <- debito$...3[j+1]
      
    } else {
      
      next()
    }
    
  }
}

for (i in 1:nrow(fat_agrup)) {
  
  for (j in 1:nrow(dinheiro)) {
    
    if (fat_agrup$numero[i] == dinheiro$...1[j]) {
      
      fat_agrup$Valor_Dinheiro[i] <- dinheiro$...3[j+1]
      
    } else {
      
      next()
    }
  }
}
 

fat_agrup$Porc_Credito <- fat_agrup$Valor_Credito/fat_agrup$Total * 100
fat_agrup$Porc_Debito <- fat_agrup$Valor_Debito/fat_agrup$Total * 100
fat_agrup$Porc_Dinheiro<- fat_agrup$Valor_Dinheiro/fat_agrup$Total * 100

# Test consistency

porc_abs <- as.data.frame(fat_agrup$Porc_Credito + fat_agrup$Porc_Debito + fat_agrup$Porc_Dinheiro)
sum (porc_abs <= 99)

# Create new variables

fat_agrup$qtd_pedidos <- 0
fat_agrup$pedido_medio <- 0
fat_agrup$perca_recebivel <- 0
fat_agrup$perca_garcom <- 0
fat_agrup$porc_garcom <- 0

# Filling new variables

# Loss in RECEBIVEIS

taxa_antecipacao <- 0.0279
fat_agrup$perca_recebivel <- fat_agrup$Valor_Credito * taxa_antecipacao

# Perca garcom

fat_agrup$perca_garcom <- ifelse(fat_agrup$A_D == "A", (fat_agrup$Porc_Credito/100 * taxa_antecipacao) * (fat_agrup$acrescimo * 0.8), "0")

# Removing A_D

fat_agrup <- fat_agrup[-c(4)]

# Agrupando por dia

main_agrup <- fat_agrup
# as numeric
main_agrup$numero <- as.numeric(main_agrup$numero)
main_agrup$perca_garcom <- as.numeric(main_agrup$perca_garcom)
fat_agrup$perca_garcom <- as.numeric(fat_agrup$perca_garcom)
main_agrup <- main_agrup %>%
  group_by(data) %>%
  select(-c(numero)) %>%
  summarize_all(funs(sum))

# adjusting wrong scales

main_agrup$Porc_Credito <- main_agrup$Valor_Credito/main_agrup$Total * 100
main_agrup$Porc_Debito <- main_agrup$Valor_Debito/main_agrup$Total * 100
main_agrup$Porc_Dinheiro <- main_agrup$Valor_Dinheiro/main_agrup$Total * 100

# Test consistency

porc_abs_dia <- as.data.frame(main_agrup$Porc_Credito + main_agrup$Porc_Debito + main_agrup$Porc_Dinheiro)
sum (porc_abs_dia >= 99)

# add new columns to visualize problem of total - registrado

main_agrup$total_registrado <- 0
main_agrup$dif_fatur_regist <- 0

main_agrup$total_registrado <- main_agrup$Valor_Credito + main_agrup$Valor_Debito + main_agrup$Valor_Dinheiro
main_agrup$dif_fatur_regist <- round(main_agrup$Total - main_agrup$total_registrado, digits = 2)

# filling quantidade de pedidos e media por pedido

main_agrup$qtd_pedidos <- qtd_ped_dia
main_agrup$pedido_medio <- main_agrup$valor/main_agrup$qtd_pedidos

# Filling % garcom dia

main_agrup$porc_garcom <- round(main_agrup$valor/main_agrup$acrescimo, digits = 2)

# rounding

fat_agrup[c(9:11,14:15)] <- round(fat_agrup[c(9:11,14:15)] , digits = 2)
main_agrup[c(8:10,12:14)] <- round(main_agrup[c(8:10,12:14)], digits = 2)

# changing order, renaming and saving
main <- 0
submain <- fat_agrup[ ,c(4,6:11,2,5,3,14:15)]
main <- main_agrup[, c(1,2,4,3,5:7,16:17,8:10,11:12,15,13:14)]                    

submain <- submain %>%
  rename(
    Dia = 1,
    Faturamento_Pedido = 8,
    Valor_Final = 9,
    Adicional_Garcom = 10,
    Perca_Adiantamento = 11,
    Perca_Garcom = 12)

main <- main %>%
  rename(
    Dia = 1,
    Faturamento_Pedido = 2,
    Valor_Final = 3,
    Adicional_Garcom = 4,
    Total_Registrado = 8,
    Diferenca_Final_Registrado = 9,
    QTD_Pedidos = 13,
    Media_Pedido = 14,
    Porc_Garcom = 15,
    Perca_Adiantamento = 16,
    Perca_Garcom = 17)

# Saving

### IF RUNNING CODE FROM START CHECK TO SEE IF TYPE D IS DISCOUNTING ACCORDINGLY ###
### MAIN AND SUBMAIN SEENS TO BE OK ###

# CREATING PROFIT WITH DEBT/CREDIT RATES DISCOUNTED: 1.59%/2.09% AND 8% GARCOM / 2% C
# ONLY MAIN TABLE #
main$faturamento_real <- 0
main$faturamento_real <- round (main$Valor_Final - (main$Adicional_Garcom * 0.8) - main$Perca_Adiantamento - main$Perca_Garcom - (main$Valor_Credito * 0.0209) - (main$Valor_Debito * 0.0159), digits = 2)
main <- main[c(1:2,5,4,6:18,3)]

#ADJUSTING % GARCOM
main$Porc_Garcom <- round(main$Adicional_Garcom/main$Faturamento_Pedido, digits = 3)

# saving

nomes_tabelas <- c("Faturamento_desagrupado", "Faturamento_agrupado_por_dia")
tab21 <- submain
tab22 <- main

for (i in 1:2) {
  write.csv(get(paste0('tab2',i)),paste0('G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Lobo Brasil/',
                                   '[TABELAS]Prontas R/',nomes_tabelas[i],".csv"),
            row.names = FALSE)
}

### END
  

## EDIT FOR MEDIA FATURAMENTAL MENSAL/DIA DA SEMANA

main_edit <- main
main_edit$mes <- 0
main_edit$d_semana <- 0

str (main_edit$Dia)

main_edit$Dia <- as.POSIXlt(main_edit$Dia)

main_edit$d_semana <-  weekdays(main_edit$Dia)
main_edit$mes <- month(main_edit$Dia, label = TRUE)

## This is NOT going to be ugly

meses_lubri <- c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
dsemana_lubri <- c("segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira","sábado","domingo")

fatu_mensal <- 0
fatu_dia_semana <- 0

for (i in 1:length(meses_lubri)) {
  
fatu_mensal[i] <- sum(main_edit$Valor_Final[which(main_edit$mes == meses_lubri[i])])

}
media_mensal <- sum(fatu_mensal)/length(meses_lubri)
media_mensal


for (i in 1:length(dsemana_lubri)) {
  
  fatu_dia_semana[i] <- sum(main_edit$Valor_Final[which(main_edit$d_semana == dsemana_lubri[i])])

}

fatu_dia_semana

dias_funcionando_semana <- 0

for (i in 1:length(dsemana_lubri)) {
  
  dias_funcionando_semana[i] <- length(which(main_edit$d_semana == dsemana_lubri[i]))
  
}

dias_funcionando_semana

media_dia_semana <- 0

for (i in 1:length(dsemana_lubri)) {
  
  media_dia_semana[i] <- fatu_dia_semana[i] / dias_funcionando_semana[i]
  
}

quantidade_pedidos_dsemana <- 0

for (i in 1:length(dsemana_lubri)) {
  
  quantidade_pedidos_dsemana[i] <- sum(main_edit$QTD_Pedidos[which(main_edit$d_semana == dsemana_lubri[i])])
  
}


media_pedidos_dsemana <- round (quantidade_pedidos_dsemana /  dias_funcionando_semana, digits = 0)


fatu_semanal <- data.frame(dsemana_lubri,dias_funcionando_semana, fatu_dia_semana, media_dia_semana, media_pedidos_dsemana)
colnames(fatu_semanal) <- c("Dia da semana", "Aberto", "Faturamento", "Média Faturamento", "Média de pedidos")

fatu_semanal
