library (tidyverse)
library (lubridate)
library(readxl)
library(readr)

pratos_mc <- subset(Vendas_por_grupo, Vendas_por_grupo$grupo == "Pratos")


# Faturamento de pratos está com 10% incluso, subtraindo
# Foi tirado a média anual de % inclusa do garçom, que é de 8.39% > sum(main$Porc_Garcom)/nrow(main)

#pratos_mc$faturamento_real <- pratos_mc$faturamento*0.916

# A partir do faturamento real, calcular o faturamento do prato, incluido valor do garçom

pratos_mc$faturamento_prato <- round(pratos_mc$faturamento / pratos_mc$qtd_vendida,digits = 2)

# rounding

#pratos_mc$preco_prato_1 <- round(pratos_mc$preco_prato, digits = 0)

# Custo variavel (sem 10% de segurança)

pratos_mc$custo_variavel <- 0

### adicionando informações

# vetor custos por porção de prato sem margem de segurança

v_nome_prato <- c("BRASILEIRO","MOQUECA DE BADEJO","ARROZ DE HAUCA","PICANHA DO LOBO","PIRARUCU",
                  "BOBO DE PALMITO PUPUNHA","BOBO DE CAMARAO","FILE MIGNON JABUTICABA",
                  "FILE MIGNON DO LOBO","	PEITO DE PATO  MANTEIGA LOBO BRASIL",
                  "PEITO DE PATO AO MOLHO DE TUCUPI","CAMARAO BAIANO","BADEJO AO MOLHO DE MOQUECA",
                  "BADEJO AO MOLHO DE CAMARAO","CAMARAO DO LOBO","FILE SUINO DO CHEF",
                  "PATO NO TUCUPI","FILE MIGNON NA CERVEJA","ARROZ DO MAR","GALINHADA COM PEQUI",
                  "TRIO RAIZ","BAIAO DE DOIS")

v_preco_prato <- c(5.4,10.3,9.47,17.18,8.51,8.7,12.34,10.9,12.59,14.13,15.94,23.41,13.75,12.21,
                   22.63,8.66,10.51,10.77,19.97,2.87,10.49,6.37)

precos_pratos <- data.frame(cbind(v_nome_prato,v_preco_prato))  

precos_pratos$v_nome_prato <- as.character(precos_pratos$v_nome_prato)
precos_pratos$v_preco_prato <- as.numeric(as.character(precos_pratos$v_preco_prato))

# adicionando informacoes a tabela

pratos_mc$custo_prato <- 0

for (i in 1:nrow(pratos_mc)) {
  
  for (j in 1:nrow(precos_pratos)) {
    
    pratos_mc$custo_prato[i] <- ifelse(pratos_mc$item[i] == precos_pratos$v_nome_prato[j],
                                       precos_pratos$v_preco_prato[j],pratos_mc$custo_prato[i])
  }
}

margem_pratos <- subset(pratos_mc, pratos_mc$custo_prato != 0)


# Calculando valor total envolvido na preparação dos pratos

# extra cozinhas funcionários extras
val_prat_ext_coz <- subset (final_extras, final_extras$Tipo_despesa == "extra_cozinha")

# extra cozinha funcionários fixos
val_prat_fix_ext_coz <- subset (final_fixos, final_fixos$Tipo_despesa == "extra_cozinha")

# valor cris coletti
fat_nomes <- tbl_fixos
fat_nomes$`1FORNECEDOR` <- as.factor(fat_nomes$`1FORNECEDOR`)
levels(fat_nomes$`1FORNECEDOR`)

val_prat_fix_chefe <- subset(tbl_fixos, tbl_fixos$`1FORNECEDOR` == "Cris Colete" | tbl_fixos$`1FORNECEDOR` == "Cris Collete" | tbl_fixos$`1FORNECEDOR` == "Cristiano Coletti" | tbl_fixos$`1FORNECEDOR` == "Cristiano Colletti" | tbl_fixos$`1FORNECEDOR` == "Cris Colleti" | tbl_fixos$`1FORNECEDOR` == "Cristiano Colleti")  

val_prat_fix_chefe_2 <- subset(val_prat_fix_chefe, val_prat_fix_chefe$`2CATEGORIA` != "extra_cozinha")
val_prat_fix_chefe_2 <- subset(val_prat_fix_chefe_2, val_prat_fix_chefe_2$`2CATEGORIA` != "rescisao")

# somando tudo para valor total

sum(val_prat_ext_coz$Valor)
sum(val_prat_fix_ext_coz$Valor)
sum(val_prat_fix_chefe_2$`6VALOR PAGO`)

valor_chefe <- sum(val_prat_ext_coz$Valor) + sum(val_prat_fix_ext_coz$Valor) + sum(val_prat_fix_chefe_2$`6VALOR PAGO`)

# faltando valor_chefe

valor_chefe_por_prato <- valor_chefe / sum(pratos_mc$qtd_vendida)

margem_pratos$custo_variavel <- round(margem_pratos$custo_prato + valor_chefe_por_prato, digits = 2)

margem_pratos$margem_contribuicao <- margem_pratos$faturamento_prato - margem_pratos$custo_variavel - (0.08*margem_pratos$faturamento_prato)

margem_pratos$porc_margem_contribuicao <- round(margem_pratos$margem_contribuicao / margem_pratos$faturamento_prato, digits = 2)


# Limpando df para ficar clean

margem_pratos <- margem_pratos[c(2,6:10)]

margem_pratos <- margem_pratos[c(1:2,5:6,3)]

# Saving

write.csv(margem_pratos,paste0('G:/Meu Drive/Tactin/',
                              'Trabalhos - Análise de Dados/Lobo Brasil/',
                              '[TABELAS]Prontas R/[Margem]pratos_principais.csv'), row.names = FALSE)
