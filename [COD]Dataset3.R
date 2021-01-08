### Load librarys and file ###
library (tidyverse)
library (lubridate)
library(readxl)
library(readr)
library(tm)
library(stringi)
library(plyr)

### READ DB - Despesas jan ~ dez 2019

names_read<- c("JAN","FEV","MAR","ABR","MAIO","JUN","JUL","AGO","SET","OUT","NOV","DEZ")

for (k in 1:length(names_read)) {
  
db_transitoria <- read_excel(paste0('G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/',
                                    'Lobo Brasil/[DADOS]Lobo Brasil Olavo/Despesas excel/',
                                    names_read[k],".xlsx"),
                             col_types = c("skip", "skip", "text", "text", "text", "skip", "skip", "numeric", "date", "text", "skip"),
                             skip = 1)

assign(paste0("df_",names_read[k]),db_transitoria)

}

# Aggregate DF

db_teste <- rbind(df_JAN,df_FEV)
db_teste <- rbind(db_teste, df_MAR)
db_teste <- rbind(db_teste, df_ABR)
db_teste <- rbind(db_teste, df_MAIO)
db_teste <- rbind(db_teste, df_JUN)
db_teste <- rbind(db_teste, df_JUL)
db_teste <- rbind(db_teste, df_AGO)
db_teste <- rbind(db_teste, df_SET)
db_teste <- rbind(db_teste, df_OUT)
db_teste <- rbind(db_teste, df_NOV)
db_teste <- rbind(db_teste, df_DEZ)

db_t1 <- db_teste
db_t1$`6VALOR PAGO`[is.na(db_t1$`6VALOR PAGO`)] <- 0 
db_agregada <- subset(db_t1, `6VALOR PAGO` != 0)
db_agregada <- db_agregada[-808,]

# Creating DB by forma de pagamento

db_by_forma <- db_agregada %>% group_by(month=floor_date(`7DATA PGTO`,"month"), `8FORMA PGTO`) %>%
  summarize(`6VALOR PAGO`=sum(`6VALOR PAGO`))
db_by_forma <- db_by_forma %>% drop_na

db_by_forma$month <- month(ymd(db_by_forma$month), label = TRUE, abbr = FALSE)

db_by_forma <- db_by_forma %>%
  rename(
    Mes = 1,
    Forma_Pagamento = 2,
    Valor_Pago = 3)

write.csv(db_by_forma,paste0('G:/Meu Drive/Tactin/',
                                 'Trabalhos - Análise de Dados/Lobo Brasil/',
                                 '[TABELAS]Prontas R/despesas_por_forma_pgto.csv'), row.names = FALSE)

# Creating DB by classificação geral

db_by_class <- db_agregada %>% group_by(month=floor_date(`7DATA PGTO`,"month"), `3CLASSIFICAÇÃO GERAL`) %>%
  summarize(`6VALOR PAGO`=sum(`6VALOR PAGO`))
db_by_class <- db_by_class %>% drop_na

db_by_class$month <- month(ymd(db_by_class$month), label = TRUE, abbr = FALSE)

db_by_class <- db_by_class %>%
  rename(
    Mes = 1,
    CLassificacao_Geral = 2,
    Valor_Pago = 3)

write.csv(db_by_class,paste0('G:/Meu Drive/Tactin/',
                             'Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[TABELAS]Prontas R/despesas_por_classe_geral.csv'), row.names = FALSE)

# Creating DB by categoria

db_by_categoria <- db_agregada %>% group_by(month=floor_date(`7DATA PGTO`,"month"), `2CATEGORIA`) %>%
  summarize(`6VALOR PAGO`=sum(`6VALOR PAGO`))
db_by_categoria <- db_by_categoria %>% drop_na

db_by_categoria$month <- month(ymd(db_by_categoria$month), label = TRUE, abbr = FALSE)

db_by_categoria <- db_by_categoria %>%
  rename(
    Mes = 1,
    Categoria = 2,
    Valor_Pago = 3)

write.csv(db_by_categoria,paste0('G:/Meu Drive/Tactin/',
                             'Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[TABELAS]Prontas R/despesas_por_categoria.csv'), row.names = FALSE)

# Creating DB by fornecedor

db_by_fornecedor <- db_agregada %>% group_by(month=floor_date(`7DATA PGTO`,"month"), `1FORNECEDOR`) %>%
  summarize(`6VALOR PAGO`=sum(`6VALOR PAGO`))
db_by_fornecedor <- db_by_fornecedor %>% drop_na

db_by_fornecedor$month <- month(ymd(db_by_fornecedor$month), label = TRUE, abbr = FALSE)

db_by_fornecedor <- db_by_fornecedor %>%
  rename(
    Mes = 1,
    Fornecedor = 2,
    Valor_Pago = 3)

write.csv(db_by_fornecedor,paste0('G:/Meu Drive/Tactin/',
                             'Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[TABELAS]Prontas R/despesas_por_fornecedor.csv'), row.names = FALSE)


## CHANGES 27/01 #

# Removing punctuation and numbers

despesas_por_forma_pgto$Forma_Pagamento <- removePunctuation(despesas_por_forma_pgto$Forma_Pagamento)
despesas_por_classe_geral$CLassificacao_Geral <- removePunctuation(despesas_por_classe_geral$CLassificacao_Geral)
despesas_por_classe_geral$CLassificacao_Geral <- removeNumbers(despesas_por_classe_geral$CLassificacao_Geral)

write.csv(despesas_por_classe_geral,paste0('G:/Meu Drive/Tactin/',
                             'Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[TABELAS]Prontas R/despesas_por_classe_geral.csv'), row.names = FALSE)


write.csv(despesas_por_forma_pgto,paste0('G:/Meu Drive/Tactin/',
                             'Trabalhos - Análise de Dados/Lobo Brasil/',
                             '[TABELAS]Prontas R/despesas_por_forma_pgto.csv'), row.names = FALSE)

#### UPDATE - CREATING NEW TREE DB BY FUNCIONÁRIOS

funcionarios <- subset(db_agregada, db_agregada$`3CLASSIFICAÇÃO GERAL` == "01 - Funcionários")

# removing annoying special characters and renaming


funcionarios$`3CLASSIFICAÇÃO GERAL` <- removePunctuation(funcionarios$`3CLASSIFICAÇÃO GERAL`)
funcionarios$`3CLASSIFICAÇÃO GERAL` <- removeNumbers(funcionarios$`3CLASSIFICAÇÃO GERAL`)
funcionarios$`3CLASSIFICAÇÃO GERAL` <- iconv(funcionarios$`3CLASSIFICAÇÃO GERAL`, from = "UTF-8", to = "ASCII//TRANSLIT")                  
funcionarios$`3CLASSIFICAÇÃO GERAL` <- stri_replace_all_fixed(funcionarios$`3CLASSIFICAÇÃO GERAL`," ", "")      


stri_enc_isutf8(funcionarios$`2CATEGORIA`)

funcionarios$`2CATEGORIA` <- removePunctuation(funcionarios$`2CATEGORIA`)
funcionarios$`2CATEGORIA` <- removeNumbers(funcionarios$`2CATEGORIA`)
funcionarios$`2CATEGORIA` <- iconv(funcionarios$`2CATEGORIA`, from = "UTF-8", to = "ASCII//TRANSLIT")   

unique(funcionarios$`2CATEGORIA`)

funcionarios$`2CATEGORIA` <- revalue(funcionarios$`2CATEGORIA`, c("F  Adiantamento  Vale" = "adiantamento_vale",
                                                                  "F  Comissao" = "comissao",
                                                                  "F  FGTS" = "fgts",
                                                                  "F  Funcionario Extra" = "funcionario_extra",
                                                                  "F  Funcionario Limpeza" = "funcionario_limpeza",
                                                                  "F  GPS" = "gps",
                                                                  "F  Salario" = "salario",
                                                                  "F  Sindicato" = "sindicato",
                                                                  "F  Transporte" = "transporte",
                                                                  "F  Vale Alimentacao  Refeicao" = "vale_alimentacao_refeicao",
                                                                  "F   Extra Bar" = "extra_bar",
                                                                  "F   Extra Cozinha" = "extra_cozinha",
                                                                  "F   Extra Louca" = "extra_louca",
                                                                  "F   Extra Salao" = "extra_salao",
                                                                  "F   Extra Limpeza" = "extra_limpeza",
                                                                  "F   Exames" = "exames",
                                                                  "F  Complemento Salario" = "complemento_salario",
                                                                  "F  Ferias" = "ferias",
                                                                  "F  Vale Transporte" = "vale_transporte",
                                                                  "F  Coordenacao de Evento" = "coordenacao_de_evento",
                                                                  "F  Uniforme" = "uniforme",
                                                                  "F  Rescisao" = "rescisao",
                                                                  "F  o a parcela" = "decimo_terceiro_parcelas"))

unique(funcionarios$`2CATEGORIA`)

# testing db

funcionarios_teste <- funcionarios

# adding classes

funcionarios_teste$grande_classe <- 0
funcionarios_teste$micro_classe <- 0

# columns for grande classe classification
funcionarios_teste$outros <- 0
funcionarios_teste$funcionario_fixo <- 0
funcionarios_teste$funcionario_extra <- 0

# columns for mixed funcionarios in same category
funcionarios_teste$ext_coz <- 0
funcionarios_teste$ext_sal <- 0
funcionarios_teste$func_exr <- 0

# grande classe: outros

funcionarios_teste$outros <- ifelse(funcionarios_teste$`2CATEGORIA` == "uniforme" | funcionarios_teste$`2CATEGORIA` == "transporte" | funcionarios_teste$`2CATEGORIA` == "gps" | funcionarios_teste$`2CATEGORIA` == "coordenacao_de_evento",
                                          1, "")

# grande classe: funcionario_fixo

funcionarios_teste$funcionario_fixo <- ifelse(funcionarios_teste$`2CATEGORIA` == "salario" | funcionarios_teste$`2CATEGORIA` == "comissao" | funcionarios_teste$`2CATEGORIA` == "adiantamento_vale" | funcionarios_teste$`2CATEGORIA` == "funcionario_limpeza" | funcionarios_teste$`2CATEGORIA` == "extra_limpeza" | funcionarios_teste$`2CATEGORIA` == "complemento_salario" | funcionarios_teste$`2CATEGORIA` == "vale_alimentacao_refeicao" | funcionarios_teste$`2CATEGORIA` == "exames" | funcionarios_teste$`2CATEGORIA` == "decimo_terceiro_parcelas" | funcionarios_teste$`2CATEGORIA` == "vale_transporte" | funcionarios_teste$`2CATEGORIA` == "ferias" | funcionarios_teste$`2CATEGORIA` == "fgts" | funcionarios_teste$`2CATEGORIA` == "sindicato" | funcionarios_teste$`2CATEGORIA` == "rescisao",
                                              "2","")


# grande classe: funcionario_extra

funcionarios_teste$funcionario_extra <- ifelse(funcionarios_teste$`2CATEGORIA` == "extra_bar" | funcionarios_teste$`2CATEGORIA` == "extra_louca",
                                               "3","")

# grande classe: funcionario_fixo with categoria having mixed funcionario_fixo with funcionario_extra
# extra cozinha

extra_cozinha <- subset(funcionarios_teste, funcionarios_teste$`2CATEGORIA` == "extra_cozinha")
unique(extra_cozinha$`1FORNECEDOR`)

names_extra_cozinha_funcs_extras <- c("Caio","Giovana","Igor","Sarah","Extras cozinha fria","Igor Ferreira",
                                      "Gabriela Ofinato","Gabriela Ginatto","Guilherme Vontobel","Gabi cozinheira",
                                      "Cida","Gabi","Ivam","Maura","Catarina","Lauriberto Cipriano")

for (i in 1:length(names_extra_cozinha_funcs_extras)) {
  
funcionarios_teste$ext_coz <- ifelse(funcionarios_teste$`2CATEGORIA` == "extra_cozinha" & funcionarios_teste$`1FORNECEDOR` == names_extra_cozinha_funcs_extras[i],
                                     "4",funcionarios_teste$ext_coz)

}

# extra salão

extra_salao <- subset(funcionarios_teste, funcionarios_teste$`2CATEGORIA` == "extra_salao")
unique(extra_salao$`1FORNECEDOR`)

names_extra_salao_funcs_extras <- c("Kamil","Adir","Victor","Diego", "Vitor","Cleiton",
                                    "Diego Salão","Patrick","Gabriel garçon","Ivan Santos",
                                    "Lucas cruz","Ivan","Jhonatan","Lucas","Diego Bar",
                                    "Lucas salão")

for (i in 1:length(names_extra_salao_funcs_extras)) {
  
  funcionarios_teste$ext_sal <- ifelse(funcionarios_teste$`2CATEGORIA` == "extra_salao" & funcionarios_teste$`1FORNECEDOR` == names_extra_salao_funcs_extras[i],
                                       "5",funcionarios_teste$ext_sal)
  
}

# funcionarios_extra

extra_funcionario <- subset(funcionarios_teste, funcionarios_teste$`2CATEGORIA` == "funcionario_extra")
unique(extra_funcionario$`1FORNECEDOR`)

names_extra_funcionario_extras <- c("Adir","Adir garçom","Andreia Louça","Biel bar",
                                    "Diego Bar","Giovana cozinha","Igor cozinha",
                                    "Igor Cozinha","Isabela Ruggieiro","Juliana louça",
                                    "Kamil garçom","Rafa Cozinha","Rafael louça","Terezinha Louça",
                                    "Terezinha louças")

for (i in 1:length(names_extra_funcionario_extras)) {
  
  funcionarios_teste$func_exr <- ifelse(funcionarios_teste$`2CATEGORIA` == "funcionario_extra" & funcionarios_teste$`1FORNECEDOR` == names_extra_funcionario_extras[i],
                                       "6",funcionarios_teste$func_exr)
  
}
funcionarios_teste <- backup_1_funcs
# filling grande classe

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$outros == 1,"Outros",funcionarios_teste$grande_classe)

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$funcionario_fixo == 2,"Funcionarios_fixos",funcionarios_teste$grande_classe)

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$funcionario_extra == 3,"Funcionarios_extras",funcionarios_teste$grande_classe)

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$ext_coz == 4,"Funcionarios_extras",funcionarios_teste$grande_classe)

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$ext_sal == 5,"Funcionarios_extras",funcionarios_teste$grande_classe)

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$func_exr == 6,"Funcionarios_extras",funcionarios_teste$grande_classe)

funcionarios_teste$grande_classe <- ifelse(funcionarios_teste$grande_classe == "","Funcionarios_fixos", funcionarios_teste$grande_classe)

unique(funcionarios_teste$grande_classe)

# Creating three separate tables for Richard

tbl_outros <- subset(funcionarios_teste, funcionarios_teste$grande_classe == "Outros")
tbl_fixos <- subset(funcionarios_teste, funcionarios_teste$grande_classe == "Funcionarios_fixos")
tbl_extras <- subset(funcionarios_teste, funcionarios_teste$grande_classe == "Funcionarios_extras")


##### UNTIL THIS POINT THERE IS INFO ABOUT INDIVIDUALS ########


# tbl outros, overall structure

final_outros <- tbl_outros

final_outros <- final_outros[c(8,2,4:5)]

final_outros$micro_classe <- final_outros$grande_classe

final_outros <- final_outros[c(4,1,5,2:3)]                             

final_outros <- final_outros %>%
  rename(
    Data = 1,
    Grande_classe = 2,
    Micro_classe = 3,
    Tipo_despesa = 4,
    Valor = 5)

# re order by date

final_outros <- final_outros[(order(as.Date(final_outros$Data))),]

# tbl extras overall structure

final_extras <- tbl_extras

final_extras <- final_extras[c(2,4:5,7:8)]

final_extras <- final_extras[c(3,5,4,1,2)]

final_extras$micro_classe <- final_extras$grande_classe

final_extras <- final_extras %>%
  rename(
    Data = 1,
    Grande_classe = 2,
    Micro_classe = 3,
    Tipo_despesa = 4,
    Valor = 5)
   
#re ordering

final_extras <- final_extras[(order(as.Date(final_extras$Data))),]

# tbl fixos overall structure

final_fixos <- tbl_fixos

final_fixos <- final_fixos[c(2,4:5,7:8)]

final_fixos <- final_fixos[c(3,5,4,1,2)]

final_fixos <- final_fixos %>%
  rename(
    Data = 1,
    Grande_classe = 2,
    Micro_classe = 3,
    Tipo_despesa = 4,
    Valor = 5)

# filling micro_classes

unique(final_fixos$Tipo_despesa)

final_fixos$Micro_classe <- ifelse (final_fixos$Tipo_despesa == "salario" | final_fixos$Tipo_despesa == "comissao" | final_fixos$Tipo_despesa == "adiantamento_vale",
                                    "Salario",final_fixos$Micro_classe)

final_fixos$Micro_classe <- ifelse (final_fixos$Tipo_despesa == "exames" | final_fixos$Tipo_despesa == "decimo_terceiro_parcelas" | final_fixos$Tipo_despesa == "vale_transporte" | final_fixos$Tipo_despesa == "ferias" | final_fixos$Tipo_despesa == "fgts" | final_fixos$Tipo_despesa == "sindicato" | final_fixos$Tipo_despesa == "rescisao",
                                    "Encargos", final_fixos$Micro_classe)
                                    
final_fixos$Micro_classe <- ifelse (final_fixos$Micro_classe == "0",
                                    "Extra",final_fixos$Micro_classe)
    
# ordering by day

final_fixos <- final_fixos[(order(as.Date(final_fixos$Data))),]

## Saving

write.csv(final_outros,paste0('G:/Meu Drive/Tactin/',
                                  'Trabalhos - Análise de Dados/Lobo Brasil/',
                                  '[TABELAS]Prontas R/[Funcionarios]Despesas_outros.csv'), row.names = FALSE)

write.csv(final_fixos,paste0('G:/Meu Drive/Tactin/',
                              'Trabalhos - Análise de Dados/Lobo Brasil/',
                              '[TABELAS]Prontas R/[Funcionarios]Despesas_fixos.csv'), row.names = FALSE)

write.csv(final_extras,paste0('G:/Meu Drive/Tactin/',
                              'Trabalhos - Análise de Dados/Lobo Brasil/',
                              '[TABELAS]Prontas R/[Funcionarios]Despesas_extras.csv'), row.names = FALSE)


### t

teste_fin <- db_agregada

db_by_forma <- db_agregada %>% group_by(month=floor_date(`7DATA PGTO`,"month"), `8FORMA PGTO`) %>%
  summarize(`6VALOR PAGO`=sum(`6VALOR PAGO`))

teste_fin_group <- teste_fin %>%
  group_by(`1FORNECEDOR`)

