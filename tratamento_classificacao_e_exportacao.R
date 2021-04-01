# PROJETO 1

#baixa pacote caso ainda não esteja instalado
if(!require(dplyr))install.packages("dplyr")
#carregar o pacote
library(dplyr)

#buscar o local onde está o arquivo
setwd("~/Linguagem R/dados-covid-sp-master/dados-covid-sp-master/data")

#abrir o arquivo
covid_sp<-read.csv("dados_covid_sp.csv",sep=";",encoding = "UTF-8")
head(covid_sp)

#alterando o nome de coluna
covid_sp_alterado<-rename(covid_sp,municipio = nome_munic)

#alterando mais de uma coluna
covid_sp_alterado<-rename(covid_sp_alterado,data=datahora,rotulo_mapa=map_leg,
                          codigo_mapa=map_leg_s)
#excluir uma coluna por nome
covid_sp_alterado$cod_ra<-NULL

#excluir a coluna pelo número
covid_sp_alterado<-select(covid_sp_alterado,-c(21))

#excluir várias colunas por nome
covid_sp_alterado<-subset(covid_sp_alterado,select = -c(codigo_ibge,cod_drs))

#excluir varias colunas por número
covid_sp_alterado<-select(covid_sp_alterado,-c(14,15))
covid_sp_alterado<-select(covid_sp_alterado,-c(17:19))

#excluir uma linha
#por número
covid_sp_alterado<-slice(covid_sp_alterado,-c(239660))

covid_sp_alterado<-slice(covid_sp_alterado,-c(239661:239666))

#Por nome
covid_sp_alterado<-covid_sp_alterado%>% filter(municipio!="Ignorado")


####VERIFICANDO VALORES MISSING (AUSENTES)

#NA = valores ausentes
#NAN = valores indefinidos

sapply(covid_sp_alterado, function(x)sum(is.na(x)))
sapply(covid_sp_alterado, function(x)sum(is.nan(x)))

if(!require(tidyr))install.packages("tidyr")
library("tidyr")

covid_sp_alterado2<-covid_sp_alterado%>% mutate_all(replace_na,54)

##OU

covid_sp_alterado2<-replace(x=covid_sp_alterado,list = is.na(covid_sp_alterado),
                            values = 54)

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$semana_epidem == 54]<- 2021

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-01' &
                                   covid_sp_alterado2$data <= '2021-01-07'  ] <- 54

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-08' &
                                   covid_sp_alterado2$data <= '2021-01-14'  ] <- 55

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-15' &
                                   covid_sp_alterado2$data <= '2021-01-21'  ] <- 56



#VERIFICANDO A TIPAGEM DOS ATRIBUTOS (Variáveis)
# existem tipos básicos
#character
#integer
#numeric
#logical
#complex
#factor(sequência de valores definidos por níveis)
#date (data)

str(covid_sp_alterado2)

#OU

glimpse(covid_sp_alterado2)

#trasformação da tipagem em atributos
covid_sp_alterado2$semana_epidem<-as.integer(covid_sp_alterado2$semana_epidem)
#ver a mudança
glimpse(covid_sp_alterado2)

#alterando a data
covid_sp_alterado2$data<-as.Date(covid_sp_alterado2$data)
glimpse(covid_sp_alterado2)


#criando colunas
covid_sp_alterado2["idoso(%)"]<-100*covid_sp_alterado2$pop_60/covid_sp_alterado2$pop

#exportação de arquivo
write.table(covid_sp_alterado2,file = "covid_tratado.csv",sep = ",")
