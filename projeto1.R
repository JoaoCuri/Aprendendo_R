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
