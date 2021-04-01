## Explorar e analisar dados

library(dplyr)
if(!require(rstalix))install.packages("rstatix")
library(rstatix)

#buscar onde o arquivo está e abrir o arquivo
setwd("~/Linguagem R")

covid_tratado<-read.csv2("covid_sp_tratado.csv",sep = ",")
glimpse(covid_tratado)

#alterando variaveis
#data
covid_tratado$data<-as.Date(covid_tratado$data,format("%y-%m%d"))

covid_tratado$idoso...<-as.numeric(covid_tratado$idoso...)

#renomeando coluna idoso
covid_tratado<-rename(covid_tratado,porcentagem_idoso=idoso...)

#APLICANDO FILTROS
#filtro por linha(cidades)

#Campinas
covid_campinas<-covid_tratado%>%filter(municipio=="Campinas")

covid_campinas["desidade_demografica"]<-covid_campinas$pop/covid_campinas$area

covid_campinas["area"]<-covid_campinas$area/100

covid_campinas["dens_demografica"]<-covid_campinas$pop/covid_campinas$area

#guarulhos
covid_guarulhos<-covid_tratado%>%filter(municipio=="Guarulhos")
covid_guarulhos["area"]<-covid_guarulhos$area/100

covid_guarulhos["dens_demografica"]<-covid_guarulhos$pop/covid_guarulhos$area

