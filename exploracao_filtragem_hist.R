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


#guarulhos
covid_guarulhos<-covid_tratado%>%filter(municipio=="Guarulhos")
covid_guarulhos["area"]<-covid_guarulhos$area/100

covid_guarulhos["dens_demografica"]<-covid_guarulhos$pop/covid_guarulhos$area

"Analise estatistica"
#media
mean(covid_campinas$obitos_novos)
mean(covid_campinas$casos_novos)

summarise_at(covid_campinas,vars(obitos_novos,casos_novos),mean)

mean(covid_guarulhos$obitos_novos)
mean(covid_guarulhos$casos_novos)

summarise_at(covid_guarulhos,vars(obitos_novos,casos_novos),mean)

#média móvel
plot(covid_campinas$data,covid_campinas$casos_mm7d, title("MÉDIA MÓVEL"), col = "red")
plot(covid_guarulhos$data,covid_guarulhos$casos_mm7d, title("MÉDIA MÓVEL"), col = "purple")

#mediana
median(covid_campinas$obitos_novos)
median(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos,casos_novos),median)

summarise_at(covid_guarulhos,vars(obitos_novos,casos_novos),median)

#MODA
"o R não tem uma função moda por isso temos que criar"

moda<-function(m){
  valor_unico<- unique(m)#busca o único valor para a coluna valor
  valor_unico[which.max(tabulate(match(m,valor_unico)))]
}

moda(covid_campinas$obitos_novos)
moda(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos,casos_novos),moda)


covid_julho_campinas <- covid_campinas %>% filter(mes==7)
moda(covid_campinas_julho$obitos_novos)
moda(covid_campinas_julho$casos_novos)
summarise_at(covid_campinas_julho,vars(obitos_novos,casos_novos),moda)
mean(covid_campinas_julho$obitos_novos)
mean(covid_campinas_julho$casos_novos)

#histograma
hist(covid_campinas_julho$obitos_novos,col="blue")
hist(covid_campinas_julho$casos_novos,col="green")

hist(covid_campinas$obitos_novos,col="grey")
hist(covid_campinas$casos_novos,col="Red")

hist(covid_guarulhos$obitos_novos,col="blue")
hist(covid_guarulhos$casos_novos,col="yellow")
