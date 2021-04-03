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

#medidas de posição
#minimo
min(covid_campinas$obitos_novos)
min(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos,casos_novos),min)

#maximo
max(covid_campinas$obitos_novos)
max(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos,casos_novos),max)

#Amplitude
range(covid_campinas$obitos_novos)
range(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos,casos_novos),range)

#quartirs
quantile(covid_guarulhos$obitos_novos)
quantile(covid_guarulhos$casos_novos)
summarise_at(covid_guarulhos,vars(obitos_novos,casos_novos),quantile)

#amplitude interquartis
IQR(covid_campinas$obitos_novos)
IQR(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos,casos_novos),IQR)

#summary
summary(covid_campinas$obitos_novos)
summary(covid_campinas$casos_novos)

summary(covid_guarulhos$obitos_novos)
summary(covid_guarulhos$casos_novos)


#boxplot
summary(covid_campinas$obitos_novos)
boxplot(covid_campinas$obitos_novos)

summary(covid_campinas$casos_novos)
boxplot(covid_campinas$casos_novos)

#tratamento de outliers
#identidficar e excluir todos os outliers
covid_guarulhos%>%identify_outliers(casos_novos)
outliers<- c(boxplot.stats(covid_guarulhos$casos_novos)$out)
covid_guarulhos_semout<-covid_guarulhos[-c(which(covid_guarulhos$casos_novos
                                                 %in% outliers)),]
boxplot(covid_guarulhos_semout$casos_novos)

#excluir alguns outliers
covid_campinas%>%identify_outliers(casos_novos)
covid_campinas_semout<-covid_campinas%>%filter(data!="2020-06-19")
boxplot(covid_campinas_semout$casos_novos)

#medidas de dispersão

#variancia
var(covid_campinas$obitos_novos)
var(covid_campinas$casos_novos)

var(covid_guarulhos$obitos_novos)
var(covid_guarulhos$casos_novos)

summarise_at(covid_campinas_julho,vars(obitos_novos,casos_novos),var)

#desvio padrão
sd(covid_campinas$obitos_novos)
sd(covid_campinas$casos_novos)

summarise_at(covid_guarulhos,vars(obitos_novos,casos_novos),sd)

#teste de normalidade
"exixtem 4 testes de normalidade principais (numéricos) e dois testes graficos:
shapiro wilk(limite de 5000 amostras)
Anderson-darling
kolmogrov_Smirnov
Cramer Von mises
Histograma
QQplot"

#Nível de significancia de 0,05 (5%) ou confiança de 0,95(95%) - (MAIS UTILIZADO)
#qndo o paramento p>0,05(distribuição normal)

if(!require(nortest))install.packages("nortest")
library(nortest)

#histograma
hist(covid_campinas$casos_novos,probability = T,col = "blue")
lines(density(covid_campinas$casos_novos),col = "Red")

#QQplot
qqnorm(covid_campinas$casos_novos,col="purple")
qqline(covid_campinas$casos_novos)

#shapiro wilk
shapiro.test(covid_campinas$casos_novos)

#Anderson Darling
ad.test(covid_campinas$casos_novos)

#Kolmogrov
ks.test(covid_campinas$casos_novos,pnorm)
lillie.test(covid_campinas$casos_novos)

#Cramer_von mises
cvm.test(covid_campinas$casos_novos)

#CORRELAÇÃO LINEAR
#metodo 
"person - para dados paraméticos
sperman - volume de dados não paramétricos
Kendall - volume pequeno de dados não parametricos"

plot(covid_campinas$obitos)
cor(covid_campinas$casos,covid_campinas$obitos,method = "spearman")

regressao<-lm(formula = obitos ~ casos,data = covid_campinas)
regressao$coefficients
summary(regressao)

### Equação: obitos=51.67+0,0337*casos

### Coeficiente de determinação (ajustado): 0,9832

#grafico de linha com ajuste de reta usando o GGPlot2

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(ggpubr))install.packages("ggpubr")
library(ggpubr)

ggplot(data = covid_campinas, mapping = aes(x = casos, y = obitos)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\" ,   \")~~")), label.x = 15000,
                        label.y = 1800) +
  theme_gray()

library(corrplot)
matrix_corr<- cor(covid_campinas[5:13],method = "spearman")

corrplot(matrix_corr,method = "color")
corrplot(matrix_corr,method = "color",
         type = "full",order = "original", addCoef.col = "black",#adiciona o coeficiente a matriz
         tl.col = "black",tl.srt = 45,# cor e rotação do nome das variaveis
)

#grafico linear por cidades
covid_cidades<-covid_sp_tratado %>% filter (municipio  %in% c("Campinas", "Guarulhos", "Sorocaba"))

ggplot(covid_cidades,aes(x=casos, y=obitos,color=municipio))+
  geom_line()+labs(title = "Evolução dos obitos em função dos casos de covid",
                   x="Casos",
                   y="Obitos",
                   color="Meses")+
  theme_classic()
