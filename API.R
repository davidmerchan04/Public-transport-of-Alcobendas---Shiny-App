rm(list=ls())

install.packages("rlist")

library(httr)
library(rlist)
library(jsonlite)
library(dplyr)

resp=GET("http://datos.gob.es/apidata/catalog/dataset/l01280066-transporte-publico-autobus-historico1.json")

http_type(resp)
http_error(resp)

jsonRespText<-content(resp,as="text") 
jsonRespText

jsonRespParsed<-content(resp,as="parsed") 

url=jsonRespParsed$result$items[[1]]$distribution[[4]]$accessURL

data=read.csv(url)
head(data)
View(data)

jsonRespParsed

names(data)

names(data)[1]="Linea"
names(data)[2]="Año"
names(data)[4]="Número.anual.de.pasajeros"
names(data)[5]="Expediciones.por.dia.laborable"
names(data)[6]="Viajeros.por.dia"
names(data)[7]="Viajeros.por.expedicion"
names(data)[8]="Kilometros.anuales.realizados" 

data$Tipo.de.transporte=gsub("Ãº","u",data$Tipo.de.transporte)



