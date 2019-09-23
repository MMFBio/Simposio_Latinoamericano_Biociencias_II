

## Trabajo accidentes Lonomia en Misiones

class_final<-c("descartado"= 8, "sospechoso"= 16, "confirmado"= 15)
round(((class_final/(sum(class_final)))*100),2)
chisq.test(class_final, simulate.p.value = T)

sexo=c("F"= 8, "M"=23)
chisq.test(sexo, simulate.p.value = T)
(sexo/sum(sexo))*100

lugar=c("campo"=13 , "selva"=3 , "domicilio"=15 )
round(((lugar/sum(lugar))*100),2)
chisq.test(lugar, simulate.p.value = T)
chisq.test(lugar, simulate.p.value = T)$residuals

umbicación_picada=c("SD"=2, "multiples"= 1, "MMSS"= 20, "MMII"= 8)
round((umbicación_picada/sum(umbicación_picada))*100,2)
chisq.test(umbicación_picada, simulate.p.value = T)
chisq.test(umbicación_picada, simulate.p.value = T)$residuals

tempo_atendimento=c("<1h"=3, "1-3h"= 1, "3-6h"= 1, "6-12h"=1, "12-24h"=3, ">24h"=22)
chisq.test(tempo_atendimento, simulate.p.value = T)
chisq.test(tempo_atendimento, simulate.p.value = T)$residuals
round((tempo_atendimento/(sum(tempo_atendimento))*100),2)

classif<-c("leve"= 1, "moderado"= 28, "grave"= 2)
classif/sum(classif)*100
chisq.test(classif, simulate.p.value = T)
chisq.test(classif, simulate.p.value = T)$residuals

trat<-c("no"= 1, "5 ampollas"= 27, "8 ampollas"= 3)
round((trat/sum(trat))*100,2)
chisq.test(trat, simulate.p.value = T)
chisq.test(trat, simulate.p.value = T)$residuals

sintomas<-c(17, 6,	4,	2,	1,	1)
round((sintomas/sum(sintomas))*100,2)
chisq.test(sintomas, simulate.p.value = T)
chisq.test(sintomas, simulate.p.value = T)$residuals

ejemplar_id<-c("no"= 16, "foto"= 2, "traido"= 11, "capturado"= 2)
round((ejemplar_id/sum(ejemplar_id))*100,2)
chisq.test(ejemplar_id, simulate.p.value = T)
chisq.test(ejemplar_id, simulate.p.value = T)$residuals

edad<-c(46, 22, 37, 3, 56, 14, 13, 34, 40, 5, 30, 12, 15, 11, 37, 15, 17, 9, 8, 9, 9, 6, 7, 17, 59, 2, 18, 59, 52, 12, 64)
boxplot(edad, ylab= "Edad")
summary(edad)
shapiro.test(edad)
sd(edad)
mean(edad)

departamento=c("Gral Belgrano"=8, "Eldorado"= 2, "San Pedro"= 7, "Cainguas"= 2, "Guarani"= 9, "25 de Mayo"= 2, "Obera"=1)
chisq.test(departamento, simulate.p.value = T)
chisq.test(departamento, simulate.p.value = T)$residuals

ano=c("2014"= 7, "2015"= 12, "2016"= 6, "2017"=5, "2018"=1)
chisq.test(ano, simulate.p.value = T)
chisq.test(ano, simulate.p.value = T)$residuals
sum(ano)

mes=c("ene"= 6, "feb"=0, "mar"=0, "abr"=5, "may"=5, "jun"=0,
      "jul"=0, "ago"=0, "sep"=0, "oct"=0, "nov"=4, "dic"= 11)
chisq.test(mes, simulate.p.value = T)
chisq.test(mes, simulate.p.value = T)$residuals






