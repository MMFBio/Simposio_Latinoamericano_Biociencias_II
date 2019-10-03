
library(ggplot2)

## Trabajo accidentes Lonomia en Misiones

class_final<-c("Descartado"= 8, "Suspeito"= 16, "Confirmado"= 15)
round(((class_final/(sum(class_final)))*100),2)
chisq.test(class_final, simulate.p.value = T)
a<-barplot(height=class_final, names=c("Descartado", "Suspeito", "Confirmado"), 
        col=rgb(0.9,0.5,0.1,0.6),
        ylim=c(0,20), 
        cex.names=1.3,
        cex.axis = 1.3,
        font = 15,
        axis.lty = 10)
text(a, class_final+1.6,paste(round((class_final/sum(class_final))*100, 2),"%", sep=""),cex=1.5) 



sexo=c("F"= 8, "M"=23)
chisq.test(sexo, simulate.p.value = T)
(sexo/sum(sexo))*100
c<-barplot(height=sexo, names=c("Feminino", "Masculino"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,25), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(c, sexo+1.6,paste(round((sexo/sum(sexo))*100, 2),"%", sep=""),cex=1.5) 



lugar=c("selva"=3 , "campo"=13 , "domicilio"=15 )
round(((lugar/sum(lugar))*100),2)
sum(lugar)
chisq.test(lugar, simulate.p.value = T)
chisq.test(lugar, simulate.p.value = T)$residuals
d<-barplot(height=lugar, names=c("Vegetação nativa","Campo", "Domicilio"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,20), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(d, lugar+1.6,paste(round((lugar/sum(lugar))*100, 2),"%", sep=""),cex=1.5) 

umbicación_picada=c("SD"=2, "multiples"= 1,  "MMII"= 8, "MMSS"= 20)
round((umbicación_picada/sum(umbicación_picada))*100,2)
chisq.test(umbicación_picada, simulate.p.value = T)
chisq.test(umbicación_picada, simulate.p.value = T)$residuals
e<-barplot(height=umbicación_picada, names=c("SD", "Multiplos",  "M. inferiores", "M. superiores"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,25), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(e, umbicación_picada+1.6,paste(round((umbicación_picada/sum(umbicación_picada))*100, 2),"%", sep=""),cex=1.5) 


tempo_atendimento=c("<1h"=3, "1-3h"= 1, "3-6h"= 1, "6-12h"=1, "12-24h"=3, ">24h"=22)
chisq.test(tempo_atendimento, simulate.p.value = T)
chisq.test(tempo_atendimento, simulate.p.value = T)$residuals
round((tempo_atendimento/(sum(tempo_atendimento))*100),2)
f<-barplot(height=tempo_atendimento, names=c("<1h", "1-3h", "3-6h", "6-12h", "12-24h", ">24"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,25), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(f, tempo_atendimento+1.6,paste(round((tempo_atendimento/sum(tempo_atendimento))*100, 2),"%", sep=""),cex=1.5)

classif<-c("leve"= 1, "moderado"= 28, "grave"= 2)
classif/sum(classif)*100
chisq.test(classif, simulate.p.value = T)
chisq.test(classif, simulate.p.value = T)$residuals
g<-barplot(height=classif, names=c("Leve", "Moderado", "Grave"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,31), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(g, classif+1.6,paste(round((classif/sum(classif))*100, 2),"%", sep=""),cex=1.5)

trat<-c("no"= 1, "5 ampollas"= 27, "8 ampollas"= 3)
round((trat/sum(trat))*100,2)
chisq.test(trat, simulate.p.value = T)
chisq.test(trat, simulate.p.value = T)$residuals
h<-barplot(height=trat, names=c("Não", "5 ampolas", "8 ampolas"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,31), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(h, trat+1.6,paste(round((trat/sum(trat))*100, 2),"%", sep=""),cex=1.5)


sintomas<-c(17, 6,	4,	2,	1,	1)
round((sintomas/sum(sintomas))*100,2)
chisq.test(sintomas, simulate.p.value = T)
chisq.test(sintomas, simulate.p.value = T)$residuals
i<-barplot(height=sintomas, names=c("G+HS","L+G+HS", "L", "G", "G+HS+IRA", "L+G"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,20), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(i, sintomas+1.6,paste(round((sintomas/sum(sintomas))*100, 2),"%", sep=""),cex=1.5)


ejemplar_id<-c("no"= 16, "traido"= 11, "foto"= 2,  "capturado"= 2)
round((ejemplar_id/sum(ejemplar_id))*100,2)
chisq.test(ejemplar_id, simulate.p.value = T)
chisq.test(ejemplar_id, simulate.p.value = T)$residuals
b<-barplot(height=ejemplar_id, names=c("Não", "Indivíduo", "Foto", "Capturado"), 
           col=rgb(0.9,0.5,0.1,0.6),
           ylim=c(0,20), 
           cex.names=1.3,
           cex.axis = 1.3,
           font = 15,
           axis.lty = 10)
text(b, ejemplar_id+1.6,paste(round((ejemplar_id/sum(ejemplar_id))*100, 2),"%", sep=""),cex=1.5) 


edad<-c(46, 22, 37, 3, 56, 14, 13, 34, 40, 5, 30, 12, 15, 11, 37, 15, 17, 9, 8, 9, 9, 6, 7, 17, 59, 2, 18, 59, 52, 12, 64)
boxplot(edad, col=rgb(0.9,0.5,0.1,0.6), boxwex = 0.5)
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






