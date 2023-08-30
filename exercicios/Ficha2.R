#Ficha 2 Script

#1
Ficha2$Género<-factor(Ficha2$Género, label =c("Feminino","Masculino"), levels = c(1,2))


#2
#a) Género
Fabs<-table(Ficha2$Género)  #frequencia absoluta
Fabs
Frel<-round((table(Ficha2$Género)/length(Ficha2$Género)*100),0)
Frel #Frequência relativa em %
tabelafreq<- cbind(Fabs,Frel)
tabelafreq
#b) Tempo
Ficha2$Tempo_desl<-factor(Ficha2$Tempo_desl,label=c("Menos de 30 minutos","30 a 60 minutos","Entre 1 a 2 horas","mais de 2 horas"),levels=c(1,2,3,4))

