CargaHoraria<-c(100,220,100,150)
summary(CargaHoraria)   #sumario de n�meros ale�rios. O comando #factor
#torna n�meros em fatores/classes.
CargaHoraria <- as.factor(CargaHoraria)
summary(CargaHoraria) #agora mostra quantas vezes aparecem as diferentes cargas hor�rias.

#tipos de variaveis
a="João" #tipo character/string
b=14 #tipo numerico(inteiro)
c=14.9 #tipo numerico
d=c<b #tipo logico
d
V<-c(8,7,6,5,76)
C<-c("João,Ruben,Jorge,César")
is.vector(V) #v�-se � um vetor
mode(V)   #diz o tipo do vetor
is.vector(C)
mode(C)
L1<-list(9,4,7.8)
L1
str(L1)
L2<-list("João","Pedro","Paulo" )
L1[1]
L2[3]
informacao<-list("João","Pedro",c(7,9,0))
informacao[2]
informacao[3]
informacao[[3]][1] #escolher a posi��o dentro de uma posi��o composta.
