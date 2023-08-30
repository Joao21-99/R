a<-10
25->c
#O lado da seta n�o interessa e sim de que lado est� a variavel

a<-"João"
b<-"Leandro"
c=c(a,b)
c
summary(c) #resumo estatistico
d<-c(4,7,8,12,3,45)
summary(d)
str(c)
install.packages("stringr") #pacote que combina multiplas strings
getwd()  
setwd("C:/Users/joaoy")
library(stringr)

Nome<-"João "
Apelido<-"Yanga"
NomeCompleto<-str_c(Nome,Apelido) #str � o comando especifico para combinar juntar as strings
#4^2=4**2=16 (pot�ncia)


#-----------------Lógica----------------------
#O "E" e o "Ou"
7==7
4==2+2
5==4.9
7==14/2
7==14/2 & 16==4^2
3==3 & 4==5   #& � o simbolo para o "E" 

3==3 || 4==5 
4==6 || 6==4 # || � o simbolo para o "OU"
