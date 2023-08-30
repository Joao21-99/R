#Teste de normalidade
#xb-- média amostral sx-- desvio padrão amostral
mean(Idade48inq$Idades)
media<- mean(Idade48inq$Idades)
media
dp<-sqrt(var(Idade48inq$Idades))
dp
t1 <-ks.test(Idade48inq$Idades, "pnorm", media, dp)
t1
#teste de shapirowilk
x<-c(4,5,10,1,3,4,5,15,16,2,17,0)
summary(x)
shapiro.test(x)

#Para avaliar se a variavel x provém de uma população com distribuição normal
#recorreu-se ao teste de ajustamento de shapirowilk que se apresenta na tabela2
#A anliase desse output perimite concluir que a variavel x não provem de uma popuplação
#com distruibuição normal (W=0.851; p= 0.038)
