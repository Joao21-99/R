#Ficha de trabalho 1
#1
nome<-c("Maria","Miguel","Ana","Pedro")
idade<-c(20,19,19,21)
pontuação<-c(7,9,9.2,8.3)

df<-data.frame(nome,idade,pontuação) #construção do data frame
df
rownames(df)<-c("Estudante 1", "Estudante 2", "Estudante 3","Estudante 4") #Dá os nomes às linhas
df
#2
tempo<-c(60,45,62,58) #construção da nova variaviel e dos respectivos dados.
df1<-data.frame(nome,idade,pontuação,tempo)
rownames(df1)<-c("Estudante 1", "Estudante 2", "Estudante 3","Estudante 4")
df1


#3
names(df1) #Extrai o nome das variaveis

#4
df1[,4] #Informação do estudante 4

#5
Feminino<-c("sim","Não","Sim","Não") #informação do sexo feminino
fem<-data.frame(nome,idade,pontuação,tempo,Feminino)
fem

#6
df1_novo<-df1 #Igualar os dataframes. O dataframe df1-novo irá ficar com as informações do df1.
nome<-c("Maria","Miguel","Ana","Pedro","João", "Rita","Sara")
idade<-c(20,19,19,21,19,22,18)
pontuação<-c(7,9,9.2,8.3,7.2,9.4,8.6)   #adicionar as informações
tempo<-c(60,45,62,58,7.2,9.4,8.6)
df1_novo<-data.frame(nome,idade,tempo,pontuação)
df1_novo
rownames(df1_novo)<-c("Estudante 1", "Estudante 2", "Estudante 3","Estudante 4","estudantes 5","estudamtes 6","estudantes 7")
df1_novo

#7
df1_novo[6,4]<-9.8 #escolher o elemento e alterar
df1_novo

#8
classif<-data.frame(nome,pontuação)
classif

#9
sort(classif[,1]) #????



