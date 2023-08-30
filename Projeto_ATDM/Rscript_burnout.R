#Trabalho de grupo ATDM
#CHECKPOINT 1


Grupo5I<-Grupo5 # Criamos dois dataframes. Um serve como esboço.
Grupo5<-Grupo5I
Grupo5I <- data.frame(Grupo5)
#amostra total: 134

Existp <- 0

for(i in 1 : nrow(Grupo5I)){
  IsNa <- is.na(Grupo5I$PMentoria[i])
  
  if( IsNa == FALSE)
  {Existp <- Existp + 1}
}

Existp#133

#ALTERAÇÃO DOS NOMES DOS VALORES DAS VARIÁVEIS----------------------------------
#Alteração dos valores "1, 2" para "Feminino, Masculino" da variável "Sexo"
Grupo5I$Sexo<-factor(Grupo5$Sexo, label = c("Feminino","Masculino"),
levels = c(1,2))

#Alteração "1, 2" para "Sim, Não" da variável "Escolhi"
Grupo5I$Escolhi<-factor(Grupo5$Escolhi, label = c("Sim", "Não"),
levels = c(1,2))

#Alteração "1, 2" para "Sim, Não" da variável "opcao1"
Grupo5I$opcao1<-factor(Grupo5$opcao1, label =c("Sim","Não"),
levels= c(1,2))

#Alteração "1, 2" para "Sim, Não" da variável "PMentoria"
Grupo5I$PMentoria<-factor(Grupo5$PMentoria, label =c("Sim", "Não"), 
levels = c(1,2))

View(Grupo5I)

str(Grupo5I)
#-------------------------------------------------------------------------------
  

#LIMPEZA DOS DADOS--------------------------------------------------------------
summary(Grupo5I) #resumo estatístico da base de dados

#horas de sono: 33 (errado)
#horas das redes: 90(errado) 
#horas de tv: 45(errado)

#O comando sort ordena os valores das variáveis, dessa forma podemos ver os val-
#ores que não fazem sentido na base de dados.
sort(Grupo5I$IHorasTV)
sort(Grupo5I$HorasSono)
sort(Grupo5I$HorasRedes)

#Esta função limpa os dados da variável "HorasRedes" que são igual ou superior a 
#24
for(i in 1 : nrow(Grupo5I)){
  IsNa <- is.na(Grupo5I$HorasRedes[i])
  
  if(Grupo5I$HorasRedes[i] >= 24 && IsNa == FALSE)
  {Grupo5I$HorasRedes[i] <- NA}
}

sort(Grupo5I$HorasRedes)

#Esta função limpa os dados da variável "HorasTV" que são igual ou superior a 30
for(i in 1 : nrow(Grupo5I)){
  IsNa <- is.na(Grupo5I$HorasTV[i])
  
  if(Grupo5I$HorasTV[i] >= 30 && IsNa == FALSE)
  {Grupo5I$HorasTV[i] <- NA}
}

sort(Grupo5I$HorasTV)

#Esta função limpa os dados da variável "HorasSono" que são igual ou superior a 
#30
for(i in 1 : nrow(Grupo5I)){
  IsNa <- is.na(Grupo5I$HorasSono[i])
  
  if(Grupo5I$HorasSono[i] >= 33 && IsNa == FALSE)
  {Grupo5I$HorasSono[i] <- NA}
}

sort(Grupo5I$HorasSono)
--------------------------------------------------------------------------------

  
#ESTUDO DA VARIÁVEL: IDADE------------------------------------------------------
#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DAS IDADES
boxplot(Grupo5I$Idade,
        main="Diagrama de extremos e quartis para a distribuição das idades",
        cex.main = 1,
        ylab = "Idades",
        xlab = "Todos os alunos", 
        col = c("darkorange")) #com outliers

#Criar dataframe que não irá ter outliers
Idade_SemOutliers <- data.frame(Grupo5I$Idade) 
View(Idade_SemOutliers)
#Função que irá remover os outliers (todos os valores superiores a 25)
for(i in 1 : nrow(Idade_SemOutliers)){
  IsNa <- is.na(Idade_SemOutliers$Grupo5I.Idade[i])
  if(Idade_SemOutliers$Grupo5I.Idade[i] >= 25 && IsNa == FALSE)
  {Idade_SemOutliers$Grupo5I.Idade[i] <- NA}
}

sort(Idade_SemOutliers)

#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DAS IDADES SEM OUTLIERS
boxplot(Idade_SemOutliers$Grupo5I.Idade,
        main="Diagrama de extremos e quartis para a distribuição das idades sem 
        outliers",
        cex.main = 1,
        ylab = "Idades",
        xlab = "Todos os alunos",
        col = c("blue"))

#CÁLCULO DAS MÉDIAS, MODA, MEDIANAS, QUARTIS E DESVIO-PADRÃO
#Para calcular moda é preciso criar uma função

Calcular_Moda <- function(Vetor_Idades){
  Idades_Unicas <- unique(Vetor_Idades)#retira valores duplicados
  Idades_Unicas[which.max(tabulate(match(Vetor_Idades,Idades_Unicas)))] #which.max 
  #vai escolher o número que apareceu mais vezes.Tabulate passa por todos os va-
  #lores que saiem do match. O match retorna um valor dependente das vezes que
  #existe um valor de "IdadesUnicas" em "VetorIdades"
}

#COM OUTLIERS
#IDADE MÉDIA
Media_Idade <- round(mean(Grupo5I$Idade,na.rm = TRUE),2) #na.rm é um comando que remove os mis-
Media_Idade
#sings
#Resultado: 20.55

#MODA DA IDADE
#Criar um vector com as idades
Vetor_Idades <- Grupo5I$Idade[!is.na(Grupo5I$Idade)] #!is.na(Grupo5I$Idade) tira
#os valores missings
ValorModa_Idade <- Calcular_Moda(Vetor_Idades)
ValorModa_Idade
#Resultado: 20

#MEDIANA DA IDADE
median(Grupo5I$Idade,na.rm = TRUE)
#Resultado: 20

#QUARTIS DA IDADE
quantile(Grupo5I$Idade,na.rm = TRUE) #calcula os quartis
#Resultado: 
#valor mais baixo: 17
#1º quartil: 19
#2º quartil: 20
#3ºquartil: 21
#valor mais alto: 52

#DESVIO-PADRÃO
#Calcular variância
Variancia_Idade <- round(var(Grupo5I$Idade,na.rm = TRUE),3)
Variancia_Idade
#Resultado: 12.234
DesvioPadrao_Idade <- round(sqrt(Variancia_Idade),3)
DesvioPadrao_Idade
#Resultado: 3.498

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_Idade <- round(DesvioPadrao_Idade/Media_Idade,3)
CD_Idade
#Resultado: 0.17 disperção moderada

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_Idade <- round((Media_Idade-ValorModa_Idade)/DesvioPadrao_Idade,3)
CAP_Idade
#Resultado: 0.157 distribuição assimétrica positiva

#SEM OUTLIERS
#IDADE MÉDIA
Media_IdadeSemOutliers <- round(mean(Idade_SemOutliers$Grupo5I.Idade,na.rm = TRUE),2)
Media_IdadeSemOutliers
#Resultado: 19.97

#MODA DA IDADE
#Criar um vector com as idades
Vetor_IdadesSemOutliers <- Idade_SemOutliers$Grupo5I.Idade[!is.na(Idade_SemOutliers$Grupo5I.Idade)]
ValorModa_IdadeSemOutliers <- Calcular_Moda(Vetor_Idades)
ValorModa_IdadeSemOutliers
#Resultado: 20

#MEDIANA DA IDADE
median(Idade_SemOutliers$Grupo5I.Idade,na.rm = TRUE)
#Resultado: 20
sort(Grupo5I$Idade)
#QUARTIS DA IDADE
quantile(Idade_SemOutliers$Grupo5I.Idade,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 17
#1º quartil: 19
#2º quartil: 20
#3ºquartil: 21
#valor mais alto: 24

summary(Idade_SemOutliers)

#DESVIO-PADRÃO
#Calcular variância
Variancia_IdadeSemOutliers <- round(var(Vetor_IdadesSemOutliers),3)
Variancia_IdadeSemOutliers
#Resultado: 2.207
DesvioPadrao_IdadeSemOutliers <- round(sqrt(Variancia_IdadeSemOutliers),3)
DesvioPadrao_IdadeSemOutliers
#Resultado: 1.486

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_IdadeSemOutliers <- round(DesvioPadrao_IdadeSemOutliers/Media_IdadeSemOutliers,3)
CD_IdadeSemOutliers
#Resultado: 0.074 disperção fraca

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_IdadeSemOutliers <- round((Media_IdadeSemOutliers-ValorModa_IdadeSemOutliers)/DesvioPadrao_IdadeSemOutliers,3)
CAP_IdadeSemOutliers
#Resultado: -0.02 distribuição assimétrica negativa
#-------------------------------------------------------------------------------  
  
    
#ESTUDO DA VARIÁVEL: SEXO-------------------------------------------------------
#GRAFICO CIRCULAR PARA A DISTRIBUIÇÃO DOS ESTUDANTES POR SEXO
pie(table(Grupo5I$Sexo),cex=0.8,
    main = "Distribuição dos estudantes por sexo",cex.main=1,
    col = c("#FC717F", "#529EFF"))

legend("topleft", legend = c("Feminino", "Masculino"),
       fill = c("#FC717F", "#529EFF"),cex =0.7)


text(locator(n=1),paste(round(prop.table(table(Grupo5$Sexo))[1],
                              digits=2)*100,"%"),cex=0.7)

text(locator(n=1),paste(round(prop.table(table(Grupo5$Sexo))[2],
                              digits=2)*100,"%"),cex=0.7)

#MODA
#Resultado: É "Masculino" pois a percentagem é de 51% superior a 49% "Feminino"
#-------------------------------------------------------------------------------


#ESTUDO DA VARIÁVEL: CURSO------------------------------------------------------
#TABELA DE FREQUÊNCIAS DOS CURSOS
Fabs_Curso<-table(Grupo5I$Curso)
Fabs_Curso
Frel_Curso<-round((table(Grupo5I$Curso)/length(Grupo5I$Curso)),2)
Frel_Curso
Tabela_Curso<-cbind(Fabs_Curso,Frel_Curso)
Tabela_Curso
colnames(Tabela_Curso)<- c("Freq Absoluta","Freq Relativa")
Tabela_Curso
DF_Tabela_Curso<- data.frame(Tabela_Curso)
View(DF_Tabela_Curso)

#MODA
#Resultado: É "L.Biotecnologia" pois a percentagem é 63%, superior aos outros cursos 
--------------------------------------------------------------------------------

  
#ESTUDO DA VARIÁVEL: ANOCURRICULAR----------------------------------------------
#TABELA DE FREQUÊNCIAS DOS ANOS
Fabs_Ano <- table(Grupo5$AnoCurricular)
Fabs_Ano
Frel_Ano <- round((table(Grupo5$AnoCurricular)/length(Grupo5I$AnoCurricular)),2)
Frel_Ano
Tabela_Ano <- cbind(Fabs_Ano,Frel_Ano)
colnames(Tabela_Ano) <- c("Freq Absoluta","Freq Relativa")
rownames(Tabela_Ano) <- c("1º ano","2º ano","3ºano")
DF_Tabela_Ano <- data.frame(Tabela_Ano) 
View(DF_Tabela_Ano)

#CÁLCULO DAS MODA, MEDIANAS E QUARTIS

#MODA
#Resultado: É "1ºano" pois a percentagem é 47%, superior aos outros anos

#MEDIANA
Median_Ano <- median(Grupo5I$AnoCurricular,na.rm = TRUE)
Median_Ano
#Resultado: 2

#QUARTIS
quantile(Grupo5I$AnoCurricular,na.rm = TRUE)
#Resultado
#valor mais baixo: 1
#1º quartil: 1
#2º quartil: 2
#3ºquartil: 2
#valor mais alto: 3

#CÁLCULO COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_Ano <- (1+2-2*Median_Ano)/(2-1)
CAP_Ano
#Resultado: -1 distribuição assimétrica negativa

#-------------------------------------------------------------------------------


#ESTUDO DA VARIÁVEL: OPCAO1-----------------------------------------------------
#GRÁFICO CIRCULAR DO CURSO COMO 1ª OPÇÃO
pie(table(Grupo5I$opcao1),cex=0.8,
    main= "Gráfico circular do curso como 1ª opção", cex.main =1,
    col = c("darkorange", "blue"))

legend("topleft",legend = c("Sim", "Não"),
       fill = c("darkorange", "blue"),cex=0.7)

text(locator(n=1),paste(round(prop.table(table(Grupo5I$opcao1))[1],
                              digits=2)*100,"%"),cex=0.7)

text(locator(n=1),paste(round(prop.table(table(Grupo5I$opcao1))[2],
                              digits=2)*100,"%"),cex=0.7)
#MODA
#Resultado: É "Sim" pois a percentagem é de 53% superior ao "Não" de 47%
--------------------------------------------------------------------------------

  
#ESTUDO DA VARIÁVEL: ESCOLHI----------------------------------------------------
#GRÁFICO CIRCULAR DA ESCOLHA PESSOAL DO CURSO
pie(table(Grupo5I$Escolhi),cex=0.8,
    main= "Gráfico circular da escolha pessoal do curso", cex.main =1,
    col = c("darkorange", "blue"))

legend("topleft",legend = c("Sim", "Não"),
       fill = c("darkorange", "blue"),cex=0.7)

text(locator(n=1),paste(round(prop.table(table(Grupo5I$Escolhi))[1],
                              digits=2)*100,"%"),cex=0.7)

text(locator(n=1),paste(round(prop.table(table(Grupo5I$Escolhi))[2],
                              digits=2)*100,"%"),cex=0.7)
#MODA
#Resultado: É "Sim" pois a percentagem é de 99% superior ao "Não" de 1%
--------------------------------------------------------------------------------

  
#ESTUDO DA VARIAVÉL: TEMPODESLOCA-----------------------------------------------
#HISTOGRAMA PARA A DISTRIBUIÇÃO DO TEMPO DESLOCADO

#Criar um vetor com o tempo de deslocação e eliminar os missings
Vetor_TempoDesloca <- Grupo5I$TempoDesloca[!is.na(Grupo5I$TempoDesloca)]

#Calcular o número de classes a usar
Dimensao_TempoDesloca <- length(Vetor_TempoDesloca) #Determina a dimensão
NumeroClasse_TempoDesloca <- trunc(1+1.44*log(Dimensao_TempoDesloca))+1 #trunc
#separa a parte decimal da inteira, logo passar de um número real para um número
#inteiro

#Calcular a amplitude total da amostra
Amplitude_TempoDesloca <- max(Vetor_TempoDesloca)-min(Vetor_TempoDesloca)

#Calcular a amplitude das classes
AmplitudeClasse_TempoDesloca <- round(Amplitude_TempoDesloca/
NumeroClasse_TempoDesloca,2)

#Calcular o limite das classes
LimiteClasse_TempoDesloca <- round(seq(min(Vetor_TempoDesloca),
min(Vetor_TempoDesloca)+NumeroClasse_TempoDesloca*AmplitudeClasse_TempoDesloca,
AmplitudeClasse_TempoDesloca),3)

#Criar o histograma
Histograma_TempoDesloca <- hist(Vetor_TempoDesloca,
                                breaks = LimiteClasse_TempoDesloca,
                                axes = FALSE, freq = TRUE,
                                labels = TRUE,
                                main ="Histograma para a distribuição do tempo deslocado",
                                xlab = "Tempo em minutos",
                                ylab = "Frequência absoluta",
                                col = c("darkorange","#C59900","#95A900",
                                        "#39B600","#00BE6C","#00A5FF","blue",
                                        "blue","blue"))
                           axis(1,LimiteClasse_TempoDesloca)
                           axis(2, at = seq(0,40,by=5))
                           
#CALCULO DAS MÉDIAS, MODA, MEDIANAS, QUARTIS E DESVIO-PADRÃO
                           
#TEMPO DESLOCADO MÉDIA
Media_TempoDesloca <- round(mean(Grupo5I$TempoDesloca,na.rm = TRUE),2)
Media_TempoDesloca
#Resultado: 109.54
                           
#MODA DO TEMPO DESLOCADO
ValorModa_TempoDesloca <- Calcular_Moda(Vetor_TempoDesloca)
ValorModa_TempoDesloca
#Resultado: 180
                           
#MEDIANA DO TEMPO DESLOCADO
median(Grupo5I$TempoDesloca,na.rm = TRUE)
#Resultado: 90
                           
#QUARTIS DO TEMPO DESLOCADO
quantile(Grupo5I$TempoDesloca,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 5
#1º quartil: 50
#2º quartil: 90
#3ºquartil: 164
#valor mais alto: 480
summary(Grupo5I$TempoDesloca)

#DESVIO-PADRÃO
#Calcular variância
Variancia_TempoDesloca <- round(var(Grupo5I$TempoDesloca,na.rm = TRUE),3)
Variancia_TempoDesloca
#Resultado: 5719.716
DesvioPadrao_TempoDesloca <- round(sqrt(Variancia_TempoDesloca),3)
DesvioPadrao_TempoDesloca
#Resultado: 75.629

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_TempoDesloca <- round(DesvioPadrao_TempoDesloca/Media_TempoDesloca,3)
CD_TempoDesloca
#Resultado: 0.69 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_TempoDesloca <- round((Media_TempoDesloca-ValorModa_TempoDesloca)/DesvioPadrao_TempoDesloca,3)
CAP_TempoDesloca
#Resultado: -0.932 distribuição assimétrica negativa
--------------------------------------------------------------------------------
  

#ESTUDO DA VARIAVÉL: HORASESTUDO--------------------------------------------
#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DE HORAS DE ESTUDO POR SEMANA
boxplot(Grupo5I$HorasEstudo,
        main="Diagrama de extremos e quartis para a distribuição 
        de horas de estudo por semana",
        cex.main = 1,
        ylab = "Horas de estudo por semana",
        xlab = "Todos os alunos", 
        col = c("darkorange")) #com outliers
sort(Grupo5I$HorasEstudo)
        
#Criar dataframe que não irá ter outliers
HorasEstudo_SemOutliers <- data.frame(Grupo5I$HorasEstudo) 
View(HorasEstudo_SemOutliers)
#Função que irá remover os outliers (todos os valores iguais ou superiores a 20)
for(i in 1 : nrow(HorasEstudo_SemOutliers)){
  IsNa <- is.na(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo[i])
  if(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo[i] > 22 && IsNa == FALSE)
  {HorasEstudo_SemOutliers$Grupo5I.HorasEstudo[i] <- NA}
}

sort(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo)

#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DE HORAS DE ESTUDO POR SEMA-
#NA SEM OUTLIERS
boxplot(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo,
        main="Diagrama de extremos e quartis para a distribuição
        de horas de estudo por semana sem outliers",
        cex.main = 1,
        ylab = "Horas de estudo por semana",
        xlab = "Todos os alunos",
        col = c("blue"))

#CALCULO DAS MÉDIAS, MODA, MEDIANAS, QUARTIS E DESVIO-PADRÃO

#COM OUTLIERS
#HORAS ESTUDO MÉDIA
Media_HorasEstudo <- round(mean(Grupo5I$HorasEstudo,na.rm = TRUE),2) 
Media_HorasEstudo
#Resultado: 9.37

#MODA DAS HORAS ESTUDO
#Criar um vector com as horas de estudo
Vetor_HorasEstudo <- Grupo5I$HorasEstudo[!is.na(Grupo5I$HorasEstudo)]
ValorModa_HorasEstudo <- Calcular_Moda(Vetor_HorasEstudo)
ValorModa_HorasEstudo
#Resultado: 4

#MEDIANA DAS HORAS ESTUDO
median(Grupo5I$HorasEstudo,na.rm = TRUE)
#Resultado: 7

#QUARTIS DAS HORAS ESTUDO
quantile(Grupo5I$HorasEstudo,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 0
#1º quartil: 4
#2º quartil: 7
#3ºquartil: 12
#valor mais alto: 70

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasEstudo <- round(var(Grupo5I$HorasEstudo,na.rm = TRUE),3)
Variancia_HorasEstudo
#Resultado: 84.909
DesvioPadrao_HorasEstudo <- round(sqrt(Variancia_HorasEstudo),3)
DesvioPadrao_HorasEstudo 
#Resultado: 9.215

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasEstudo <- round(DesvioPadrao_HorasEstudo/Media_HorasEstudo,3)
CD_HorasEstudo
#Resultado: 0.983 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasEstudo <- round((Media_HorasEstudo-ValorModa_HorasEstudo)/DesvioPadrao_HorasEstudo,3)
CAP_HorasEstudo
#Resultado: 0.583 distribuição assimétrica positiva
sort(Grupo5I$HorasEstudo)

#SEM OUTLIERS
#HORAS ESTUDO MÉDIA
Media_HorasEstudoSemOutliers <- round(mean(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo,na.rm = TRUE),2)
Media_HorasEstudoSemOutliers
#Resultado: 7.24

#MODA DAS HORAS ESTUDO
#Criar um vector com as horas de estudo
Vetor_HorasEstudoSemOutliers <- HorasEstudo_SemOutliers$Grupo5I.HorasEstudo[!is.na(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo)]
ValorModa_HorasEstudoSemOutliers <- Calcular_Moda(Vetor_HorasEstudoSemOutliers)
ValorModa_HorasEstudoSemOutliers
#Resultado: 4

#MEDIANA DAS HORAS ESTUDO
median(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo,na.rm = TRUE)
#Resultado: 6

#QUARTIS DAS HORAS ESTUDO
quantile(HorasEstudo_SemOutliers$Grupo5I.HorasEstudo,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 0
#1º quartil: 4
#2º quartil: 6
#3ºquartil: 10
#valor mais alto: 18
summary(HorasEstudo_SemOutliers)

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasEstudoSemOutliers <- round(var(Vetor_HorasEstudoSemOutliers),3)
Variancia_HorasEstudoSemOutliers
#Resultado: 19.533
DesvioPadrao_HorasEstudoSemOutliers <- round(sqrt(Variancia_HorasEstudoSemOutliers),3)
DesvioPadrao_HorasEstudoSemOutliers
#Resultado: 5.136

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasEstudoSemOutliers <- round(DesvioPadrao_HorasEstudoSemOutliers/Media_HorasEstudoSemOutliers,3)
CD_HorasEstudoSemOutliers
#Resultado: 0.61 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasEstudoSemOutliers <- round((Media_HorasEstudoSemOutliers-ValorModa_HorasEstudoSemOutliers)/DesvioPadrao_HorasEstudoSemOutliers,3)
CAP_HorasEstudoSemOutliers
#Resultado: 0.742 distribuição assimétrica positiva
--------------------------------------------------------------------------------
  

#ESTUDO DA VARIÁVEL: HORASREDES-------------------------------------------------
#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DIÁRIA DE HORAS DE REDES 
boxplot(Grupo5I$HorasRedes,
        main="Diagrama de extremos e quartis para a distribuição diária de
        horas de redes",
        cex.main = 1,
        ylab = "Horas de redes diárias",
        xlab = "Todos os alunos", 
        col = c("darkorange")) #com outliers
sort(Grupo5I$HorasRedes)
#Criar dataframe que não irá ter outliers
HorasRedes_SemOutliers <- data.frame(Grupo5I$HorasRedes) 
View(HorasRedes_SemOutliers)
#Função que irá remover os outliers (todos os valores iguais ou superiores a 7)
for(i in 1 : nrow(HorasRedes_SemOutliers)){
  IsNa <- is.na(HorasRedes_SemOutliers$Grupo5I.HorasRedes[i])
  if(HorasRedes_SemOutliers$Grupo5I.HorasRedes[i] >= 7 && IsNa == FALSE)
  {HorasRedes_SemOutliers$Grupo5I.HorasRedes[i] <- NA}
}

sort(HorasRedes_SemOutliers$Grupo5I.HorasRedes)

#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DIÁRIA DE HORAS DE REDES
#SEM OUTLIERS
boxplot(HorasRedes_SemOutliers$Grupo5I.HorasRedes,
        main="Diagrama de extremos e quartis para a distribuição diária de 
        horas de redes sem outliers",
        cex.main = 1,
        ylab = "Horas de redes diárias",
        xlab = "Todos os alunos",
        col = c("blue"))

#CALCULO DAS MÉDIAS, MODA, MEDIANAS, QUARTIS E DESVIO-PADRÃO

#COM OUTLIERS
#HORAS REDES MÉDIA
Media_HorasRedes <- round(mean(Grupo5I$HorasRedes,na.rm = TRUE),2) 
Media_HorasRedes
#Resultado: 2.7

#MODA DAS HORAS REDES
#Criar um vector com as horas de redes
Vetor_HorasRedes <- Grupo5I$HorasRedes[!is.na(Grupo5I$HorasRedes)]
ValorModa_HorasRedes <- Calcular_Moda(Vetor_HorasRedes)
ValorModa_HorasRedes 
#Resultado: 2

#MEDIANA DAS HORAS REDES
median(Grupo5I$HorasRedes,na.rm = TRUE)
#Resultado: 2

#QUARTIS DAS HORAS REDES
quantile(Grupo5I$HorasRedes,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 0
#1º quartil: 1
#2º quartil: 2
#3ºquartil: 3
#valor mais alto: 13

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasRedes <- round(var(Grupo5I$HorasRedes,na.rm = TRUE),3)
Variancia_HorasRedes
#Resultado: 4.806
DesvioPadrao_HorasRedes <- round(sqrt(Variancia_HorasRedes),3)
DesvioPadrao_HorasRedes 
#Resultado: 2.192

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasRedes <- round(DesvioPadrao_HorasRedes/Media_HorasRedes,3)
CD_HorasRedes
#Resultado: 0.812 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasRedes <- round((Media_HorasRedes-ValorModa_HorasRedes)/DesvioPadrao_HorasRedes,3)
CAP_HorasRedes
#Resultado: 0.319 distribuição assimétrica positiva


#SEM OUTLIERS
#HORAS REDES MÉDIA
Media_HorasRedesSemOutliers <- round(mean(HorasRedes_SemOutliers$Grupo5I.HorasRedes,na.rm = TRUE),2)
Media_HorasRedesSemOutliers
#Resultado: 2.3

#MODA DAS HORAS REDES
#Criar um vector com as horas de redes
Vetor_HorasRedesSemOutliers <- HorasRedes_SemOutliers$Grupo5I.HorasRedes[!is.na(HorasRedes_SemOutliers$Grupo5I.HorasRedes)]
ValorModa_HorasRedesSemOutliers <- Calcular_Moda(Vetor_HorasRedesSemOutliers)
ValorModa_HorasRedesSemOutliers
#Resultado: 2

#MEDIANA DA IDADE
median(HorasRedes_SemOutliers$Grupo5I.HorasRedes,na.rm = TRUE)
#Resultado: 2

#QUARTIS DA IDADE
quantile(HorasRedes_SemOutliers$Grupo5I.HorasRedes,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 0
#1º quartil: 1
#2º quartil: 2
#3ºquartil: 3
#valor mais alto: 6
summary(HorasRedes_SemOutliers)

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasRedesSemOutliers <- round(var(Vetor_HorasRedesSemOutliers),3)
Variancia_HorasRedesSemOutliers
#Resultado: 1.856
DesvioPadrao_HorasRedesSemOutliers <- round(sqrt(Variancia_HorasRedesSemOutliers),3)
DesvioPadrao_HorasRedesSemOutliers
#Resultado: 1.362

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasRedesSemOutliers <- round(DesvioPadrao_HorasRedesSemOutliers/Media_HorasRedesSemOutliers,3)
CD_HorasRedesSemOutliers
#Resultado: 0.592 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasRedesSemOutliers <- round((Media_HorasRedesSemOutliers-ValorModa_HorasRedesSemOutliers)/DesvioPadrao_HorasRedesSemOutliers,3)
CAP_HorasRedesSemOutliers
#Resultado: 0.22 distribuição assimétrica positiva
--------------------------------------------------------------------------------
  

#ESTUDO DA VARIÁVEL: HORASTV----------------------------------------------------
#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DIÁRIA DE HORAS DE TV 
boxplot(Grupo5I$HorasTV,
        main="Diagrama de extremos e quartis para a distribuição diária de
        horas de TV",
        cex.main = 1,
        ylab = "Horas de TV diárias",
        xlab = "Todos os alunos", 
        col = c("darkorange")) #com outliers

sort(Grupo5I$HorasTV)

#Criar dataframe que não irá ter outliers
HorasTV_SemOutliers <- data.frame(Grupo5I$HorasTV) 
View(HorasTV_SemOutliers)
#Função que irá remover os outliers (todos os valores superiores a 6)
for(i in 1 : nrow(HorasTV_SemOutliers)){
  IsNa <- is.na(HorasTV_SemOutliers$Grupo5I.HorasTV[i])
  if(HorasTV_SemOutliers$Grupo5I.HorasTV[i] > 6 && IsNa == FALSE)
  {HorasTV_SemOutliers$Grupo5I.HorasTV[i] <- NA}
}

sort(HorasTV_SemOutliers$Grupo5I.HorasTV)

#DIAGRAMA DE EXTREMOS E QUARTIS PARA A DISTRIBUIÇÃO DIÁRIA DE HORAS DE TV POR
#SEM OUTLIERS
boxplot(HorasTV_SemOutliers$Grupo5I.HorasTV,
        main="Diagrama de extremos e quartis para a distribuição diária de 
        horas de TV sem outliers",
        cex.main = 1,
        ylab = "Horas de TV diárias",
        xlab = "Todos os alunos",
        col = c("blue"))

#CALCULO DAS MÉDIAS, MODA, MEDIANAS, QUARTIS E DESVIO-PADRÃO

#COM OUTLIERS
#HORAS TV MÉDIA
Media_HorasTV <- round(mean(Grupo5I$HorasTV,na.rm = TRUE),2) 
Media_HorasTV
#Resultado: 1.91

#MODA DAS HORAS TV
#Criar um vector com as horas de TV
Vetor_HorasTV <- Grupo5I$HorasTV[!is.na(Grupo5I$HorasTV)]
ValorModa_HorasTV <- Calcular_Moda(Vetor_HorasTV)
ValorModa_HorasTV
#Resultado: 1

#MEDIANA DAS HORAS TV
median(Grupo5I$HorasTV,na.rm = TRUE)
#Resultado: 2

#QUARTIS DAS HORAS TV
quantile(Grupo5I$HorasTV,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 0
#1º quartil: 1
#2º quartil: 2
#3ºquartil: 3
#valor mais alto: 8

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasTV <- round(var(Grupo5I$HorasTV,na.rm = TRUE),3)
Variancia_HorasTV
#Resultado: 2.067
DesvioPadrao_HorasTV <- round(sqrt(Variancia_HorasTV),3)
DesvioPadrao_HorasTV
#Resultado: 1.438

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasTV <- round(DesvioPadrao_HorasTV/Media_HorasTV,3)
CD_HorasTV
#Resultado: 0.753 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasTV <- round((Media_HorasTV-ValorModa_HorasTV)/DesvioPadrao_HorasTV,3)
CAP_HorasTV
#Resultado: 0.633 distribuição assimétrica positiva

#SEM OUTLIERS
#HORAS TV MÉDIA
Media_HorasTVSemOutliers <- round(mean(HorasTV_SemOutliers$Grupo5I.HorasTV,na.rm = TRUE),2)
Media_HorasTVSemOutliers
#Resultado: 1.82

#MODA DAS HORAS TV
#Criar um vector com as horas de redes
Vetor_HorasTVSemOutliers <- HorasTV_SemOutliers$Grupo5I.HorasTV[!is.na(HorasTV_SemOutliers$Grupo5I.HorasTV)]
ValorModa_HorasTVSemOutliers <- Calcular_Moda(Vetor_HorasTVSemOutliers)
ValorModa_HorasTVSemOutliers
#Resultado: 1

#MEDIANA DA IDADE
median(HorasTV_SemOutliers$Grupo5I.HorasTV,na.rm = TRUE)
#Resultado: 1.75

#QUARTIS DA IDADE
quantile(HorasTV_SemOutliers$Grupo5I.HorasTV,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 0
#1º quartil: 1
#2º quartil: 1.75
#3ºquartil: 2.875
#valor mais alto: 6
summary(HorasTV_SemOutliers)

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasTVSemOutliers <- round(var(Vetor_HorasTVSemOutliers),3)
Variancia_HorasTVSemOutliers
#Resultado: 1.588
DesvioPadrao_HorasTVSemOutliers <- round(sqrt(Variancia_HorasTVSemOutliers),3)
DesvioPadrao_HorasTVSemOutliers
#Resultado: 1.26

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasTVSemOutliers <- round(DesvioPadrao_HorasTVSemOutliers/Media_HorasTVSemOutliers,3)
CD_HorasTVSemOutliers
#Resultado: 0.692 disperção forte

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasTVSemOutliers <- round((Media_HorasTVSemOutliers-ValorModa_HorasTVSemOutliers)/DesvioPadrao_HorasTVSemOutliers,3)
CAP_HorasTVSemOutliers
#Resultado: 0.651 distribuição assimétrica positiva
--------------------------------------------------------------------------------


#ESTUDO DA VARIÁVEL: HORASSONO--------------------------------------------------
#HISTOGRAMA PARA A DISTRIBUIÇÃO DIÁRIA DAS HORAS DE SONO

#Criar um vetor com as horas de sono e eliminar os missings
Vetor_HorasSono <- Grupo5I$HorasSono[!is.na(Grupo5I$HorasSono)]

#Calcular o número de classes a usar
Dimensao_HorasSono <- length(Vetor_HorasSono)
NumeroClasse_HorasSono <- trunc(1+1.44*log(Dimensao_HorasSono))+1

#Calcular a amplitude total da amostra
Amplitude_HorasSono <- max(Vetor_HorasSono)-min(Vetor_HorasSono)

#Calcular a amplitude das classes
AmplitudeClasse_HorasSono <- round(Amplitude_HorasSono/
                                  NumeroClasse_HorasSono,2)

#Calcular o limite das classes
LimiteClasse_HorasSono <- round(seq(min(Vetor_HorasSono),
                                    min(Vetor_HorasSono)+NumeroClasse_HorasSono*AmplitudeClasse_HorasSono,
                                    AmplitudeClasse_HorasSono),3)

#Criar o histograma
Histograma_HorasSono <- hist(Vetor_HorasSono,
                                breaks = LimiteClasse_HorasSono,
                                axes = FALSE, freq = TRUE,
                                labels = TRUE,
                                main ="Histograma para a distribuição diária das horas de sono",
                                xlab = "Tempo em horas",
                                ylab = "Frequência absoluta",
                                col = c("darkorange","#C59900","#95A900",
                                        "#5BB300","#39B600","#00BE6C","#00C1AA",
                                        "#529EFF","blue"))
                              axis(1,LimiteClasse_HorasSono)
                              axis(2, at = seq(0,48,by=4))

#CALCULO DAS MÉDIAS, MODA, MEDIANAS E QUARTIS

#HORAS SONO MÉDIA
Media_HorasSono <- round(mean(Grupo5I$HorasSono,na.rm = TRUE),2)
Media_HorasSono
#Resultado: 6.96

#MODA DAS HORAS SONO
ValorModa_HorasSono <- Calcular_Moda(Vetor_HorasSono)
ValorModa_HorasSono
#Resultado: 7

#MEDIANA DAS HORAS SONO
median(Grupo5I$HorasSono,na.rm = TRUE)
#Resultado: 7

#QUARTIS DAS HORAS SONO
quantile(Grupo5I$HorasSono,na.rm = TRUE)
#Resultado: 
#valor mais baixo: 3
#1º quartil: 6
#2º quartil: 7
#3ºquartil: 8
#valor mais alto: 9
summary(Grupo5I$HorasSono)

#DESVIO-PADRÃO
#Calcular variância
Variancia_HorasSono <- round(var(Grupo5I$HorasSono,na.rm = TRUE),3)
Variancia_HorasSono
#Resultado: 0.977
DesvioPadrao_HorasSono <- round(sqrt(Variancia_HorasSono),3)
DesvioPadrao_HorasSono
#Resultado: 0.988

#CÁLCULO DO COEFICIENTE DE DISPERÇÃO E COEFICIENTE DE ASSIMETRIA DE PEARSON 

#COEFICIENTE DE DISPERÇÃO
CD_HorasSono <- round(DesvioPadrao_HorasSono/Media_HorasSono,3)
CD_HorasSono
#Resultado: 0.142 disperção fraca

#COEFICIENTE DE ASSIMETRIA DE PEARSON
CAP_HorasSono <- round((Media_HorasSono-ValorModa_HorasSono)/DesvioPadrao_HorasSono,3)
CAP_HorasSono
#Resultado: -0.04 distribuição assimétrica negativa (valor pequeno podia se con-
#siderar simétrico)
--------------------------------------------------------------------------------
  

#ESTUDO DA VARIAVÉL: PMENTORIA--------------------------------------------------
#GRÁFICO DE BARRAS DO CONHECIMENTO DO PROGRAMA DE MENTORIA

#Criar um vector para remover missings
Vector_PMentoria <- Grupo5I$PMentoria[!is.na(Grupo5I$PMentoria)]
Vector_PMentoria

#Obter frequências relativas para usar no gráfico de barras
Frel_PMentoria<-round((table(Grupo5I$PMentoria)/length(Vector_PMentoria))*100)
Frel_PMentoria

#Inserir os valores de Frel_PMentoria em PMentoria_Valores
PMentoria_Valores <- c(62,38)

barplot(PMentoria_Valores, main="Gráfico de barras do conhecimento do programa de mentoria",
        cex.main = 1,
        ylim=c(0,100),
        ylab="%",
        xlab="Resposta dos alunos",
        names.arg=c("Sim","Não"),
        col=c("darkorange","blue"))
  
legend("topright",legend = c("Sim", "Não"),
fill = c("darkorange", "blue"),cex=0.7)
        

text(locator(n=1),paste(Frel_PMentoria[1],"%"),cex=0.8)
text(locator(n=1),paste(Frel_PMentoria[2],"%"),cex=0.8)

#MODA
#Resultado: é "Sim" pois a percentagem é de 62% superior ao do "Não" de 38%

#-------------------------------------------------------------------------------
#TERMINA A ANÁLISE DESCRITIVA UNIVARIADA
#-------------------------------------------------------------------------------
  










  
#-------------------------------------------------------------------------------  
#COMEÇA A ANÁLISE DESCRITIVA BIVARIADA
#------------------------------------------------------------------------------- 
#Tabela de contigência do Burnout com segundo o sexo do Estudante
Contigencia_1Sexo <- data.frame(table(Grupo5I$Burnout_P1, Grupo5I$Sexo))
colnames(Contigencia_1Sexo) <- c("Burnout1","Sexo","Quantidade de estudantes") 
Contigencia_1Sexo$Burnout1 <- factor(Contigencia_1Sexo$Burnout1, 
                              labels = c("Nunca","Quase Nunca","Freq",
                              "Regularmente","Muitas vezes","Quase sempre","Sempre"),
                              levels = c(0,1,2,3,4,5,6))
View(Contigencia_1Sexo)
#Foi criado o dataframe Contigencia_1Sexo e mudado os valores da colunas e dos valores
#-------------------------------------------------------------------------------

#Estudo das questão do burnout 7 pelo ano curricular que frequenta.
#Vai ser usado o coeficiente de correlação de Spearman

#Tabela com o burnout 7 e ano curricular
Contigencia_7Ano <- data.frame(table(Grupo5I$Burnout_P7, Grupo5I$AnoCurricular))
colnames(Contigencia_7Ano) <- c("Burnout7","Ano Curricular","Freq") 
Contigencia_7Ano$Burnout7 <- factor(Contigencia_7Ano$Burnout7,
                              labels = c("Nunca","Quase Nunca","Algumas vezes",
                              "Regularmente","Muitas vezes","Quase sempre","Sempre"),
                              levels = c(0,1,2,3,4,5,6))
View(Contigencia_7Ano)

#Calcular coeficiente de correlação de Spearman
cor(Grupo5I$Burnout_P7,Grupo5I$AnoCurricular, method = "spearman", use = "complete.obs") 
#Resultado: 0.125

#-------------------------------------------------------------------------------
#Estudo das HorasEstudo e HorasSono
#Vai ser usado o coeficiente de correlação de Pearson

#Calcular coeficiente de correlação de Pearson

cor(Grupo5I$HorasEstudo,Grupo5I$HorasSono, method = "pearson", use = "complete.obs") 
#Resultado: -0.0492

Tabela_EstudoSono <- data.frame(Grupo5I$HorasEstudo,Grupo5I$HorasSono)
colnames(Tabela_EstudoSono) <- c("Horas Estudo","Horas Sono") 
View(Tabela_EstudoSono)

#Regressão linear

Regressao_EstudoSono <- lm(Grupo5I$HorasEstudo~Grupo5I$HorasSono) #lm calcula a regressão linear
Regressao_EstudoSono
#Resultado Intercept 12.6397 at -0.4636

#Diagrama de dispersão para as variáveis: HorasEstudo e HorasSono
Dispersao_EstudoSono <- plot(Grupo5I$HorasEstudo,Grupo5I$HorasSono, main="Diagrama de dispersão para as horas de estudo e do sono com regressão linear",
                            xlab = "Horas de estudo semanais",
                            ylab = "Horas de sono diárias",
                            pch = 2, col = "darkorange",
                            abline(Regressao_EstudoSono, col ="blue"))#cria a reta da regressão linear
#-------------------------------------------------------------------------------
#TERMINA A ANÁLISE DESCRITIVA BIVARIADA
#-------------------------------------------------------------------------------











#-------------------------------------------------------------------------------
#COMEÇA O ESTUDO INFERENCIAL
#-------------------------------------------------------------------------------
#Estudar burnout 12 com ano curricular
#Estudar burnout 14 com o curso
#Estudar burnout 1 com o sexo

#-------------------------------------------------------------------------------
#Burnout 1 com o sexo: Pretende-se comparar o nível de exaustam que os estudant-
#es dos sexos femininos e masculinos
#H0 - O nível de exaustam é independente do sexo;
#H1 - O nível de exaustam não é independente do sexo;

#Testes para duas populações, masculino e feminino
#Variável independente: sexo
#Variável dependente: burnout1

#Realização do teste Mann-Whitney
wilcox.test(as.numeric(Grupo5I$Sexo),Grupo5I$Burnout_P1)
#Resultado: Rejeita-se a H0, o nível de exaustam não é independente do sexo

#Representação gráfica por um boxplot

boxplot(Grupo5I$Burnout_P1~Grupo5I$Sexo,main = "Gráfico da distribuição do burnout 1 por sexo", 
        ylab ="Burnout P1", xlab = "Sexo",col = c("pink","lightblue"))

#O sexo feminino fica mais exausto que o masculino

#-------------------------------------------------------------------------------
#Burnout 12 X ano curricular
#QUESTÃO DE INVESTIGAÇÃO:
#Será que os estudantes sentem-se melhores alunos com o progredir do curso?

#H0: &1 = &2 =&3 - As medianas são iguais.   (Hipostese nula)
#H1: Existe pelo menos um par de medianas diferentes (Hipótese alternativa)

#teste de Kruskal-Wallis
#(3 populações- alunos do 1º,2º e 3ºano/variável qual. ordinal )
kruskal.test(Grupo5I$Burnout_P12 ~Grupo5I$AnoCurricular, data = Grupo5I)
#p-value = 0.112 >0.05 não se rejeita

#Gráfico do Burnout_P12 e o AnoCurricular
boxplot(Grupo5I$Burnout_P12~Grupo5I$AnoCurricular,main = "Gráfico da distribuição do burnout 12 por ano curricular", 
        ylab ="Burnout P12", xlab = "Ano Curricular",col = c("darkorange","gray","blue"))
#-------------------------------------------------------------------------------
#Burnout 14 X curso
#QUESTÃO DE INVESTIGAÇÃO:
#Será que os alunos do curso de Bioinformática tem tido mais interesse
#comparativamente com os alunos dos outros cursos?

#H0: &1 = &2 =&3 - As medianas são iguais.   (Hipostese nula)
#H1: Existe pelo menos um par de medianas diferentes (Hipótese alternativa)

#teste de Kruskal-Wallis
kruskal.test(Grupo5I$Burnout_P14 ~Grupo5I$Curso,data =   Grupo5I )
#p-value = 0.9645 > 0.05, não se rejeita H0.

#Gráfico do Burnout_P14 e o Curso
boxplot(Grupo5I$Burnout_P14~Grupo5I$Curso,main = "Gráfico da distribuição do burnout 14 por curso", 
        ylab ="Burnout P14", xlab = "Curso",col = c("darkorange","gray","blue"))

#-------------------------------------------------------------------------------
#Regressão Linear Múltipla
Regressao_Multipla <- lm(Grupo5I$HorasEstudo~Grupo5I$HorasRedes + Grupo5I$HorasTV)
Regressao_Multipla
#Intercept 6.947 , HorasRedes: 0.01356, HorasEstudo: -0.00534

#Fazer o gráfico
plot(Regressao_Multipla,
     col = "darkorange",
     pch = 2)
     
#Análise gráfica
par(mfrow=c(2,2))
plot(Regressao_Multipla,
     col = "darkorange",
     pch = 2)
par(mfrow=c(1,1))

#Normalidade dos residuos
shapiro.test(Regressao_Multipla$residuals)
#Resultado: w = 0.71373, p = 3.56e-14 <- não é uma distribuição normal

#Remover os outliers 7, 79 e 117 para tentar obter uma distribuição normal
HTV1 <- Grupo5I$HorasTV[-c(7,79,117)]
HTV1

HE1 <- Grupo5I$HorasEstudo[-c(7,79,117)]
HE1

HR1 <- Grupo5I$HorasRedes[-c(7,79,117)]
HR1


Regressao_Multipla2 <- lm(HE1~HR1 + HTV1)
Regressao_Multipla2

plot(Regressao_Multipla2,
     col = "darkorange",
     pch = 2)

#Análise gráfica
par(mfrow=c(2,2))
plot(Regressao_Multipla2,
     col = "darkorange",
     pch = 2)
par(mfrow=c(1,1))

#Normalidade dos residuos
shapiro.test(Regressao_Multipla2$residuals)
#Resultado: w = 0.91756, p = 1.136e-6 <- não é uma distribuição normal

#Outliers nos residuos
summary(rstandard(Regressao_Multipla2))
#Os valores saem do intervalo [-3,3] são do tipo [-1.309347,4.869080]

#Homocedasticidade
bptest(Regressao_Multipla2)
#P = 0.4595 existe homocedasticidade

#Independência dos resíduos
durbinWatsonTest(Regressao_Multipla2)
#P value 0.266 não se rejeita

#Ausência de multicolinearidade
dados <- data.frame(HE1, HR1, HTV1)
pairs.panels(dados)
#O valor do r é inferior 0.9
vif(Regressao_Multipla2)
#O valor do VIF é inferior ao 10 é 1.00247

#Análise 
summary(Regressao_Multipla2)
#P value = 0.8285 não se rejeita H0
--------------------------------------------------------------------------------
#Análise fatorial

#Vai ser selecionado as variáveis burnout
burnout<-Grupo5[,14:28]
head(burnout)

#Correlação entre as variáveis burnout
matcor <- cor(burnout,use="complete.obs")
print(matcor,digits = 2)

lowerCor(burnout)

#Medida de adequabilidade de Kaiser-Mayer-Olkin
KMO(burnout)
#Como todos valores são superiores a 0.5 todas a variáveis ajustam á
#estrutura

#Teste de esfericidade de Bartlett
cortest.bartlett(matcor,n=nrow(burnout))
#Resultados: p = 6.278e-159 | df = 105| Logo é rejeitado a HO e existe uma nível
# de correlação substancial.

library(psych)
#Critério de Kaiser
fit <- princomp(na.omit(burnout), cor = TRUE)
fit
summary(fit)
#Pelo critério de Kaiser é sugerido 3 fatores.

#Gráfico de linhas do fit
screeplot(fit)
plot(fit,type = "lines")
#Pelo gráfico é sugerio 4 fatores.


#Obtenção da solução com o número de fatores sugeridos
fit1 <- principal(burnout, nfactors = 3, n.obs = nrow(Grupo5), rotate = "none",
                 scores = TRUE)
fit1
fit1$loadings

fit2 <- principal(burnout, nfactors = 4, n.obs = nrow(Grupo5), rotate = "none",
                 scores = TRUE)
fit2
fit2$loadings
#Poderá para o fit1 existir alguns problema pois existem valores de comunalidade a menos de
#0.6. No fit2 não apresenta esses problemas.

#Representação gráfica com 3 fatores
biplot(fit1)
abline(h=0)
abline(v=0)

#Representação gráfica com 4 fatores
biplot(fit2)
abline(h=0)
abline(v=0)

#Rotação da solução fatorial de 4

fit2_varimax <- principal(burnout, nfactors = 4, n.obs = nrow(Grupo5I),
                          rotate = "varimax", scores = TRUE)

fit2_varimax

biplot(fit2_varimax)
abline(h=0)
abline(v=0)
library(GPArotation)
#Modelo do fatorial de 4
fa.diagram(fit2_varimax)

#Criação de fatores para avaliar a sua consistência
Fator1 <- data.matrix(Grupo5I[,14:18])
Fator2 <- data.matrix(Grupo5I[,19:22])
Fator3 <- data.matrix(Grupo5I[,25:28])
Fator4 <- data.matrix(Grupo5I[,23:24])

alpha(Fator1)
alpha(Fator2)
alpha(Fator3)
alpha(Fator4)

