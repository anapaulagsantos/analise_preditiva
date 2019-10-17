library(readxl)
HR <- read_excel("E:/4 - Análise Preditiva/Prova/H&R.xlsx")
View(HR)

#rodar modelo de regressao multipla - usando todas as variaveis
# y = GrossSales

#resume todas as variaveis
summary(HR)

#roda a regressao
reg.mlt = lm(data=HR, GrossSales~.)

#Questao A - Coeficiente da variavel LINES = 1,414
#sumariza os dados gerados na regressao
summary(reg.mlt)

#Questao B - Há evidencias de pontos influentes? Se sim, quais sao?
#resposta = sim, pontos 5 e 27
#estatistica utilizada para detectar os pontos = Estatistica D de Cook

#instalar pacote que tem ferramentas para analisar regressao
library(car)

influenceIndexPlot(reg.mlt,vars=c("Cook", "Studentized", "hat"), main="Diagnostic Plots",)


#Questao C: Utilizando os pacotes RMS ou CAR, calcular o VIF.
#maior valor de VIF = 3,5, sem evidencia de multicolinearidade

#calcular VIF - carrega a biblioteca que calcula o VIF
library(rms)

#calcula o VIF arredondando (round) para 1 casa decimal
round(vif(reg.mlt),1)

#Questao D - erro percentual para estimar GrossSales para a primeira observacao da planilha

#calcula o valor com a equacao de regressao de GrossSales chapeu
HR$GrossSales_Hat=fitted.values(reg.mlt) 

#calcula residuo com a diferenca do valor original para o price chapeu
HR$RES=residuals(reg.mlt) 
View(HR)

#calcula o erro percentual -> EP = 19,877
HR$EP=HR$RES/HR$GrossSales*100

#plota grafico
plot(HR$EP)
grid(col=4)
