library(readxl)
sportgv <- read_excel("E:/4 - Análise Preditiva/Prova/sportgv.xlsx")
View(sportgv)

sportgv

summary (sportgv)

#criar arvore de decisao a partir das variaveis previsoras fornecidas, utilizando rpart

library(rpart)
library(rpart.plot)
#sempre que for fazer uma arvore precisa rodar o setseed pq a arvore contem numeros
#aleatorios
set.seed(93)

#rpart = particiona a base em pedacos
#method = class -> porque a variavel target eh qualitativa (resposta 0 ou 1)
ad1=rpart(data=sportgv, promo~PROD_A+PROD_B+PROD_C+REGIAO+SETOR+TMPCLI+SEDE+FUNC, method = "class")

#gera grafico
#(ad1 = arvore de decisao 1), branch.lty = 5 (faz o pontilhado)
prp(ad1, type=2, extra=104,nn=T, fallen.leaves = T, branch.lty = 5)

#monta dataframe com os valores das variaveis preditoras
#PROD_A	PROD_B	PROD_C	REGIAO	SETOR	TMPCLI	SEDE	FUNC	promo
#0,00	   7,10	    3,27	SUL	     IND	NEW	     0	  10	   1

novoad<-data.frame(PROD_A = 0, PROD_B = 7.1, PROD_C = 3.27, REGIAO = "SUL", SETOR = "IND", TMPCLI = "NEW", SEDE = 0, FUNC =10)

#vamos analisar na amostra teste
#ad2 eh a arvore podada
sportgv$klass=predict(ad1, newdata = sportgv,type = "class")

#matriz de classificacao
table(sportgv$promo,sportgv$klass)

#vamos calcular a probabilidade de promo="sim"
#nao usar class como type pq ele classifica em sim ou nao usando o ponto de corte 0,5
pr=predict(ad1, newdata = sportgv,type = "prob")

#gera a tabela com probabilidades - calcula as probabilidades de um individuo
#da amostra pertencer a cada uma das classes da variavel alvo.

#Questao A1 - saber a probabilidade do segundo individuo da amostrar ser sim = 0,21 = 21%
#(col 1 da linha 2)
head(pr)

#Questao A2 - Empresa quer agir quando a probabilidade de responder as promocoes for maior que
#0,75. Calcular % de individuos que receberah emails = 0,52 = 52%
#filtra a base
PC=.75
prop.table(table(sportgv$promo)) #fornece a resposta

######################################################################

#regressao logistica

#rodar modelo de regressao logistica para prever PROMO. Aplicar o metodo STEP e considerar 
#somente o modelo obtido apos a selecao de variaveis

set.seed(123)

#roda a regressao -> #family binomial eh por causa do 0 e 1
mod1=glm(data = sportgv, promo~.,family = binomial() )  
summary(mod1)


#seleciona variaveis pela funcao step
mod2=step(mod1)
summary(mod2)

#roda a previsao do modelo 2
sportgv$ps=predict(mod2, newdata = sportgv, type = "response")

#Questao B1 - calcular a probabilidade do seegundo individuo da planilha responder as promocoes

#printa os primeiros 6 valores - probabilidade do individuo 2 = 41,43%
print(head(sportgv$ps), digits=3)

#Questao B2 - Qual a area sob a curva ROC para o modelo obtido apos a selecao das variaveis?
library (hmeasure)
#avaliacao do modelo
#curva roc = 0,9685
HMeasure (sportgv$promo,sportgv$ps) $metric


