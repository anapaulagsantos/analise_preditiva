library(tidyverse)

#importar dataframe Boston

library(readxl)
dados = read_excel("E:/4 - Análise Preditiva/Trabalho/dados.xlsx")

View(dados)

#### Analisar e tratar os dados ####

# 1) Analise univariada

#exclusao da coluna id

boston=dados[,-1]

# a) Missing values: nao tem
summary (boston)

# b) Outliers

#boxplot das variaveis:

par(mfrow=c(1,3))
boxplot(boston$crim, main="crim", col="blue")
boxplot(boston$zn, main="zn",col="yellow")
boxplot(boston$indus, main="indus",col="green")

boxplot(boston$nox, main="nox",col="blue")
boxplot(boston$rm, main="rm", col="yellow")
boxplot(boston$age, main="age",col="green")

boxplot(boston$dis, main="dis",col="blue")
boxplot(boston$rad, main="rad", col="yellow")
boxplot(boston$tax, main="tax",col="green")

boxplot(boston$ptratio, main="ptratio",col="blue")
boxplot(boston$lstat, main="lstat", col="yellow")
boxplot(boston$medv, main="medv",col="green")

#gerar logaritimos das variaveis que apresentaram outliers

#a) crim: 
  boston$lcrim = log10(boston$crim)
  
  #refaz o grafico
  par(mfrow=c(1,3))
  boxplot(boston$crim, main="crim",col="yellow")
  boxplot(boston$lcrim, main="lcrim", col="blue")

# b) zn: acrescentando 1 pois log10(1) = 0 e log10(0) = erro

boston$lzn = log10(boston$zn + 1) 

#refaz o grafico
par(mfrow=c(1,3))
boxplot(boston$zn, main="zn",col="yellow")
boxplot(boston$lzn, main="lzn", col="blue")

# c) rm:

#boston$lrm= log(boston$rm) não resolve o outlier

#refaz o grafico
#par(mfrow=c(1,3))
#boxplot(boston$rm, main="rm",col="yellow")
#boxplot(boston$lrm, main="lrm", col="blue")

#como as técnicas de logaritimos nao funcionaram, a partir da variavel original "rm", optou-se por
#substituir todas as linhas que possuem "RM" menor que 5 para 5 e maior que 7 para 7. 

boston$nrm= ifelse(boston$rm<5, 5,boston$rm)
boston$nrm= ifelse(boston$nrm>7, 7,boston$nrm)

par(mfrow=c(1,3))
boxplot(boston$rm, main="rm",col="yellow")
boxplot(boston$nrm, main="nrm", col="blue")

# d) dis:

boston$ldis=log10(boston$dis)

#refaz o grafico
par(mfrow=c(1,3))
boxplot(boston$dis, main="dis",col="yellow")
boxplot(boston$ldis, main="ldis", col="blue")

# e) ptratio

#boston$lptratio=log10(boston$ptratio) #nao resolve os outliers

max(boston$ptratio)
boston$sptratio = sqrt(22+1-boston$ptratio)

#refaz o grafico
par(mfrow=c(1,3))
boxplot(boston$ptratio, main="ptratio",col="yellow")
boxplot(boston$sptratio, main="sptratio", col="blue")

# f) lstat
boston$llstat=log(boston$lstat)

#refaz o grafico
par(mfrow=c(1,3))
boxplot(boston$lstat, main="lstat",col="yellow")
boxplot(boston$llstat, main="llstat", col="blue")

#elimina outlier que sobrou

min (boston$llstat)
boston$llstat= ifelse(boston$llstat<0.6, 0.6,boston$llstat)

#refaz o grafico
par(mfrow=c(1,3))
boxplot(boston$lstat, main="lstat",col="yellow")
boxplot(boston$llstat, main="llstat", col="blue")

# g) medv
#boston$lmedv=log(boston$medv) #nao resolveu o outlier

#refaz o grafico
#par(mfrow=c(1,3))
#boxplot(boston$medv, main="medv",col="yellow")
#boxplot(boston$lmedv, main="lmedv", col="blue")

#tratando outlier de medv
#substituir todas as linhas que possuem "medv" maiores que 37 por 37.

max(boston$medv)
boston$nmedv= ifelse(boston$medv>37, 37,boston$medv)

#refaz o grafico
par(mfrow=c(1,3))
boxplot(boston$medv, main="medv",col="yellow")
boxplot(boston$nmedv, main="nmedv", col="blue")

# c) Medidas descritivas e graficos

#selecao das variaveis que ficarao no modelo
View (boston)

nboston=boston[c("lcrim", "lzn", "indus", "chas", "nox", "nrm", "age", "ldis", "rad", "tax", "sptratio", "llstat", "nmedv")]

summary (nboston)

# 2) Analise bivariada (regressao linear - resposta vs. variavel previsora)

# a) Verificar relacao entre as variaveis

options(scipen = 999) #evita notação científica

#a) nmedv x lcrim

reg_lin_lcrim=lm(data = nboston, nmedv~lcrim)

summary(reg_lin_lcrim)

plot(nboston$lcrim, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x lcrim")
grid(col=2)
abline(reg_lin_lcrim, col="red", lwd=2)

#b) nmedv x lzn

reg_lin_lzn=lm(data = nboston, nmedv~lzn)

summary(reg_lin_lzn)

plot(nboston$lzn, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x lzn")
grid(col=2)
abline(reg_lin_lzn, col="red", lwd=2)


#c) nmedv x indus

reg_lin_indus=lm(data = nboston, nmedv~indus)

summary(reg_lin_indus)

plot(nboston$indus, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x indus")
grid(col=2)
abline(reg_lin_indus, col="red", lwd=2)

#d) nmedv x chas

reg_lin_chas=lm(data = nboston, nmedv~chas)

summary(reg_lin_chas)

plot(nboston$chas, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x chas")
grid(col=2)
abline(reg_lin_chas, col="red", lwd=2)

#e) nmedv x nox

reg_lin_nox=lm(data = nboston, nmedv~nox)

summary(reg_lin_nox)

plot(nboston$nox, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x nox")
grid(col=2)
abline(reg_lin_nox, col="red", lwd=2)

#f) nmedv x nrm

reg_lin_nrm=lm(data = nboston, nmedv~nrm)

summary(reg_lin_nrm)

plot(nboston$nrm, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x nrm")
grid(col=2)
abline(reg_lin_nrm, col="red", lwd=2)

#g) nmedv x age

reg_lin_age=lm(data = nboston, nmedv~age)

summary(reg_lin_age)

plot(nboston$age, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x age")
grid(col=2)
abline(reg_lin_age, col="red", lwd=2)

#h) nmedv x ldis

reg_lin_ldis=lm(data = nboston, nmedv~ldis)

summary(reg_lin_ldis)

plot(nboston$ldis, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x ldis")
grid(col=2)
abline(reg_lin_ldis, col="red", lwd=2)

#i) nmedv x rad

reg_lin_rad=lm(data = nboston, nmedv~rad)

summary(reg_lin_rad)

plot(nboston$rad, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x rad")
grid(col=2)
abline(reg_lin_rad, col="red", lwd=2)

# j) Nmedv x tax:

reg_lin_tax=lm(data = nboston, nmedv~tax)

summary(reg_lin_tax)

plot(nboston$tax, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x tax")
grid(col=2)
abline(reg_lin_tax, col="red", lwd=2)

# k) Nmedv x sptratio:

reg_lin_sptratio=lm(data = nboston, nmedv~sptratio)

summary(reg_lin_sptratio)

plot(nboston$sptratio, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x sptratio")
grid(col=2)
abline(reg_lin_sptratio, col="red", lwd=2)

# l) Nmedv x llstat:

reg_lin_llstat=lm(data = nboston, nmedv~llstat)

summary(reg_lin_llstat)

plot(nboston$llstat, nboston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x llstat")
grid(col=2)
abline(reg_lin_llstat, col="red", lwd=2)

# 3) Correlacoes entre as variaveis previsoras

install.packages("corrplot")
library(corrplot)

cor_prev = cor(nboston) # Corr matrix
round(cor_prev, 2)

corrplot(cor(nboston), method = "circle") #matriz grafica

corrplot(cor(nboston), method = "pie")   #matriz grafica

corrplot (cor_prev, type="upper")

corrplot(cor(nboston), method = "number")   #matriz grafica

# a) Identificar possiveis colinearidades

library (Hmisc)

cor_prev = rcorr (as.matrix(nboston))

cor_prev$r

corrplot (cor_prev$r, p.mat=cor_prev$P, sig.level = 0.005, method="number", type = "upper")

#### Rodar modelo com todas as variaveis 

#Antes de rodar o modelo de regressao com todas as variaveis, optou-se por dividir 
#o dataset nboston para viabilizar o treino e a validacao do modelo.

# dataset para treino: treino.nboston
# dataset para validacao: teste.nboston
#
# Qualquer alteracao em treino.nboston tera que ser feita tb no teste.nboston

library(caTools)

set.seed(1334598) # permite repetir procedimento aleatorio obtendo o mesmo resultado em cada execucao

# separando base de treinamento e teste
amostra = sample.split(nboston$nmedv, SplitRatio=0.70)

# criando dados de treino
treino.nboston = subset(nboston, amostra==TRUE)

# criando dados de teste
teste.nboston = subset(nboston, amostra==FALSE)

#roda a regressao dos 2 dataset com todas as variaveis

reg.lin_treino.nboston = lm(treino.nboston,formula=nmedv~.)

reg.lin_teste.nboston = lm(teste.nboston,formula=nmedv~.)

#sumariza os dados gerados para treino
summary(reg.lin_treino.nboston)

#sumariza os dados gerados para teste
summary(reg.lin_teste.nboston)

# 4) Analisar sinais dos coeficientes
# 5) Avaliar R2
# 6) Testes de hipoteses

# 7) Diagnostico (residuos (TRES), alavancagem (hat), influentes (Cook))

#instalar pacote que tem ferramentas para analisar regressao
library(car)

influenceIndexPlot(reg.lin_treino.nboston,vars=c("Cook", "Studentized", "hat"), main="Diagnostic Plots",)

#retirar o pontos 113, 262, 264 e 265.
treino.nboston1 = treino.nboston [-c(113, 262, 264, 265), ]

#roda a regressão do novo data frame
reg.mlt_treino.nboston1=lm(data=treino.nboston1, nmedv~.)

#sumariza os dados gerados
summary(reg.mlt_treino.nboston1)

#novo diagnostico

influenceIndexPlot(reg.mlt_treino.nboston1,vars=c("Cook", "Studentized", "hat"), main="Diagnostic Plots nboston 1",)

# 8) Verificar existencia de multicolinearidade de nboston2

#carrega a biblioteca que calcula o VIF
library(rms)

#calcula o VIF arredondando (round) para 1 casa decimal
round(vif(reg.mlt_treino.nboston1),1)

#nao teve vif maior que 10, logo nao ha multicolinearidade

#### Selecionar variaveis ####

# 9) Definir metodo(s) de selecao

#Sera utilizado o metodo Information Criterion de Akaike (AIC), 
#para selecao das variaveis.

#####modelos

#modelo 1

reg.mlt_treino.nboston1_AIC = step(reg.mlt_treino.nboston1)

summary(reg.mlt_treino.nboston1_AIC)

#diagnostico modelo 1

influenceIndexPlot(reg.mlt_treino.nboston1_AIC,vars=c("Cook", "Studentized", "hat"), main="Diagnostic Plots treino.nboston1_AIC",)

#VIF modelo 1
round(vif(reg.mlt_treino.nboston1_AIC),1)

#nao teve vif maior que 10, logo nao ha multicolinearidade


#modelo 2

#cria dataframe sem rad
treino.nboston1_vif= treino.nboston1[c("lcrim", "lzn", "indus", "chas", "nox", "nrm", "age", "ldis", "tax", "sptratio", "llstat", "nmedv")]

reg.mlt_treino.nboston1_vif = lm(data=treino.nboston1_vif, nmedv~.)

summary(reg.mlt_treino.nboston1_vif)

#diagnostico modelo 2

influenceIndexPlot(reg.mlt_treino.nboston1_vif,vars=c("Cook", "Studentized", "hat"), main="Diagnostic Plots treino.nboston1_vif",)

#VIF modelo 2
round(vif(reg.mlt_treino.nboston1_vif),1)

#nao teve vif maior que 10, logo nao ha multicolinearidade

#### Avaliar capacidade preditiva do modelo (erros percentuais, MAPE) ####

#previsao 

# modelo 1

#nmedv_hat é o valor que é calculado com a equacao de regressao 
cp_modelo1 <- tibble(nmedv_hat=fitted.values(reg.mlt_treino.nboston1_AIC))

#calculo do valor residual
cp_modelo1 <- mutate(cp_modelo1, RES=residuals(reg.mlt_treino.nboston1_AIC))

#calcular o valor residual percentual (divide o residuo pelo valor observado x 100)
cp_modelo1 <- mutate(cp_modelo1, EP=cp_modelo1$RES/cp_modelo1$nmedv_hat*100)

#gera grafico de erro
plot(cp_modelo1$EP, main="Erro percentual Modelo AIC", ylab="Residuos/valores previstos") # valores em %
grid(col=4)

summary (cp_modelo1)

#mape
library(modelr)

mape(reg.mlt_treino.nboston1_AIC, treino.nboston1)
#[1] 0.1341746

# modelo 2

#nmedv_hat é o valor que é calculado com a equacao de regressao 
cp_modelo2 <- tibble(nmedv_hat=fitted.values(reg.mlt_treino.nboston1_vif))

#calculo do valor residual
cp_modelo2 <- mutate(cp_modelo2, RES=residuals(reg.mlt_treino.nboston1_vif))

#calcular o valor residual percentual (divide o residuo pelo valor observado x 100)

cp_modelo2 <- mutate(cp_modelo2, EP=cp_modelo2$RES/cp_modelo2$nmedv_hat*100)

#gera grafico de erro
plot(cp_modelo2$EP, main="Erro percentual Modelo VIF", ylab="Residuos/valores previstos") # valores em %
grid(col=4)

summary (cp_modelo2)

#mape 

mape(reg.mlt_treino.nboston1_vif,treino.nboston1_vif)
#[1] 0.1358396

#### Validar modelo 1 ####

# prevendo os resultados

summary(reg.mlt_treino.nboston1_AIC)
str(teste.nboston)

#ajuste a base de teste para as mesmas variáveis do treino
teste.nboston1_AIC <- teste.nboston[c("lcrim","chas", "nox", "nrm","age","ldis","rad","tax","sptratio","llstat","nmedv")]
str(teste.nboston1_AIC)

#calculo dos novos valores a partir do modelo
prevendo <- predict(reg.mlt_treino.nboston1_AIC, teste.nboston1_AIC)

#comparando as previsoes com as observacoes "reais"
resultados <-  round(cbind(prevendo,teste.nboston1_AIC$nmedv, prevendo-teste.nboston1_AIC$nmedv, (prevendo-teste.nboston1_AIC$nmedv)*100/teste.nboston1_AIC$nmedv),1)

# ajustando os nomes das colunas
colnames(resultados) <- c("Previsto","Real","Diferença", "%")

# transformando o data set
resultados <- as.data.frame(resultados)

View(resultados)
summary (resultados)

#cria grafico
plot(resultados$`%`,pch=16,col="blue")
grid(col=4)
