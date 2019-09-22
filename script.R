library(tidyverse)

#importar dataframe Boston

library(readxl)
dados = read_excel("E:/4 - An�lise Preditiva/Trabalho/dados.xlsx")

View(dados)

#### Analisar e tratar os dados ####

# 1) Analise univariada

#exclusao da coluna id

boston=dados[,-1]

# a) Missing values

#sumarizacao para identificar a quantidade de missing (deixei em branco os valores =0)

summary (boston)

#Em virtude da quantidade de missing, exclui a coluna 2 (zn)
boston=boston[,-2] 

# b) Outliers

#boxplot das variaveis:

par(mfrow=c(1,3))
boxplot(boston$crim, main="crim", col="blue")
boxplot(boston$indus, main="indus",col="yellow")
boxplot(boston$nox, main="nox",col="green")

boxplot(boston$rm, main="rm", col="blue")
boxplot(boston$age, main="age",col="yellow")
boxplot(boston$dis, main="dis",col="green")

boxplot(boston$rad, main="rad", col="blue")
boxplot(boston$tax, main="tax",col="yellow")
boxplot(boston$ptratio, main="ptratio",col="green")

boxplot(boston$lstat, main="lstat", col="blue")
boxplot(boston$medv, main="medv",col="yellow")

#gerar logaritimos das variaveis que apresentaram outliers

boston$lcrim=log(boston$crim)
boston$lrm=log(boston$rm)
boston$ldis=log(boston$dis)
boston$lptratio=log(boston$ptratio)
boston$llstat=log(boston$lstat)
boston$lmedv=log(boston$medv)

#refaz graficos com antes de depois
par(mfrow=c(1,3))
boxplot(boston$crim, main="crim",col="yellow")
boxplot(boston$lcrim, main="lcrim", col="blue")

par(mfrow=c(1,3))
boxplot(boston$rm, main="rm",col="yellow")
boxplot(boston$lrm, main="lrm", col="blue")

par(mfrow=c(1,3))
boxplot(boston$dis, main="dis",col="yellow")
boxplot(boston$ldis, main="ldis", col="blue")

par(mfrow=c(1,3))
boxplot(boston$ptratio, main="ptratio",col="yellow")
boxplot(boston$lptratio, main="lptratio", col="blue")

par(mfrow=c(1,3))
boxplot(boston$lstat, main="lstat",col="yellow")
boxplot(boston$llstat, main="llstat", col="blue")

par(mfrow=c(1,3))
boxplot(boston$medv, main="medv",col="yellow")
boxplot(boston$lmedv, main="lmedv", col="blue")

####TRATAR OS OUTLIERS QUE NAO FORAM CORRIGIDOS COM A FUNCAO LOG

#elimina outlier remanescente de llstat
boston=subset(boston, llstat>0.8 )

par(mfrow=c(1,3))
boxplot(boston$lstat, main="lstat",col="yellow")
boxplot(boston$llstat, main="llstat", col="blue")

#tratando outlier de rm
#substituir todas as linhas que possuem "RM" menor que 5 para 5 e maior que 7 para 7. 

boston$nrm= ifelse(boston$rm<5, 5,boston$rm)
boston$nrm= ifelse(boston$nrm>7, 7,boston$nrm)

par(mfrow=c(1,3))
boxplot(boston$rm, main="rm",col="yellow")
boxplot(boston$nrm, main="nrm", col="blue")

#tratando outlier de ptratio
#substituir todas as linhas que possuem "ptratio" menor que 14 para 14.

boston$nptratio= ifelse(boston$ptratio<14, 14,boston$ptratio)

par(mfrow=c(1,3))
boxplot(boston$ptratio, main="ptratio",col="yellow")
boxplot(boston$nptratio, main="nptratio", col="blue")

#tratando outlier de medv
#substituir todas as linhas que possuem "medv" maiores que 40 por 40.

boston$nmedv= ifelse(boston$medv>37, 37,boston$medv)

par(mfrow=c(1,3))
boxplot(boston$medv, main="medv",col="yellow")
boxplot(boston$nmedv, main="nmedv", col="blue")

##########TESTE - NAO UTLIZADO##########
#calcular o percentil 95 e 2 para as variaveis que continuam com outlier
#quantile(boston$rm, c(.95))
#quantile(boston$rm, c(.02))
#elimina outlier de rm
#boston2=subset(boston, rm < 7.515 & rm > 4.9068 )
#boston=subset(boston, rm < 7.515 & rm > 4.9068 )
########################################


# c) Medidas descritivas e graficos

#exclus�o das variaveis substituidas pelo tratamento dos outliers
View (boston)
#gera arquivo csv para facilitar a identificacao das colunas a serem excluidas
write.csv(boston,"E:\\4 - An�lise Preditiva\\Trabalho\\boston_antes.csv", row.names = FALSE)

boston=boston[c("lcrim", "indus", "chas", "nox", "nrm", "age", "ldis", "rad", "tax", "nptratio", "llstat", "nmedv")]

summary (boston)

# 2) Analise bivariada (regressao linear - resposta vs. variavel previsora)

# a) Verificar relacao entre as variaveis

options(scipen = 999) #evita nota��o cient�fica

#a) nmedv x lcrim

reg_lin_lcrim=lm(data = boston, nmedv~lcrim)

summary(reg_lin_lcrim)

plot(boston$lcrim, boston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x lcrim")
grid(col=2)
abline(reg_lin_lcrim, col="red", lwd=2)

#b) nmedv x indus

reg_lin_indus=lm(data = boston, nmedv~indus)

summary(reg_lin_indus)

plot(boston$indus, boston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x indus")
grid(col=2)
abline(reg_lin_indus, col="red", lwd=2)

#c) nmedv x chas

reg_lin_chas=lm(data = boston, nmedv~chas)

summary(reg_lin_chas)

plot(boston$chas, boston$nmedv, pch=16, cex=1.5, col="blue", main = "nmedv x chas")
grid(col=2)
abline(reg_lin_chas, col="red", lwd=2)


# 3) Correlacoes entre as variaveis previsoras

# a) Identificar possíveis colinearidades

#### Rodar modelo com todas as variáveis e eventuais interações ####

# 4) Analisar sinais dos coeficientes
# 5) Avaliar R2
# 6) Testes de hipóteses
# 7) Diagnóstico (resíduos (TRES), alavancagem (hat), influentes (Cook))
# 8) Verificar existência de multicolinearidade; corrigir

#### Selecionar variáveis ####
# 9) Definir método(s) de seleção
# 10) Selecionar diferentes conjuntos de variáveis  diferentes modelos
# 11) Rodar e comparar diferentes modelos selecionados
# 12) Para cada modelo

# a) Analisar modelo selecionado
# b) Analisar sinais dos coeficientes
# c) Analisar p-values
# d) Diagnóstico (resíduos (TRES), alavancagem (hat), influentes (Cook))
# e) Verificar existência de multicolinearidade; corrigir

#### Avaliar capacidade preditiva do modelo (erros percentuais, MAPE) ####

#### Validar modelo selecionado ####
