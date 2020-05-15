##Exercicio 6
##Ajuste modelo para a serie do numero anual de automoveis montados no Brasil (Anfavea) a partir de 1995 (obs. 39). Faca
##o grafico da serie, funcao de autocorrelacao amostral e proponha um modelo de acordo com o que observou nos graficos.
##d <- read.csv("../automoveismontadosanfavea.csv",sep=";")
##ts.plot(ts(d[,2], freq = 1, start = 1957))
##y <- ts(d[39:62,2], freq = 1, start =1994)
##ts.plot(y)
##ts.plot(diff(y))
##mean(diff(y)); abline(h = mean(diff(y)))
##acf((diff(y)))


#Bibliotecas para descobrir quais melhores parametros para o modelo. E avaliacao dos mesmos
library(tseries)
library(forecast)

d <- read.csv("D:\\workspace\\automoveismontadosanfavea.csv",sep=";")  # le o arquivo
y <- ts(d[39:62,2], freq = 1, start =1994) #serie de dados de 1994 a 2017

par(mfrow=c(2,1)) #plotar a serie e a serie derivada no mesmo quadro para comparacoes
ts.plot(y) 
mean(y); abline(h = mean(y))
y_diff <- diff(y) #primeira diferenca da serie
ts.plot(y_diff) 
mean(y_diff); abline(h = mean(y_diff))

par(mfrow=c(2,1)) #plotar a ACF (auto correlation function) da serie [y] e da serie derivada [y_diff]
acf_y <- acf(y)
acf_y_diff <- acf(y_diff)

par(mfrow=c(2,1)) #plotar a PACF (partial auto correlation function) da serie [y] e da serie derivada [y_diff]
pacf_y <- pacf(y)
pacf_y_diff <- pacf(y_diff)

#podemos verificar que tirando a primeira diferenca as serie se torna estacionaria
par(mfrow=c(2,2)) #plotar a ACF e PACF das series para avaliar de forma comparativa
plot(acf_y)
plot(acf_y_diff)
plot(pacf_y)
plot(pacf_y_diff)

#Valida se a serie ou a serie derivada sao estacionarias
adf.test(y, alternative = "stationary") #nao estacionaria p-value = 0.9436 > 0.5
adf.test(y_diff, alternative = "stationary") #estacionaria p-value = 0.2794 < 0.5

#Alguns testes manuais de combinacao de modelos a procura dos melhores parametros, para analises de forma manual, apenas testes#
arima(y, order = c(1,0,0)) # aic = 671.17
arima(y_diff, order = c(1,0,0)) # aic = 640.45 *melhor
arima(y, order = c(2,0,0)) # aic = 671.08
arima(y_diff, order = c(2,0,0)) # aic = 639.08 *melhor

arima(y, order = c(0,0,1)) # aic = 685.96
arima(y_diff, order = c(0,0,1)) # aic = 639.02 *melhor
arima(y, order = c(0,0,2)) # aic = 676.56
arima(y_diff, order = c(0,0,2)) #  aic = 639.49 *melhor

arima(y, order = c(1,0,1)) # aic = 668.88
arima(y_diff, order = c(1,0,1)) # aic = 640.29 *melhor
arima(y, order = c(2,0,1)) # aic = 670.59
arima(y_diff, order = c(2,0,1)) # aic = 640.76 *melhor
#Alguns testes manuais de combinacao de modelos a procura dos melhores parametros, para analises de forma manual, apenas testes#

## Melhor que os testes manuais anteiores, podemos utilizar a funcao auto.arima
## a qual procura a melhor combinacao de parametros para criacao do modelo para determinada serie 
auto_y <- auto.arima(y) # melhor combinacao ARIMA(0,1,0). AIC=638.67   AICc=638.86   BIC=639.8
auto_y_diff <- auto.arima(y_diff) # serie (derivada primeira) eh melhor ARIMA(0,0,0) with zero mean AIC=638.67   AICc=638.86   BIC=639.8

#Avaliacao dos modelos
arima.final <- arima(y, order = c(0,0,0)) #esta NAO e a melhor combinacao
tsdiag(arima.final)

arima.final_diff <- arima(y_diff, order = c(0,0,0)) #esta e a melhor combinacao
tsdiag(arima.final_diff)
#Avaliacao dos modelos

#
#O melhor modelo para o exercicio proposto eh o modelo ARIMA(0,1,0)
#
