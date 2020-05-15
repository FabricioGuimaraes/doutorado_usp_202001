##Exercicio 7
##Ajuste modelo para a WWWusage que esta no programa R.
#library(graphics)
#ts.plot(WWWusage)
#dWWW <- diff(WWWusage)
#plot(dWWW)
#acf(dWWW)
#pacf(dWWW)

#install.packages("tseries")
#install.packages("forecast")

##Bibliotecas para descobrir quais melhores parametros para o modelo. E avaliacao dos mesmos
library(graphics)
library(forecast)

par(mfrow=c(2,1))
ts.plot(WWWusage) #plotando grafico da serie com a media
mean(WWWusage); abline(h = mean(WWWusage)) 
WWWusage_diff <- diff(WWWusage) #plotando grafico, da primeira derivada da serie, com uma linha na media
plot(WWWusage_diff)
mean(WWWusage_diff); abline(h = mean(WWWusage_diff))

summary(WWWusage)
summary(WWWusage_diff)

par(mfrow=c(2,1))  #plotar a ACF (partial auto correlation function) da serie [y] e da serie derivada [y_diff]
acf(WWWusage)
acf(WWWusage_diff)

par(mfrow=c(2,1))  #plotar a PACF (partial auto correlation function) da serie [y] e da serie derivada [y_diff]
pacf(WWWusage)
pacf(WWWusage_diff)

adf.test(WWWusage, alternative = "stationary") # estacionaria p-value = 0.3107 < 0.5
adf.test(WWWusage_diff, alternative = "stationary") #estacionaria p-value = 0.3506 < 0.5

auto_y <- auto.arima(WWWusage) # os parametros para o modelo ARIMA(1,1,1) 
auto_y_diff <- auto.arima(WWWusage_diff) # os parametros para o modelo ARIMA(1,0,1) with zero mean 


arima.final <- arima(WWWusage, order = c(1,1,1)) #esta e a melhor combinacao aic = 514.3
tsdiag(arima.final)
arima.final_diff <- arima(WWWusage_diff, order = c(1,0,1)) #esta e a melhor combinacao # aic = 515.58
tsdiag(arima.final_diff)

#Para ajustar o modelo, podemos tirar a primeira derivada e usar o modelo ARIMA(1,0,1).
#Ou utilizar o modelo ARIMA(1,1,1) sem derivar a serie