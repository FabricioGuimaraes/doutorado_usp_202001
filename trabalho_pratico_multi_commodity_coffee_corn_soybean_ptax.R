###Trabalho pratico para a disciplina analise de series temporais
###Prof> Airlane Pereira Alencar
###Aluno> Fabricio Guimaraes
###Feito com mais commodities, a fim de validar a correlacao entre elas.

library(ggplot2)
library(tseries)
library(forecast)

#ptax.csv
#le os arquivos com os dados de forma decrescente
cafe <- read.table("D:\\workspace\\coffee.csv", header=TRUE, row.names=NULL, sep = ",")
soja <- read.table("D:\\workspace\\soybean.csv", header=TRUE, row.names=NULL, sep = ",")
milho <- read.table("D:\\workspace\\corn.csv", header=TRUE, row.names=NULL, sep = ",")
ptax <- read.table("D:\\workspace\\ptax.csv", header=TRUE, row.names=NULL, sep = ",")
#auto.arima(ptax$Value)

#inverte a lista de dados, de forma ascendente 1900 - 2000. 
#o arquivo csv esta no formato 2000 - 1900
cafe <- cafe[seq(dim(cafe)[1],1),]
soja <- soja[seq(dim(soja)[1],1),]
milho <- milho[seq(dim(milho)[1],1),]
ptax <- ptax[seq(dim(ptax)[1],1),]

#Quais colunas possuem nesta lista
summary(cafe$Settle)
summary(soja$Settle)
summary(milho$Settle)
summary(ptax$Value)
par(mfrow=c(3,1))
ccf(cafe$Settle, ptax$Value)
ccf(soja$Settle, ptax$Value)
ccf(milho$Settle, ptax$Value)

cafe.stl <- stl(ts(cafe$Settle, frequency = 252), s.window = "periodic")
plot(cafe.stl)
soja.stl <- stl(ts(soja$Settle, frequency = 252), s.window = "periodic")
plot(soja.stl)
milho.stl <- stl(ts(milho$Settle, frequency = 252), s.window = "periodic")
plot(milho.stl)
ptax.stl <- stl(ts(ptax$Value, frequency = 250), s.window = "periodic")
plot(ptax.stl)


#Avalia se a serie e estacionaria, Analise da funcao de autocorrelação(ACF) e autocorrelação parcial(PACF)
#Aqui o decaimento do parametro AC
par(mfrow=c(2,2))
acf(cafe$Settle)
acf(soja$Settle)
acf(milho$Settle)
acf(ptax$Value)

pacf(cafe$Settle)
pacf(soja$Settle)
pacf(milho$Settle)
pacf(ptax$Value)

Box.test(cafe$Settle, lag = 1, type = "Ljung-Box")
Box.test(soja$Settle, lag = 1, type = "Ljung-Box")
Box.test(milho$Settle, lag = 1, type = "Ljung-Box")
Box.test(ptax$Value, lag = 1, type = "Ljung-Box")

adf.test(cafe$Settle, alternative = "stationary")
adf.test(soja$Settle, alternative = "stationary")
adf.test(milho$Settle, alternative = "stationary")
adf.test(ptax$Value, alternative = "stationary")


#Faz uma (lag = 1) para a diferenciacao da serie
cafe_Settle_diff <- diff(cafe$Settle, lag = 1, differences = 1)
soja_Settle_diff <- diff(soja$Settle, lag = 1, differences = 1)
milho_Settle_diff <- diff(milho$Settle, lag = 1, differences = 1)
ptax_Value_diff <- diff(ptax$Value, lag = 1, differences = 1)
#vemos que pela funcao ACF, apos o primeiro lag, ha um decaimento exponencial, 
#o que caracteriza a serie como estacionaria

summary(cafe_Settle_diff)
summary(soja_Settle_diff)
summary(milho_Settle_diff)
summary(ptax_Value_diff)

acf(cafe_Settle_diff)
acf(soja_Settle_diff)
acf(milho_Settle_diff)
acf(ptax_Value_diff)

pacf(cafe_Settle_diff)
pacf(soja_Settle_diff)
pacf(milho_Settle_diff)
pacf(ptax_Value_diff)

Box.test(cafe_Settle_diff, lag = 1, type = "Ljung-Box")
Box.test(soja_Settle_diff, lag = 1, type = "Ljung-Box")
Box.test(milho_Settle_diff, lag = 1, type = "Ljung-Box")
Box.test(ptax_Value_diff, lag = 1, type = "Ljung-Box")

adf.test(cafe_Settle_diff, alternative = "stationary")
adf.test(soja_Settle_diff, alternative = "stationary")
adf.test(milho_Settle_diff, alternative = "stationary")
adf.test(ptax_Value_diff, alternative = "stationary")

#Visualizacao das series
ts.plot(cafe$Settle)
ts.plot(soja$Settle)
ts.plot(milho$Settle)
ts.plot(ptax$Value)

ts.plot(cafe_Settle_diff)
ts.plot(soja_Settle_diff)
ts.plot(milho_Settle_diff)
ts.plot(ptax_Value_diff)

#verifica qual melhores parametros para p, d, q (AR, qte diff,MA)
#ARIMA(0,1,0)
auto.arima(cafe$Settle)
#ARIMA(1,0,0)
auto.arima(soja$Settle)
#ARIMA(0,1,0)
auto.arima(milho$Settle)
#ARIMA(0,1,0) with drift 
auto.arima(ptax$Value)


##FORECAST
#criacao dos modelos arima de acordo com os melhores parametros
#encontrados atraves da funcao auto.arima()
#coffee ARIMA(0,1,0)
#soybean ARIMA(1,0,0)
#corn ARIMA(0,1,0)
#ptax ARIMA(0,1,0) with drift
fit_cafe = arima(cafe$Settle, order=c(0,1,0))
fit_cafe_residual = residuals(fit_cafe)
fit_soja = arima(soja$Settle, order=c(1,0,0))
fit_soja_residual = residuals(fit_soja)
fit_milho = arima(milho$Settle, order=c(0,1,0))
fit_milho_residual = residuals(fit_milho)
fit_ptax = arima(ptax$Value, order=c(0,1,0))
fit_ptax_residual = residuals(fit_ptax)

plot(fit_cafe_residual)
plot(fit_soja_residual)
plot(fit_milho_residual)
plot(fit_ptax_residual)

#Avalia a estacionariedade dos residuos. O modelo apresenta residos estacionários
acf(fit_cafe_residual)
acf(fit_soja_residual)
acf(fit_milho_residual)
acf(fit_ptax_residual)

pacf(fit_cafe_residual)
pacf(fit_soja_residual)
pacf(fit_milho_residual)
pacf(fit_ptax_residual)

Box.test(fit_cafe_residual, lag= 7, type = "Ljung-Box")
Box.test(fit_soja_residual, lag= 7, type = "Ljung-Box")
Box.test(fit_milho_residual, lag= 7, type = "Ljung-Box")
Box.test(fit_ptax_residual, lag= 7, type = "Ljung-Box")

adf.test(fit_cafe_residual, alternative = "stationary")
adf.test(fit_soja_residual, alternative = "stationary")
adf.test(fit_milho_residual, alternative = "stationary")
adf.test(fit_ptax_residual, alternative = "stationary")

#diagnosis
tsdiag(fit_cafe)
tsdiag(fit_soja)
tsdiag(fit_milho)
tsdiag(fit_ptax)


#FORECAST
cafe_forecast = forecast(fit_cafe, h = 10)
plot(cafe_forecast)
lines(fitted(fit_cafe),col="blue")
accuracy(cafe_forecast, cafe[1:10, 5])
#Validar os residuos 
checkresiduals(cafe_forecast)

soja_forecast = forecast(fit_soja, h = 10)
plot(soja_forecast)
lines(fitted(fit_soja),col="blue")
accuracy(soja_forecast, soja[1:10, 5])
#Validar os residuos 
checkresiduals(soja_forecast)

milho_forecast = forecast(fit_milho, h = 10)
plot(milho_forecast)
lines(fitted(fit_milho),col="blue")
accuracy(milho_forecast, milho[1:10, 5])
#Validar os residuos 
checkresiduals(milho_forecast)

#FORECAST
ptax_forecast = forecast(fit_ptax, h = 10)
plot(ptax_forecast)
lines(fitted(fit_ptax),col="blue")
accuracy(ptax_forecast, ptax[1:10, 1])
#Validar os residuos 
checkresiduals(ptax_forecast)



###################
#predict
par(mfrow=c(2,2))

predicted_cafe <- predict(fit_cafe, n.ahead=10)
accuracy(predicted_cafe$pred, cafe[1:10, 5])
plot(predicted_cafe$pred, col='blue')
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
lines(predicted_cafe$pred+2*predicted_cafe$se,col='green',lty=5)
lines(predicted_cafe$pred-2*predicted_cafe$se,col='red',lty=5)

predicted_soja <- predict(fit_soja, n.ahead=10)
accuracy(predicted_soja$pred, soja[1:10, 5])
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
plot(predicted_soja$pred, col='blue')
lines(predicted_soja$pred+2*predicted_soja$se,col='green',lty=5)
lines(predicted_soja$pred-2*predicted_soja$se,col='red',lty=5)

predicted_milho <- predict(fit_milho, n.ahead=10)
accuracy(predicted_milho$pred, milho[1:10, 5])
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
plot(predicted_milho$pred, col='blue')
lines(predicted_milho$pred+2*predicted_milho$se,col='green',lty=5)
lines(predicted_milho$pred-2*predicted_milho$se,col='red',lty=5)

predicted_ptax <- predict(fit_ptax, n.ahead=10)
accuracy(predicted_ptax$pred, ptax[1:10, 1])
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
plot(predicted_ptax$pred, col='blue')
lines(predicted_ptax$pred+2*predicted_ptax$se,col='green',lty=5)
lines(predicted_ptax$pred-2*predicted_ptax$se,col='red',lty=5)
#predict


#NAIVE ?naive
par(mfrow=c(2,2))

previsao_naive_cafe = naive(cafe$Settle)
plot(previsao_naive_cafe) #autoplot(previsao_naive_cafe)
#checkresiduals(previsao_naive_cafe)
summary(previsao_naive_cafe)

previsao_naive_soja = naive(soja$Settle)
plot(previsao_naive_soja)
#checkresiduals(previsao_naive_soja)
summary(previsao_naive_soja)

previsao_naive_milho = naive(milho$Settle)
plot(previsao_naive_milho)
#checkresiduals(previsao_naive_milho)
summary(previsao_naive_milho)

previsao_naive_ptax = naive(ptax$Value)
plot(previsao_naive_ptax)
#checkresiduals(previsao_naive_ptax)
summary(previsao_naive_ptax)

#NAIVE

#SES ?ses
par(mfrow=c(2,2))

previsao_ses_cafe = ses(cafe$Settle, h = 10) # Previsao para os próximos 10 tempos
plot(previsao_ses_cafe)
checkresiduals(previsao_ses_cafe)
summary(previsao_ses_cafe)

previsao_ses_soja = ses(soja$Settle, h = 10) # Previsao para os próximos 10 tempos
plot(previsao_ses_soja)
checkresiduals(previsao_ses_soja)
summary(previsao_ses_soja)

previsao_ses_milho = ses(milho$Settle, h = 10) # Previsao para os próximos 10 tempos
plot(previsao_ses_milho)
checkresiduals(previsao_ses_milho)
summary(previsao_ses_milho)

previsao_ses_ptax = ses(ptax$Value, h = 10) # Previsao para os próximos 10 tempos
plot(previsao_ses_ptax)
checkresiduals(previsao_ses_ptax)
summary(previsao_ses_ptax)

#SES

#HOLT ?holt
par(mfrow=c(2,2))

previsao_holt_cafe = holt(cafe$Settle, h = 10)
plot(previsao_holt_cafe)
checkresiduals(previsao_holt_cafe)
summary(previsao_holt_cafe)

previsao_holt_soja = holt(soja$Settle, h = 10)
plot(previsao_holt_soja)
checkresiduals(previsao_holt_soja)
summary(previsao_holt_soja)

previsao_holt_milho = holt(milho$Settle, h = 10)
plot(previsao_holt_milho)
checkresiduals(previsao_holt_milho)
summary(previsao_holt_milho)

previsao_holt_ptax = holt(ptax$Value, h = 10)
plot(previsao_holt_ptax)
checkresiduals(previsao_holt_ptax)
summary(previsao_holt_ptax)


#HOLT
