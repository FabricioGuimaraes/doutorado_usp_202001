library(ggplot2)
library(tseries)
library(forecast)

#soja <- read.table("D:\\workspace\\soybean.csv", header=TRUE, row.names=NULL, sep = ",")
#milho <- read.table("D:\\workspace\\corn.csv", header=TRUE, row.names=NULL, sep = ",")

#le os arquivos com os dados de forma decrescente
commodity <- read.table("D:\\workspace\\coffee.csv", header=TRUE, row.names=NULL, sep = ",")
ptax <- read.table("D:\\workspace\\ptax.csv", header=TRUE, row.names=NULL, sep = ",")

#inverte a lista de dados, de forma ascendente 1900 - 2000. 
#o arquivo csv esta no formato 2000 - 1900
commodity <- commodity[seq(dim(commodity)[1],1),]
ptax <- ptax[seq(dim(ptax)[1],1),]

#Quais colunas possuem nesta lista
summary(commodity)
summary(ptax)
ccf(commodity$Settle, ptax$Value)

commodity.stl <- stl(ts(commodity$Settle, frequency = 252), s.window = "periodic")

#Avalia se a serie e estacionaria, Analise da funcao de autocorrelação(ACF) e autocorrelação parcial(PACF)
#Aqui o decaimento do parametro AC
par(mfrow=c(2,1))
acf(commodity$Settle)
pacf(commodity$Settle)
Box.test(commodity$Settle, lag = 1, type = "Ljung-Box")
adf.test(commodity$Settle, alternative = "stationary")


#Faz uma (lag = 1) para a diferenciacao da serie
commodity_Settle_diff <- diff(commodity$Settle, lag = 1, differences = 1)
#vemos que pela funcao ACF, apos o primeiro lag, ha um decaimento exponencial, 
#o que caracteriza a serie como estacionaria
acf(commodity_Settle_diff)
pacf(commodity_Settle_diff)
Box.test(commodity_Settle_diff, lag = 1, type = "Ljung-Box")
adf.test(commodity_Settle_diff, alternative = "stationary")


#Visualizacao das series
ts.plot(commodity$Settle)
ts.plot(commodity_Settle_diff)


#verifica qual melhores parametros para p, d, q (AR, Diferencas,MA)
auto.arima(commodity$Settle)
auto.arima(ptax$Value)


##FORECAST
#http://www.face.ufg.br/siteface_files/midias/original-nt-002.pdf
#https://rpubs.com/riazakhan94/arima_with_example
#Model Estimation -> with the least AIC and significant co-efficients
#evidence of AR(1)
#Com o comando tsdiag é possível analisar os gráficos dos resíduos (O modelo deve
#apresentar os resíduos estacionários, com média zero e variância constante)
#coffee ARIMA(0,1,0)
#soybean ARIMA(1,0,0)
#corn ARIMA(0,1,0)
#ptax ARIMA(0,1,0) with drift 
fit = arima(commodity$Settle, order=c(0,1,0))
fit_residual = residuals(fit)

#Avalia a estacionariedade dos residuos. O modelo apresenta residos estacionários
acf(fit_residual)
pacf(fit_residual)
Box.test(fit_residual, lag= 7, type = "Ljung-Box")
adf.test(fit_residual, alternative = "stationary")

#diagnosis
tsdiag(fit)

#FORECAST
commodity_forecast = forecast(fit, h = 10)
plot(commodity_forecast)
lines(fitted(fit),col="blue")
accuracy(commodity_forecast, commodity[1:10, 5])
#Validar os residuos 
checkresiduals(fit)

########ATE AQUI ESTA ################
##########DOCUMENTADO#################

#predict 
predicted <- predict(fit, n.ahead=10)
lines(predicted$pred,col='blue')
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
lines(predicted$pred+2*predicted$se,col='blue',lty=5)
lines(predicted$pred-2*predicted$se,col='blue',lty=5)
autoplot(predicted$pred)
#predict

#NAIVE
?naive
previsao_naive = naive(commodity$Settle)
autoplot(previsao_naive)
#autoplot(residuals(previsao_naive))
checkresiduals(previsao_naive)
summary(previsao_naive)
#NAIVE

#SES
?ses
previsao_ses = ses(commodity$Settle, h = 10) # Previsao para os próximos 5 tempos
autoplot(previsao_ses)
checkresiduals(previsao_ses)
summary(previsao_ses)
#SES

#HOLT
?holt
previsao_holt = holt(commodity$Settle, h = 10)
autoplot(previsao_holt)
checkresiduals(previsao_holt)
summary(previsao_holt)
#HOLT
