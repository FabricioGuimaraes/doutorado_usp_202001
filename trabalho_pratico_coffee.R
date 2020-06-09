###Trabalho pratico para a disciplina analise de series temporais
###Prof> Airlane Pereira Alencar
###Aluno> Fabricio Guimaraes
###Feito apenas com uma commodity, para avaliar como a serie temporal se comporta

library(ggplot2)
library(tseries)
library(forecast)

#coffee.csv soybean.csv corn.csv
#le os arquivos com os dados de forma decrescente
#soja <- read.table("D:\\workspace\\soybean.csv", header=TRUE, row.names=NULL, sep = ",")
#milho <- read.table("D:\\workspace\\corn.csv", header=TRUE, row.names=NULL, sep = ",")
commodity <- read.table("D:\\workspace\\coffee.csv", header=TRUE, row.names=NULL, sep = ",")
ptax <- read.table("D:\\workspace\\ptax.csv", header=TRUE, row.names=NULL, sep = ",")

#inverte a lista de dados, de forma ascendente 1900 - 2000. 
#o arquivo csv esta no formato 2000 - 1900
commodity <- commodity[seq(dim(commodity)[1],1),]
ptax <- ptax[seq(dim(ptax)[1],1),]


#Quais colunas possuem nesta lista
summary(commodity$Settle)
summary(ptax$Value)
ccf(commodity$Settle, ptax$Value)

commodity.stl <- stl(ts(commodity$Settle, frequency = 252), s.window = "periodic")
plot(commodity.stl)
ptax.stl <- stl(ts(ptax$Value, frequency = 250), s.window = "periodic")
plot(ptax.stl)


#Avalia se a serie e estacionaria, Analise da funcao de autocorrelação(ACF) e autocorrelação parcial(PACF)
#Aqui o decaimento do parametro AC
par(mfrow=c(2,1))
acf(commodity$Settle)
pacf(commodity$Settle)

acf(ptax$Value)
pacf(ptax$Value)

Box.test(commodity$Settle, lag = 1, type = "Ljung-Box")
Box.test(ptax$Value, lag = 1, type = "Ljung-Box")

adf.test(commodity$Settle, alternative = "stationary")
adf.test(ptax$Value, alternative = "stationary")


#Faz uma (lag = 1) para a diferenciacao da serie
commodity_Settle_diff <- diff(commodity$Settle, lag = 1, differences = 1)
ptax_Value_diff <- diff(ptax$Value, lag = 1, differences = 1)
#vemos que pela funcao ACF, apos o primeiro lag, ha um decaimento exponencial, 
#o que caracteriza a serie como estacionaria

summary(commodity_Settle_diff)
summary(ptax_Value_diff)

acf(commodity_Settle_diff)
pacf(commodity_Settle_diff)

acf(ptax_Value_diff)
pacf(ptax_Value_diff)

Box.test(commodity_Settle_diff, lag = 1, type = "Ljung-Box")
Box.test(ptax_Value_diff, lag = 1, type = "Ljung-Box")

adf.test(commodity_Settle_diff, alternative = "stationary")
adf.test(ptax_Value_diff, alternative = "stationary")

#Visualizacao das series
ts.plot(commodity$Settle)
ts.plot(ptax$Value)

ts.plot(commodity_Settle_diff)
ts.plot(ptax_Value_diff)

#verifica qual melhores parametros para p, d, q (AR, qte diff,MA).
#Parametros>> stepwise = FALSE, approximation = FALSE, seasonal = FALSE
#sem>> coffee ARIMA(0,1,0) soybean ARIMA(1,0,0) corn ARIMA(0,1,0) ptax ARIMA(0,1,0) with drift
#com >> coffee ARIMA(2,1,2) soybean ARIMA(1,0,0) corn ARIMA(0,1,0) ptax ARIMA(0,1,0) with drift
auto.arima(commodity$Settle, stepwise = FALSE, approximation = FALSE, seasonal = FALSE) # stepwise = FALSE, approximation = FALSE, seasonal = FALSE
#ARIMA(0,1,0) with drift mesmo valor com ou sem parametros
auto.arima(ptax$Value)


##FORECAST
#criacao dos modelos arima de acordo com os melhores parametros
#encontrados atraves da funcao auto.arima()
fit_commodity = arima(commodity$Settle, order=c(2,1,2))
fit_commodity_residual = residuals(fit_commodity)
fit_ptax = arima(ptax$Value, order=c(0,1,0))
fit_ptax_residual = residuals(fit_ptax)

plot(fit_commodity_residual)
plot(fit_ptax_residual)

#Avalia a estacionariedade dos residuos. O modelo apresenta residos estacionários
acf(fit_commodity_residual)
pacf(fit_commodity_residual)

acf(fit_ptax_residual)
pacf(fit_ptax_residual)

Box.test(fit_commodity_residual, lag= 7, type = "Ljung-Box")
Box.test(fit_ptax_residual, lag= 7, type = "Ljung-Box")

adf.test(fit_commodity_residual, alternative = "stationary")
adf.test(fit_ptax_residual, alternative = "stationary")

#diagnosis
tsdiag(fit_commodity)
tsdiag(fit_ptax)


#FORECAST
par(mfrow=c(1,1))

commodity_forecast = forecast(fit_commodity, h = 10)
plot(commodity_forecast)
lines(fitted(fit_commodity),col="blue")
accuracy(commodity_forecast, commodity[1:10, 5])
#Validar os residuos 
checkresiduals(commodity_forecast)

#FORECAST
ptax_forecast = forecast(fit_ptax, h = 10)
plot(ptax_forecast)
lines(fitted(fit_ptax),col="blue")
accuracy(ptax_forecast, ptax[1:10, 1])
#Validar os residuos 
checkresiduals(ptax_forecast)



###################
#predict
predicted_commodity <- predict(fit_commodity, n.ahead=10)
accuracy(predicted_commodity$pred, commodity[1:10, 5])
plot(predicted_commodity$pred, col='blue')
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
lines(predicted_commodity$pred+2*predicted_commodity$se,col='green',lty=5)
lines(predicted_commodity$pred-2*predicted_commodity$se,col='red',lty=5)

predicted_ptax <- predict(fit_ptax, n.ahead=10)
accuracy(predicted_ptax$pred, ptax[1:10, 1])
#Para considerar um intervalo de conficanca de 95% , usamos dois desvio-padrao (+/- 2)
plot(predicted_ptax$pred, col='blue')
lines(predicted_ptax$pred+2*predicted_ptax$se,col='green',lty=5)
lines(predicted_ptax$pred-2*predicted_ptax$se,col='red',lty=5)
#predict


#NAIVE ?naive
previsao_naive_commodity = naive(commodity$Settle)
plot(previsao_naive_commodity) #autoplot(previsao_naive_cafe)
checkresiduals(previsao_naive_commodity)
summary(previsao_naive_commodity)

previsao_naive_ptax = naive(ptax$Value)
plot(previsao_naive_ptax)
#checkresiduals(previsao_naive_ptax)
summary(previsao_naive_ptax)
#NAIVE

#SES ?ses
#simple exponencial smothing, data with no clean trend or seasonal
previsao_ses_commodity = ses(commodity$Settle, h = 10) # Previsao para os próximos 10 tempos
plot(previsao_ses_commodity)
checkresiduals(previsao_ses_commodity)
summary(previsao_ses_commodity)

previsao_ses_ptax = ses(ptax$Value, h = 10) # Previsao para os próximos 10 tempos
plot(previsao_ses_ptax)
checkresiduals(previsao_ses_ptax)
summary(previsao_ses_ptax)
#SES

#HOLT ?holt
previsao_holt_commodity = holt(commodity$Settle, h = 10, damped=TRUE)
plot(previsao_holt_commodity)
checkresiduals(previsao_holt_commodity)
summary(previsao_holt_commodity)

previsao_holt_ptax = holt(ptax$Value, h = 10)
plot(previsao_holt_ptax)
checkresiduals(previsao_holt_ptax)
summary(previsao_holt_ptax)
#HOLT



#######################################################################################
#########Testes que ainda nao ficaram legal ainda
#####GetDFPData para buscar informacoes sobre as empresas. Estou tentando montar uma carteira Markowitz
#####com informacoes de fundos de investimento. Por exemplo, separar por tipo de fundo
#####cambial, multimercado, renda fixa, acoes

# library(GetDFPData)
# gdfpd.get.info.companies(type.data = "b2wdigital")
# 
# my.companies <- c("B2W - COMPANHIA DIGITAL") 
# first.date  <- "2010-01-01"
# last.date   <- "2020-05-26"
# type.export <- "csv"

# get data using gdfpd.GetDFPData
# This can take a while since the local data is not yet cached..
# df.reports <- gdfpd.GetDFPData(name.companies = my.companies, 
#                                first.date = first.date, 
#                                last.date  = last.date)
# 
# gdfpd.export.DFP.data(df.reports = df.reports, type.export = type.export)


#########Tentativa de uso da library urca
# library(urca)
# x <- ur.df(cafe$Settle)
# y <- ur.df(cafe_Settle_diff)

#########Tentativa de uso da library pracma, para cruzamento de medias moveis
#install.packages('pracma')
# library(pracma)
# moving.avg <- ts(movavg(cafe$Settle, n=5, type="s"))
# plot(moving.avg)

#########Tentativa de uso da funcao decompose, porem utilizei a stl()
# cafe.decomp <- decompose(cafe$Settle)


