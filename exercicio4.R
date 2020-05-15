#(ex. 15) Simule numeros aleatorios seguindo o processo {at} tal que as variaveis at ??? N(0,1) sejam independentes. 
#A partir desses valores simulados, obtenha series de 1000 observacoes correspondentes aos processos (b), (c) e (e) 
#do problema 1. Analise a funcao de autocorrelacao e autocorrelacao parcial de cada serie simulada

# gera variaveis aleatorias independentes  N(0,1)s
variaveis_iid = rnorm(252, mean=0, sd=1) 

#Modelo b do exercicio 2
modelo_b <- arima.sim(list(variaveis_iid, ar = c(-0.3, 0.585), ma = c(1)), n = 1000)

#Modelo c do exercicio 2
modelo_c <- arima.sim(list(variaveis_iid, ar = c(-0.4), ma = c(-0.3, -0.8)), n = 1000)

#Modelo e do exercicio 2
modelo_e <- arima.sim(list(variaveis_iid, ma = c(-1.8, 0.72)), n = 1000)

#analise do modelo b
par(mfrow=c(2,1)) 
acf(modelo_b) 
pacf(modelo_b)

#analise do modelo c
par(mfrow=c(2,1)) 
acf(modelo_c) 
pacf(modelo_c)

#analise do modelo e
par(mfrow=c(2,1)) 
acf(modelo_e)
pacf(modelo_e)


#####Abaixo outra maneira de se fazer
#####Utilizando outra funcao, ARMAacf para calculo do acf e do pacf

#MODELO B
ACF = ARMAacf(ar=c(-0.3, 0.585), ma=1, 30)
PACF = ARMAacf(ar=c(-0.3, 0.585), ma=1, 30, pacf=TRUE) 
par(mfrow=c(2,1)) 
plot(ACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0) #aqui podemos ver o decamimento exponencial para 0
plot(PACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0) #aqui vemos que a os valores ficam oscilando para 0

#MODELO C
ACF = ARMAacf(ar=c(-0.4), ma=c(-0.3, -0.8), 30)
PACF = ARMAacf(ar=c(-0.4), ma=c(-0.3, -0.8), 30, pacf=TRUE) 
par(mfrow=c(2,1)) 
plot(ACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0) #aqui podemos ver nao ha o decamimento exponencial para 0
plot(PACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0) #aqui vemos que a os valores ficam abaixo de 0

#MODELO E
ACF = ARMAacf(ar=0, ma=c(-1.8, 0.72), 30)
PACF = ARMAacf(ar=0, ma=c(-1.8, 0.72), 30, pacf=TRUE) 
par(mfrow=c(2,1)) 
plot(ACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0) #a partir do lag 2, o valor eh 0
plot(PACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0) #aqui podemos ver o crescimento exponencial para 0