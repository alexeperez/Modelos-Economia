library(tseries)
library(forecast)

ins <- "FB"
url1 <- "http://real-chart.finance.yahoo.com/table.csv?s="
url2 <- "&a=04&b=1&c=2012&d=11&e=31&f=2015&g=m&ignore=.csv"
url <- paste0(url1, ins, url2)

d <- read.table(url,
                header = TRUE,
                sep=",")
View(d)
obs <- d[nrow(d):1, "Close"]

mes <- seq(as.Date("2012/5/1"),
           as.Date("2015/12/31"),
           by = "month")
serie <- ts(obs, 
            frequency = 12, 
            start=c(2012,5), 
            end=c(2015, 12))
class(serie)
str(serie)

# Diag de Lineas
plot(serie, col="blue",xlab="Mes", ylab=names(data)[2])

# Metodologia BOX-JENKINS
# Serie Original

# FAC Y FACP de serie Original
# Retardos mostrados
retardo_max <- 40

par(mfrow=c(2,1))
pacf(serie,
     lag.max=retardo_max,
     main="Serie",
     xlab="", 
     ylab='FACP',
     col="red")

acf(serie,
    lag.max=retardo_max, 
    xlab="Retardo serie",
    main="",
    ylab='FAC',
    col="red")

# Diferenciacion Estacional
# D = ?

DIFERENCIAR="SI"
D = 1
periodo = 12

D_serie <-serie
if (DIFERENCIAR=="SI"){
  D_serie <-diff(serie, 
                 lag=periodo,
                 differences = D)
}

# FAC Y FACP Serie dif Estacional
par(mfrow=c(2,1))
pacf(D_serie, lag.max=retardo_max,main="D_Serie",
     xlab="", ylab='FACP',col="red")

acf(D_serie,lag.max=retardo_max, xlab="Retardo D_serie",main="",
    ylab='FAC',col="red")

par(mfrow=c(1,1))
plot(D_serie,col="blue",xlab="Trimestre", ylab=names(data)[2])

# Diferenciacion Esatacionaria
# Raices Unitarias: DICKEY FULLER TEST. para:  D_serie
# H0: serie no estacionaria
# H1: serie estacionaria
# p_valor <= 5% se acepta estacionariedad

df <- adf.test(D_serie, alternative = c("stationary"))
df

# ADF en tabla
ADF<-data.frame(df$statistic,df$p.value)
colnames(ADF)<-c("Dickey Fuller","p_valor")
ADF


D_d_serie <- D_serie
if(ADF[1,2] > 0.05)
{
  d=1
  D_d_serie <- diff(D_serie,lag=1,differences=d)
}

# FAC Y FACP Serie dif Estacional y no Estacional
par(mfrow=c(2,1))
pacf(D_d_serie, lag.max=retardo_max,main=paste("d","D",names(data)[2],sep="_"),
     xlab="", ylab='FACP',col="red")

acf(D_d_serie,lag.max=retardo_max, xlab="Retardo",main="",
    ylab='FAC',col="red")


# Ajuste SARIMA

s_arima <-arima(serie, 
                order = c(1, 0, 1),
                seasonal = list(order = c(0,1,0), period=4),
                include.mean = TRUE)


par (mfrow=c(2,1))
pacf(residuals(s_arima), lag.max=retardo_max,main="Modelo Final",
     xlab="", ylab='FACP',col="red")

acf(residuals(s_arima),lag.max=retardo_max, xlab="Retardo",main="",
    ylab='FAC',col="red")

s_arima


# Predicciones o Pronosticos
# Horizonte Prediccion
h=3
prediccion<- predict(s_arima, n.ahead=h)

par(mfrow=c(1,1))
###  INTERVALOS DE CONFIANZA PRONOSTICOS
U <- prediccion$pred + 1.96*prediccion$se
L <- prediccion$pred - 1.96*prediccion$se

ts.plot(serie, prediccion$pred, U, L, col=c("blue",2,3,3), lty = c(1,1,2,2))
legend("topleft",border = "white", c("Real", "Pronostico", "IC (95%)"),col=c("blue",2,3), lty=c(1,1,2))


PRONOSTICOS <- data.frame(prediccion[1],L,U)
colnames(PRONOSTICOS) <- c("PRONOSTICO","LIM INF","LIM SUP")
View(PRONOSTICOS)



