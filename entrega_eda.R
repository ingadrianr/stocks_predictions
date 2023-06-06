#####################################################  Lectura de datos ##############################################

# En este punto tenemos como objetivo construir la mdt(master data table) que se utilizara
# en los primeros ejercicios del curso.
# 
# Algunas definiciones:
# 
# Acciones S&P 500 
# 
# *¿Que periodo utilizar para train? 80% 
# 
# *¿Que periodo utilizar para test? 20%  
# 
# *¿Que periodo vamos a pronosticar? 7 dias   
# 
# *Rezago (5 dias - 10 dias) 
# 
# *Ene 01 2022 - Dic 30 2022 Train (12 meses) (360 dias) 
# 
# *Ene 01 2023 - Abril 30 2023 Test (4 meses) (120 dias) 
# 
# 
# Primeras 10 Empresas segun el peso  
# 
# AAPL, MSFT, AMZN,NVDA, GOOGL, BRK.B,GOOG, META,UNH,XOM 
# 
# 
# 
# *Indice Forex EUR/USD 
# 
# Manejar el mismo periodo de tiempo (Diario) 
# 
#install.packages("tidyquant")
library(tidyquant)
library(tidyverse)
library(ggplot2)
library(scales)
library(tseries)

companias = c("AAPL", "MSFT", "AMZN")
              #"NVDA", "GOOGL","BRK-B","GOOG", "META","UNH","XOM")

precios <- tq_get(companias,
                 from = "2022-01-01",
                 to = "2023-04-30",
                 get = "stock.prices")

### Encabezado de la tabla

head(precios)

### Se observan los primeros 9 asociados a la primera fecha encontrada en 2022 es decir enero 03/2022
precios %>%
  group_by(symbol) %>%
  slice(1)

precios %>%
  ggplot(aes(x = date, y = close, color = symbol)) +
  geom_line() +
  labs(title = 'Stocks Close Prices from 2022-01-01 to 2023-04-30', y = 'Close Price')

precios %>%
  ggplot(aes(x = date, y = close, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Fecha',
       y = "Precio de Cierre",
       title = "Precio de Cierre por accion") +
  scale_x_date(date_breaks = "month",
               labels=date_format("%Y-%m-%d")) +theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Creo que el eje X no aporta mucho en el análisis para esta gráfica, solo aparecían datos para un par de ellas. 
#element_text(angle = 20, hjust = 1)

### Test ADF - Dickey Fuller

#https://support.minitab.com/es-mx/minitab/21/help-and-how-to/statistical-modeling/time-series/how-to/augmented-dickey-fuller-test/interpret-the-results/key-results/
# Hipótesis nula: La serie de tiempo tiene una raíz unitaria. La serie de tiempo es no estacionaria.
# Hipótesis alternativa: La serie de tiempo no tiene raíces unitarias. La serie de tiempo es estacionaria.

precios_df=as.data.frame(precios)
names(precios)

for (column in unique(precios$symbol)) {
  stock_data <- ts(precios[precios$symbol == column, "close"])
  result <- adf.test(stock_data)
  cat(paste("Acción: ", column, "\n"))
  cat(paste("ADF Estadística: ", result$statistic, "\n"))
  cat(paste("Valor p: ", result$p.value, "\n"))
  for (key in names(result$critical)) {
    cat(paste("   ", key, ": ", result$critical[key], "\n"))
  }
  cat("-----------------------\n")
}

### 
# 
# Cuando la conclusión de la prueba admita la diferenciación, examine las gráficas de los datos diferenciados en busca de características de los datos que no son estacionarios. Una tendencia en la gráfica de series temporales es un ejemplo de un patrón que indica que la media de los datos no es estacionaria. En el gráfico ACF, los picos grandes que disminuyen lentamente también indican datos que no son estacionarios. Si ve estos patrones en los datos diferenciados, considere si debe ajustarse a un modelo ARIMA con un segundo orden de diferenciación. Por lo general, 1 o 2 órdenes de diferenciación son suficientes para proporcionar un ajuste razonable a los datos.
# 
# Si los datos diferenciados son estacionarios, entonces un enfoque razonable es incluir un solo orden de diferenciación no estacional en un modelo ARIMA. Para obtener más información sobre los modelos ARIMA, vaya a Revisión general de ARIMA.
# 
# Las diagramas de series temporales muestran el resultado de la diferenciación. En estos resultados, la gráfica de series temporales de los datos originales muestra una tendencia clara. La gráfica de series temporales de los datos diferenciados muestra las diferencias entre valores consecutivos. Los datos diferenciados parecen estacionarios porque los puntos siguen un camino horizontal sin patrones obvios en la variación.
# 
# Las gráficas de ACF también muestran el efecto de la diferenciación. En estos resultados, la gráfica ACF de los datos originales muestra picos disminuidos lentamente a través de los retrasos. Este patrón indica que los datos no son estacionarios. En la gráfica ACF de los datos diferenciados, el único pico que es significativamente diferente de 0 es en el retraso 1.


### Graficas de funcion de autocorrelacion y de funcion de autocorrelacion parcial 


library(stats)

for (i in unique(precios$symbol)) {
  stock_i <- subset(precios, symbol == i)
  stock_ts <- ts(stock_i$close)
  
  stock_acf <- acf(stock_ts)
  stock_pacf <- pacf(stock_ts)
  
  par(mfrow=c(2,1))
  
  plot(stock_acf, main=paste("ACF para la acción", i))
  plot(stock_pacf, main=paste("PACF para la acción", i))

  
}

### Cuanto debemos diferenciar?
library(forecast)

for (column in unique(precios$symbol)) {
  stock_data <- ts(precios[precios$symbol == column, "close"])
  cat(paste("Acción: ", column, "\n"))
  cat(paste("Cuantas diferencias usar: ", ndiffs(stock_data, test = "adf", alpha = 0.05), "\n"))
  cat("-----------------------\n")
}

### Diferenciando las series

library(dplyr)

precios <- precios %>% 
  group_by(symbol) %>% 
  mutate(diferenciado1 = c(NA, diff(close, differences = 1, lag = 1)[-length(close)]))

### Descomposición de las series
### https://rstudio-pubs-static.s3.amazonaws.com/448195_560955e5b25b4e8587751f6c8789e8b3.html

#plot(decompose(co2))
  
library(tidyverse)
library(lubridate)


# precios$date <- ymd(precios$date)
#stock_apple<- subset(precios, symbol == "AAPL")
stock_apple <- ts(precios[precios$symbol == "AAPL", "close"])
# plot(decompose(stock_apple$close))
windows()
library(timsac)

windows()
fechas<- seq(from=as.Date("2022-01-01"), to=as.Date("2023-04-30"), by = "day")
#stock_ts <- ts(precios[precios$symbol == "AAPL","close"],start=1, end=332, frequency=5)
stock_ts <- ts(precios[precios$symbol == "AAPL","close"],start="2022-01-01", end="2023-04-30", frequency=365)
stock_ts <- na.approx(stock_ts)
stock_ts <- na.omit(stock_ts)
#appl_stl <- stl(x = stock_ts, s.window = "periodic")

decomp = decompose(stock_ts, 'additive')
plot(decomp)


window()
stock <- precios[precios$symbol == "AAPL",]
#stock_ts <- zoo(stock$close, stock$date)
windows()
stock_ts <- ts(precios[precios$symbol == "AAPL","close"], frequency=2)
decomposition <- decompose(stock_ts, type = "additive")
plot(decomposition)












#stock_ts <- as.ts(stock_ts,start=c(2022, 1), end=c(2023, 4), frequency=12)
stock_ts_interp <- na.approx(stock_ts)
appl_stl <- stl(x = stock_ts_interp, s.window = "periodic")



decomposition <- decompose(stock_ts, type = "additive")








### Promedio movil

library(zoo)


# Calcular el promedio móvil con ventana de tamaño 5
ma <- rollmean(stock_apple$close, k = 5, align = "right")

# Graficar la serie de tiempo y el promedio móvil
windows()

plot(stock_apple$close, type = "l", main = "Promedio Móvil de una Serie de Tiempo")
lines(ma, col = "red")
legend("topright", legend = c("Serie de Tiempo", "Promedio Móvil"),
       col = c("black", "red"), lty = 1, cex = 0.8)


##########################################################################################
###################### Orden en el que vamos a construir el entregable ###################
##########################################################################################

##### Promedio movil (hablar de que se trata, para que sirve y coger dos acciones una que depronto tenga mas picos que otra y comparar)


windows()
par(mfrow=c(4,3))
for (i in unique(precios$symbol)){
  stock_data <- ts(precios[precios$symbol == i, "close"])
  cat(paste("Acción: ", i, "\n"))
  
  plot(stock_data, type = "l", main = paste("Promedio Móvil con n=5 para ",i))
  ma <- rollmean(stock_data, k = 5, align = "right")
  lines(ma, col = "red")
}
legend("topright", legend = c("Serie de Tiempo", "Promedio Móvil"),col = c("black", "red"), lty = 1, cex = 0.8)  

##### Descomposicion de la serie (aditiva)
#hablar para que sirve, escribir la forma t+s+r

windows()
#par(mfrow=c(4,3))
for (i in unique(precios$symbol)){
  stock_data <- ts(precios[precios$symbol == i, "close"], frequency=5)
  #decomposition <- decompose(stock_data, type = "additive")
  #plot(decomposition, main = paste("Descomposicion para ",i))
  par(mfrow=c(10,1))  # Establece la distribución de sub-gráficos
  plot(decomposition, main = paste("Descomposicion para ",i))

}

stock <- precios[precios$symbol == "AAPL",]
#stock_ts <- zoo(stock$close, stock$date)
windows()
stock_ts <- ts(precios[precios$symbol == "AAPL","close"], frequency=2)
decomposition <- decompose(stock_ts, type = "additive")
plot(decomposition)


#queda pendiente 

#### Unidad 3: Estacionaridad y Diferenciacion

#Test ADF - Dickey Fuller

for (column in unique(precios$symbol)) {
  stock_data <- ts(precios[precios$symbol == column, "close"])
  result <- adf.test(stock_data)
  cat(paste("Acción: ", column, "\n"))
  cat(paste("ADF Estadística: ", result$statistic, "\n"))
  cat(paste("Valor p: ", result$p.value, "\n"))
  for (key in names(result$critical)) {
    cat(paste("   ", key, ": ", result$critical[key], "\n"))
  }
  cat("-----------------------\n")
}


### Dar un contexto de la prueba, escribir las hipotesis, por cada accion definir si es o no estacionaria.
# sobre las que no son estacionarias, debemos evaluar alternativas como la diferenciacion,(si preguntamos
# de donde se justifica )

### FAC Y PACF 
#hablar para que sirven que normalmente sirven para definir los ordenes del modelo autoregresivo (AR)
## y del modelo de media movil (MA)... Interpretar solo en una acción.


### Diferenciando las series
library(dplyr)

precios <- precios %>% 
  group_by(symbol) %>% 
  mutate(diferenciado1 = c(NA, diff(close, differences = 1)[-length(close)]))

#Test ADF - Dickey Fuller despues de la diferenciacion

symbols_no_estacionarios <- setdiff(unique(precios$symbol), "XOM")

for (column in symbols_no_estacionarios) {
  stock_data <- precios[precios$symbol == column, ]
  stock_data_omit_na <- na.omit(stock_data$diferenciado1)
  result <- adf.test(stock_data_omit_na)
  cat(paste("Acción: ", column, "\n"))
  cat(paste("ADF Estadística: ", result$statistic, "\n"))
  cat(paste("Valor p: ", result$p.value, "\n"))
  for (key in names(result$critical)) {
    cat(paste("   ", key, ": ", result$critical[key], "\n"))
  }
  cat("-----------------------\n")
}


#####################################################################################################
####################################### Suavizacion #################################################
#####################################################################################################

library(forecast)

### Suavizacion exponencial Simple



stock_apple <- precios[precios$symbol == "AAPL",]
stock_ts <- ts(stock_apple$close, frequency=5)

fit_apple <- HoltWinters(stock_ts, beta = FALSE, gamma = FALSE)
alpha_apple <- fit_apple$alpha


predictions <- forecast(fit_apple, h = 30)

mean(abs(predictions$mean - stock_ts[length(stock_ts)]))

plot(fit_apple, main = "Suavizamiento Exponencial Simple Apple")
lines(predictions$mean, col = "blue")
legend("topleft", legend = c("Datos", "Ajustado", "Pronostico"), col = c("black", "red", "blue"), lty = c(1,1,1))

### Método Holt-Winters

stock_ts <- ts(stock_apple$close, frequency=5)



fit_apple_hw <- HoltWinters(stock_ts, 
                            seasonal = "additive",
 #                           start.periods = 5,
                            alpha = TRUE,
                            beta = TRUE,
                            gamma = TRUE)


alpha_hw <- fit_apple_hw$alpha
beta_hw <- fit_apple_hw$beta
gamma_hw <- fit_apple_hw$gamma


summary(fit_apple_hw)$alpha


predictions_hw <- forecast(fit_apple_hw, h = 30)

mae_hw <- mean(abs(fit_apple_hw$fitted - stock_ts))

mae <- mean(abs(fit_apple$fitted - stock_ts))


plot(fit_apple_hw, main = "Suavizamiento Holt-Winters")
lines(predictions_hw$mean, col = "blue")
legend("topleft", legend = c("Datos", "Ajustado", "Pronostico"), col = c("black", "red", "blue"), lty = c(1,1,1))





#############################

### ciclos 

windows()
#par(mfrow=c(4,3))
legend_labels <- NULL
for (i in unique(precios$symbol)){
  stock <- precios[precios$symbol == i,]
  stock_ts <- ts(stock$close, frequency=5)
  cat(paste("Acción: ", i, "\n"))
  
  fit_se <- HoltWinters(stock_ts, beta = FALSE, gamma = FALSE)
  alpha <- fit_se$alpha
  cat(paste("Valor Alfa Ajustado: ", alpha, "\n"))
  mae <- mean(abs(fit_se$fitted - stock_ts))
  cat(paste("Mean Absolute Error (MAE): ", mae, "\n"))
  predictions <- forecast(fit_se, h = 30)
  
  
  plot(fit_se, main = paste("Suavizamiento Exponencial Simple para",i))
  lines(predictions$mean, col = "blue")
  legend("topleft", legend = c("Datos", "Ajustado", "Pronostico"), col = c("black", "red", "blue"), lty = 1 ,cex = 0.5)
}


### ciclos 

windows()
#par(mfrow=c(4,3))
legend_labels <- NULL
for (i in unique(precios$symbol)){
  stock <- precios[precios$symbol == i,]
  stock_ts <- ts(stock$close, frequency=5)
  cat(paste("Acción: ", i, "\n"))
  
  fit_se <- HoltWinters(stock_ts, seasonal = "additive",
                        alpha = TRUE,
                        beta = TRUE,
                        gamma = TRUE)
  

  cat(paste("Summary Modelo: ", summary(fit_se), "\n"))

  mae <- mean(abs(fit_se$fitted - stock_ts))
  cat(paste("Mean Absolute Error (MAE): ", mae, "\n"))
  predictions <- forecast(fit_se, h = 30)
  
  
  plot(fit_se, main = paste("Suavizamiento Holt-Winters para",i))
  lines(predictions$mean, col = "blue")
  
  legend("topleft", legend = c("Datos", "Ajustado", "Pronostico"), col = c("black", "red", "blue"), lty = 1 ,cex = 0.8)
}

#####################################################################################################
####################################### Box Jenkins #################################################
#####################################################################################################


#Ya sabemos que algunas series (9 de las 10) no son estacionarias. Por ende ya hicimos el ejercicio
#de encontrar cuantas veces nos sugeria diferenciar las series. En todos los casos se realizo una vez.
#Hicimos el calculo del test adf de nuevo y ya nos sugirio que son estacionarias. 

#Ahora vamos a construir modelo arima (caso diferenciacion) y modelo arma (caso en que no hubo necesidad de diferenciar)

# Modelo Arima - Arma


library(forecast)


ts_data <- ts(precios[precios$symbol == 'AAPL', "close"],frequency = 365, start = c(2022, 1), end = c(2023, 4))


# Graficar la serie de tiempo
plot(ts_data, main = "Serie de tiempo del precio de cierre de Apple", xlab = "Fecha", ylab = "Precio de cierre")

# Verificar si la serie de tiempo es estacionaria
adf_test <- adf.test(ts_data,alternative = c("stationary", "explosive"))
print(adf_test)

# Cuantas veces diferenciar?
ndiffs(ts_data)

### Se diferencia 
dif_ts_data<-diff(ts_data)

#la graficamos
plot(dif_ts_data, main=" ", ylab="valor", col="deepskyblue", xlab="Tiempo")
title(main="DIF Precio de Cierre APPLE")

### Se evalua el adf test de nuevo

adf_dif<-adf.test(dif_ts_data)
print(adf_dif)


### Funciones de acf y pacf sobre las series diferenciadas

ACF<-acf(dif_ts_data)
PACF<-pacf(dif_ts_data)

### Creacion del modelo arima en este caso

modelo<-auto.arima(dif_ts_data)
modelo

plot(modelo)
lines(dif_ts_data, col = "blue")

# Realizar pruebas de diagnóstico en los residuos del modelo
checkresiduals(dif_ts_data)

# Realizar pruebas de diagnóstico en los residuos del modelo
checkresiduals(fitted_model)
#Series: dif_ts_data 
#ARIMA(0,0,0) with zero mean 

#sigma^2 = 10.35:  log likelihood = -952.22
#AIC=1906.44   AICc=1906.45   BIC=1910.35


#Ojo con lo anterior, esto parece ser un proceso ruido blanco, y lo que indicaraia es que no hay mucho
#que hacer con los valores pasados paa predecir el futuro.

### identificacion de puntos de cambio en media 

#install.packages("changepoint")
library(changepoint)


mval<-cpt.mean(dif_ts_data,method = "AMOC") 
cpts(mval)

plot(mval, type = "l", cpt.col = "blue", xlab = "Value", cpt.width = 4, main = "default penalty")

### prediccion de la siguiente semana

pred<-forecast(dif_ts_data,h=7)


plot(pred, main=" ", ylab="valor", col="deepskyblue", xlab="Tiempo")
title(main="Predicción DIF Precio cierre APPLE")

#modelo lineal aplicado a la serie de tiempo

lm_model <- lm(Valor ~ tiempo, data = data.frame(tiempo = time(ts_data), Valor = ts_data))

summary(lm_model)


AIC_valor <- AIC(lm_model)
print(AIC_valor)
BIC_valor <- BIC(lm_model)
print(BIC_valor)


# Graficar la serie de tiempo y el modelo ajustado
ggplot(data = data.frame(tiempo = time(ts_data), Valor = ts_data)) +
  geom_line(aes(x = tiempo, y = Valor), color = "blue") +
  geom_line(aes(x = tiempo, y = predict(lm_model)), color = "red") +
  labs(title = "Ajuste de un modelo lineal a una serie de tiempo", x = "Fecha", y = "Valor")

### pronostico (aun no la logro)

#fechas_futuras <- seq(max(precios$date) + 1, length = 7, by = 1)
#pronostico <- predict(lm_model, newdata = data.frame(date = fechas_futuras))

stock_data_omit_na <- na.omit(stock_data$diferenciado1)

######################################################################
################################# Modelo Prophet #####################
######################################################################
'''
Prophet es un procedimiento para pronosticar datos de series temporales
basado en un modelo aditivo en el que las tendencias no lineales 
se ajustan a la estacionalidad anual, semanal y diaria, además 
de los efectos de las vacaciones. Funciona mejor con series 
temporales que tienen fuertes efectos estacionales y varias 
temporadas de datos históricos. Prophet es resistente a los datos faltantes y los cambios en la tendencia, y por lo general maneja bien los valores atípicos.

Este modelo puede ser usado tanto para series univariadas o multivariadas.
Con los modelos de series temporales univariadas, la idea es hacer predicciones
de valores futuros basadas únicamente en las tendencias y la estacionalidad de los 
datos pasados de la variable objetivo (la que tratamos de pronosticar) y nada más.
Los modelos multivariantes son una extensión de eso, donde la entrada puede ser múltiples series de tiempo.

La cuestión es que, con bastante frecuencia, para responder preguntas comerciales, no busca el mejor modelo con una precisión que le permitiría ganar los desafíos del aprendizaje automático, sino solo una estimación lo suficientemente buena que permita una decisión basada en datos.

Aquí es donde entra en juego el Profeta de Facebook. Es un algoritmo listo para usar, fácil de configurar y que brinda resultados decentes con muy poco esfuerzo.
'''

#install.packages("prophet")
library(prophet)


#Detección automática de puntos de cambio en Prophet


# Prophet detecta los puntos de cambio especificando primero 
# una gran cantidad de puntos de cambio potenciales en los que 
# se permite cambiar la velocidad. Luego, pone un escaso 
# adelanto en las magnitudes de los cambios de tasa 
# (equivalente a la regularización L1); 
# esto significa esencialmente que Prophet tiene una gran cantidad 
# de lugares posibles donde la tasa puede cambiar, pero usará 
# la menor cantidad posible.


data <- data.frame(date = stock_apple$date, close = stock_apple$close)

# Renombrar las columnas a "ds" y "y" (requerido por prophet)
colnames(data) <- c("ds", "y")

# Crear un objeto modelo de prophet
model <- prophet(data,  daily.seasonality = TRUE)

# Realizar pronósticos con prophet
future <- make_future_dataframe(model, periods = 10)  # 10 periodos de pronóstico
forecast <- predict(model, future)

library(ggplot2)

plot(model, forecast) +
  labs(title = "Serie APPL mediante PROPHET",
       x = "Fecha",
       y = "Precio Cierre")
# xlab("Fecha")
# ylab("Precio Cierre")


### Grafico de descomposicion

prophet_plot_components(model, forecast)

data <- data.frame(date = precios$date, close = precios$close, symbol=precios$symbol)


windows()
par(mfrow=c(4,3))


for (i in unique(precios$symbol)) {
  stock_i <- subset(data, symbol == i)
  stock_i <- subset(stock_i, select = -symbol)
  colnames(stock_i) <- c("ds", "y")
  
  model <- prophet(stock_i,  daily.seasonality = TRUE)
  
  # Realizar pronósticos con prophet
  future <- make_future_dataframe(model, periods = 10) 
  forecast <- predict(model, future)
  
  plot(model, forecast) +
    labs(title = paste("Modelo Prophet para",i),
         x = "Fecha",
         y = "Precio Cierre")
  
}


### Otra forma de hacerlo

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(fable.prophet)
library(plotly)


stock_data <- ts(precios[precios$symbol == 'AAPL', "close"], frequency=5)
stock_tsibble <- as_tsibble(stock_data)
model <- stock_tsibble %>%
  model(prophet = Prophet(close))

forecast <- forecast(model, h = 10)
autoplot(forecast)





stock_apple=subset(precios, select = c("date","close" ,'symbol'))


stock_apple <- as_tsibble(stock_apple,index = date,key = symbol)

stock_apple %>% 
  autoplot(diferenciado1)

stock_apple <- fill_gaps(stock_apple)


prophet(stock_apple)

fit <- stock_apple %>% 
  model(
    Prophet        = prophet(diferenciado1 ~ growth("linear") + season("week", type = "additive")),
    Prophet_auto = prophet(diferenciado1),
    ARIMA          = ARIMA(diferenciado1)
  )
fit

fit %>% 
  select(Prophet, Prophet_auto) %>%
  components() %>% 
  autoplot()

fc <- fit %>% 
  forecast(h = 7)

fc %>% 
  autoplot(stock_apple)


fc %>% 
  autoplot(stock_apple %>% filter_index("2023-03-01" ~ .), level = NULL)



########################3333

lax_passengers <- read.csv("https://raw.githubusercontent.com/mitchelloharawild/fable.prophet/master/data-raw/lax_passengers.csv")

# Tidy and summarise the data for comparison of international and domestic passenger counts
lax_passengers <- lax_passengers %>%
  mutate(datetime = mdy_hms(ReportPeriod)) %>%
  group_by(month = yearmonth(datetime), type = Domestic_International) %>%
  summarise(passengers = sum(Passenger_Count)) %>%
  ungroup()


lax_passengers <- lax_passengers %>% 
  as_tsibble(index = month, key = type)

lax_passengers %>% 
  autoplot(passengers)


prophet(passengers ~ growth("linear") + season("year", type = "multiplicative"))


fit <- lax_passengers %>% 
  model(
    Prophet        = prophet(passengers ~ growth("linear") + season("year", type = "multiplicative")),
    `Prophet auto` = prophet(passengers),
    ARIMA          = ARIMA(passengers),
    ETS            = ETS(passengers),
    Harmonic       = ARIMA(passengers ~ fourier(K = 3) + PDQ(0,0,0)),
    SNAIVE         = SNAIVE(passengers)
  )
fit