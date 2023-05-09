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

companias = c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL","BRK-B","GOOG", "META","UNH","XOM")

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
# stock_apple<- subset(precios, symbol == "AAPL")
# plot(decompose(stock_apple$close))

library(timsac)
decomp(stock_apple$close, trade=TRUE)


### Promedio movil

library(zoo)


# Calcular el promedio móvil con ventana de tamaño 5
ma <- rollmean(stock_apple$close, k = 5, align = "right")

# Graficar la serie de tiempo y el promedio móvil
plot(stock_apple$close, type = "l", main = "Promedio Móvil de una Serie de Tiempo")
lines(ma, col = "red")
legend("topright", legend = c("Serie de Tiempo", "Promedio Móvil"),
       col = c("black", "red"), lty = 1, cex = 0.8)