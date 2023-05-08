#####################################################  Lectura de datos ##############################################
'''
En este punto tenemos como objetivo construir la mdt(master data table) que se utilizara
en los primeros ejercicios del curso.

Algunas definiciones:

Acciones S&P 500 

*¿Que periodo utilizar para train? 80% 

*¿Que periodo utilizar para test? 20%  

*¿Que periodo vamos a pronosticar? 7 dias   

*Rezago (5 dias - 10 dias) 

*Ene 01 2022 - Dic 30 2022 Train (12 meses) (360 dias) 

*Ene 01 2023 - Abril 30 2023 Test (4 meses) (120 dias) 


Primeras 10 Empresas segun el peso  

AAPL, MSFT, AMZN,NVDA, GOOGL, BRK.B,GOOG, META,UNH,XOM 



*Indice Forex EUR/USD 

Manejar el mismo periodo de tiempo (Diario) 
'''

library(tidyquant)

companias = c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL",'BRK.B','GOOG', 'META','UNH','XOM' )

precios <- tq_get(companias,
                 from = "2022-01-01",
                 to = "2023-04-30",
                 get = "stock.prices")

head(prices)



