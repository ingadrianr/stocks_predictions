companias = c("AAPL", "MSFT", "AMZN","NVDA", "GOOGL","BRK-B","GOOG", "META","UNH","XOM")
precios = tidyquant::tq_get(companias, from = "2022-01-01", to = "2023-04-30", get = "stock.prices")
stock = precios[precios$symbol == "AAPL",'close']
stock_ts = stats::as.ts(stock, F)

stock_normalized = scale(stock_ts)
plot(stock_normalized, main = "Índice AAPL Normalizado", xlab = "Dia", ylab = "Precio cierre")

y = zoo::as.zoo(stock_normalized)

x1 = stats::lag(y, k=1)
x2 = stats::lag(y, k=2)
x3 = stats::lag(y, k=3)
x4 = stats::lag(y, k=4)
x5 = stats::lag(y, k=5)
x6 = stats::lag(y, k=6)
x7 = stats::lag(y, k=7)
x8 = stats::lag(y, k=8)
x9 = stats::lag(y, k=9)
x10 = stats::lag(y, k=10)

data = cbind(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
clean_data = na.omit(data)

inputs = clean_data[,2:11]
outputs = clean_data[,1]

fit = RSNNS::elman(inputs[0:round(nrow(clean_data)*0.7, digits = 0), ], 
                   outputs[0:round(nrow(clean_data)*0.7, digits = 0), ], 
                   size=c(9,2), learnFuncParams=c(0.1), maxit=10000)

RSNNS::plotIterativeError(fit,main = "Iterative Error for 9,2 Neuron elman Model")

predictions = stats::predict(fit, inputs[round(nrow(clean_data)*0.7, digits = 0):nrow(clean_data), ])
my_list = matrix(NA, nrow = round(nrow(clean_data)*0.7, digits = 0)-1, ncol = 1)
pred = rbind(my_list, predictions)

plot(y, type = "l", main = "Prediccion RNN Elman AAPL", xlab = "Dia", ylab = "Precio cierre", col = "black")
lines(pred, col = "red")
