library(tseries)
library(forecast)
library(rugarch)
library(tidyverse)
library(caret)
library(arfima)
#загрузка и обработка данных
df0 <- read.csv('EURUSD.csv',sep=';')
df <- subset(df0,select = -c(X.TICKER.,X.PER.,X.TIME.))
df <- transform(df, X.DATE. = as.Date(as.character(X.DATE.), "%Y%m%d"))
nm1 <- setdiff(names(df), "X.DATE.")
nm2 <- setNames(as.list(rep(0, length(nm1))), nm1)
df <- df %>%
  complete(X.DATE. = seq(X.DATE.[1], X.DATE.[length(X.DATE.)],
                         by = "1 day"), fill = nm2)
for(i in (1:length(df$X.CLOSE.))){
  if (df$X.CLOSE.[i] == 0){
    df$X.CLOSE.[i] = (df$X.CLOSE.[i-1] + df$X.CLOSE.[i+1])/2
  }
}
#разделение данных на тестовые и обучающие
train_data <- df$X.CLOSE.[1:(length(df$X.CLOSE.)-7)]
test_data <- diff(df$X.CLOSE.)[(length(df$X.CLOSE.) - 7):length(df$X.CLOSE.)]
test_data <- test_data[-length(test_data)]
plot(x=df$X.DATE., y=df$X.CLOSE., type='l',
     main='Курс EUR/USD', xlab='Дата', ylab='Курс')
#обработка обучающих данных
train_data = ts(train_data)
train_data <- diff(train_data)
Pacf(train_data)
Acf(train_data)
plot(train_data)
#проверка ряда на стационарность
adf.test(train_data, alternative = "stationary")
#FARIMA
arfima_fit <- arfima(train_data, order= c(3,1,3))
summary(arfima_fit)
arfima_forecast <- predict(arfima_fit, n.ahead = 7)
plot(arfima_forecast)
#GARCH
mod_specify = ugarchspec(
  variance.model = list(garchOrder = c(1, 1)), 
  mean.model = list(armaOrder = c(7,7)),
  distribution.model = "norm")
mod_fitting = ugarchfit (data = train_data, spec = mod_specify)
garch_forecast <- ugarchforecast(fitORspec = mod_fitting, n.ahead = 7)
plot(garch_forecast, which = 1)
#запись прогнозов
preds <- data.frame(arfima_preds=arfima_forecast[[1]][["Forecast"]],
                   garch_preds=c(garch_forecast@forecast[["seriesFor"]]))
#оценки моделей
arfima_evaluation <- data.frame(R_squared = R2(preds$arfima_preds, test_data),
                               RMSE = RMSE(preds$arfima_preds, test_data),
                               MAE = MAE(preds$arfima_preds, test_data))
garch_evaluation <- data.frame(R_squared = R2(preds$garch_preds, test_data),
                    RMSE = RMSE(preds$garch_preds, test_data),
                    MAE = MAE(preds$garch_preds, test_data))

arfima_evaluation
garch_evaluation
#F-тесты
var.test(preds$arfima_preds,test_data)
var.test(preds$garch_preds,test_data)

#запись прогнозов в отдельный файл
write.csv2(data.frame("DATE"=df$X.DATE.[(length(df$X.DATE.)-6):length(df$X.DATE.)],
                     "FARIMA prediction"=as.numeric(preds$arfima_preds),
                     "GARCH prediction"=preds$garch_preds),
           'predictions.csv',row.names = F)

