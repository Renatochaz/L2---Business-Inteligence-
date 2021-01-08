main_forecast <- main
count_TSObject2 = ts(main_forecast[,c('Faturamento_Pedido')])
main_forecast$clean_count = tsclean(count_TSObject2)

ggplot() +
  geom_line(data = main_forecast, aes(x = Dia, y = clean_count)) + ylab('cleaned count')


ggplot() +
  geom_line(data = main_forecast, aes(x=Dia, y = Faturamento_Pedido)) + ylab('Normal count')

main_forecast$cnt_ma = ma(main_forecast$Faturamento_Pedido, order = 7)
main_forecast$cnt_ma30 = ma(main_forecast$Faturamento_Pedido, order = 30)

summary(main_forecast)
main_forecast_2 <- replace(main_forecast, TRUE, lapply(main_forecast, na.aggregate))
summary(main_forecast_2)


# Plot original data
ggplot() +
  geom_line(data = main_forecast, aes(x=Dia, y = Faturamento_Pedido, colour = "Total sales")) +
  geom_line(data = main_forecast, aes(x=Dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = main_forecast, aes(x=Dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

# Plot NA removed data
ggplot() +
  geom_line(data = main_forecast_2, aes(x=Dia, y = Faturamento_Pedido, colour = "Total sales")) +
  geom_line(data = main_forecast_2, aes(x=Dia, y = cnt_ma, colour = "Weekly MA")) +
  geom_line(data = main_forecast_2, aes(x=Dia, y = cnt_ma30, colour = "Monthly MA")) +
  ylab('Total sales count')

#Decomposition of data, 270 observation per unit of time (year)
count_ma = ts(na.omit(main_forecast_2$clean_count), frequency = 24)
decomp = stl(count_ma, "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)



# Is stationary? Yes
adf.test(count_ma, alternative = "stationary")

# ACF? Pretty bad
acf(count_ma, main = '')
pacf(count_ma, main = '')

#DIf 1
count_dl = diff(deseasonal_cnt, differences = 1)
plot(count_dl)
adf.test(count_dl, alternative = "stationary")

# ACF PACF
Acf(count_dl, main = 'ACF For Dif series')
Pacf(count_dl, main = 'PACF for Dif series')

#
clean_ts = ts(main_forecast_2$Faturamento_Pedido)

#Autofit?
auto.arima(clean_ts, seasonal = FALSE)

fit <- auto.arima(clean_ts, seasonal = TRUE)
tsdisplay(residuals(fit), lag.max = 40, main = '(0,0,3) Model residuals')
accuracy(fit)
checkresiduals(fit)

fit2 <- arima(clean_ts, order=c(7,1,1), seasonal = c(4,0,6))
tsdisplay(residuals(fit2), lag.max = 40, main = 'Seasonal Model residuals')
accuracy(fit2)
checkresiduals(fit2)

hold_ts <- window(ts(clean_ts), start = 200)
fit_no_holdout_ts = arima(ts(clean_ts[-c(200:309)]), order = c(7,1,1), seasonal = c(4,0,6))
fcast_no_holdout_ts <- forecast(fit_no_holdout_ts, h=12)
plot(fcast_no_holdout_ts, main = "")
lines(ts(clean_ts))

table_predict <- rename(data.frame(as.numeric(fcast_no_holdout_ts$mean)), predicted = 1)
table_predict$Dia <- main_forecast_2$Dia[200:211]
#table_predict_2$ma <- count_ma[500:511]
table_predict$Faturamento <- main_forecast_2$Faturamento_Pedido[200:211]
#table_predict_2 <- table_predict_2[c(2,4,3,1)]
table_predict_2$erro <- table_predict_2$vendas - table_predict_2$predicted
table_predict_2$precisao <- 100 - (abs(table_predict_2$erro / table_predict_2$vendas * 100))


