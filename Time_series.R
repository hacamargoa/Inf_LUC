library(lubridate)
library(forecast)
library(tseries)

AllvarLUH1W<-ts(Global_data[[10]][-1,2],start=decimal_date(ymd("1961-01-01")),frequency =1)
plot(AllvarLUH1W)
fit<-auto.arima(AllvarLUH1W)
plot(fit$residuals,type="b")
plot(forecast(fit, 5))
#fit<-tbats(AllvarLUH1W)
plot(fit)
forecast(fit, 5)
plot(forecast(fit, 5))
test<-Global_data[[2]][,2]
pacf (test, lag = length(test) - 1, pl = TRUE)
acf (test, lag = length(test) - 1, pl = TRUE)

reg_<-lm(WYield~Year,data=Global_data[[2]])
summary(reg_)
diff<-Global_data[[2]][c(20:50),2]-Global_data[[2]][c(1:31),2]
plot(reg_$residuals~Global_data[[2]]$Year,type="b")
test<-reg_$residuals
pacf (test, lag = length(test) - 1, pl = TRUE)
acf (test, lag = length(test) - 1, pl = TRUE)
#seasonality test
diff<-test[c(20:50)]-test[1:31]
pacf (diff, lag = length(test) - 1, pl = TRUE)
acf (diff, lag = length(test) - 1, pl = TRUE)
diff<-Global_data[[2]][c(2:50),2]-Global_data[[2]][c(1:49),2]
diff_ts<-ts(diff,start=decimal_date(ymd("1962-01-01")),frequency =1)
fit<-auto.arima(diff)
class(fit)
plot(forecast(fit))
plot(diff,type="b")
var(diff)
