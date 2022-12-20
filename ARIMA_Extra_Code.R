datneed <- SMOdat

fit_yw <- ar(datneed, aic=TRUE, order.max = NULL, method="yule-walker")
fit_yw #Order 1 with sigma^2 = .08142

fit_mle <- ar(datneed, aic = TRUE, order.max = NULL, method="mle")
fit_mle #Order 12 with sigma^2 = .0001731

fit_ols <- ar(datneed, aic = TRUE, order.max = NULL, method="ols")
fit_ols


fit_ml <- arima(datneed, order = c(1, 0, 0), method="ML")
fit_ml

fit_css <- arima(datneed, order = c(1, 0, 0), method="CSS")
fit_css


plot.ts(datneed)

diff1data <- diff(datneed, differences = 1)
plot.ts(diff1data)

diff2data <- diff(datneed, differences = 2)
plot.ts(diff2data)


acf(diff2data, lag.max=20) 
pacf(diff2data, lag.max=20) 
auto.arima(datneed)

cand_arima1 <- arima(datneed, order=c(0,1,5))
cand_arima2 <- arima(datneed, order=c(5,2,2))
cand_arima3 <- arima(diff2data, order=c(5,0,2))

cand_arima1
cand_arima2
cand_arima3 #Candidate 3 appears to be the best model because of its log likelihood and aic
