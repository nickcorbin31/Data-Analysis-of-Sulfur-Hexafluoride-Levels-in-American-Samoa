library('forecast')
library('Metrics')
library('graphics')
dat=SMOdat
dat
dats <- ts(dat[ , 4], start=c(1996, 25), frequency=12)
dats
trendpattern=filter(dats,filter=c(1/8, 1/4, 1/4, 1/4, 1/8),sides=2)
plot(dats,ylab='Sulfur Hexafluoride Level',main='SMO Sulfur Hexafluoride Moving average annual trend')
lines(trendpattern,lwd=2,col='red')
datsforecast=HoltWinters(dats,beta=FALSE,gamma=FALSE)
datsforecast
head(datsforecast)
plot(datsforecast,lwd=3,main='Simple Exponential Smoothing for 1997-2021 in American Samoa',xlab='Time in years (data from every month)'
     ,ylab='Sulfur Hexafluoride level in picomol/mol')
accuracy(datsforecast$x,dats)
forecastHW48=forecast(datsforecast,h=48)
forecastHW48
plot(forecastHW48,main='Simple Exponential Smoothing Forecast for next 48 months in American Samoa',xlab='Time in years (data from every month)'
     ,ylab='Sulfur Hexafluoride level in picomol/mol')
datsresid48=ts(forecastHW48$residuals,start=c(1996))
resid48=datsresid48[-(1)]
acf(resid48,lag.max=20,main='Correlogram for residuals of Simple Exponential Smoothing Forecast for next 48 months in American Samoa')
Box.test(resid48,lag=20,type='Ljung-Box')

holtforecast=HoltWinters(dats,gamma=FALSE)
holtforecast
plot(holtforecast,lwd=3,main="Holt's Exponential Smoothing for 1997-2021 in American Samoa",xlab='Time in years (data from every month)',ylab='Sulfur Hexafluoride level in picomol/mol')
holt48=forecast(holtforecast,h=48)
holt48
plot(holt48,main="Holt's Exponential Smoothing Forecast for next 48 months in American Samoa",xlab='Time in years (data from every month)'
     ,ylab='Sulfur Hexafluoride level in picomol/mol')
holt48resid=ts(holt48$residuals,start=c(1996))
residholt48=holt48resid[-(1:2)]
acf(residholt48,main="Correlogram for residuals of Holt's Exponential Smoothing Forecast for next 48 months in American Samoa")
Box.test(residholt48,lag=20,type='Ljung-Box')

fullholt=HoltWinters(dats)
fullholt
plot(fullholt,lwd=3,main="Holt-Winter's Exponential Smoothing for 1997-2021 in American Samoa",xlab='Time in years (data from every month)',ylab='Sulfur Hexafluoride level in picomol/mol')
fullholtforecast=forecast(fullholt,h=48)
fullholtforecast
plot(fullholtforecast,main="Holt-Winter's Exponential Smoothing Forecast for next 48 months in American Samoa",xlab='Time in years (data from every month)'
     ,ylab='Sulfur Hexafluoride level in picomol/mol')
fullholtresid=ts(fullholtforecast$residuals,start=c(1996))
fhr=fullholtresid[-(1:12)]
acf(fhr,lag.max=20,main="Correlogram for residuals of Holt-Winter's Exponential Smoothing Forecast for next 48 months in American Samoa")
Box.test(fhr,lag=20,type='Ljung-Box')
logfullholt=log(dats)
plot(logfullholt)
logfhforecast=HoltWinters(logfullholt)
logfhforecast
plot(logfhforecast)
logfhforecast48=forecast(logfhforecast,h=48)
logfhforecast48
plot(logfhforecast48)
logfhresid=ts(logfhforecast48$residuals,start=c(1996))
lfhresid=logfhresid[-(1:12)]
acf(lfhresid,lag.max=20)
Box.test(lfhresid,lag=20,type='Ljung-Box')
