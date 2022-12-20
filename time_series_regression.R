SMOdat <- SMOdat

head(SMOdat)
tail(SMOdat)

sulfur_hexafluoride_value <- ts(SMOdat[,4], start=1997, freq=12)
logshv <- log(sulfur_hexafluoride_value)

plot(stl(logshv, s.window="periodic"))

t <- seq(1997, 2021, length=length(sulfur_hexafluoride_value))
tts <- ts(SMOdat)
t2 <- t^2
sin.t <- sin(2*pi*t)
cos.t <- cos(2*pi*t)
sin.t2 <- (sin(2*pi*t))/2
cos.t2 <- (cos(2*pi*t))/2

plot(sulfur_hexafluoride_value, main='Model 1')
model1 <- lm(sulfur_hexafluoride_value ~ (tts - t) + cos.t + sin.t)
lines(t, model1$fit, col=2, lwd=1)

plot(sulfur_hexafluoride_value, main='Model 2')
model2 <- lm(sulfur_hexafluoride_value ~ t + t2 + sin.t + cos.t)
lines(t, model2$fit, col=4, lwd=1)

time(sulfur_hexafluoride_value)
cycle(sulfur_hexafluoride_value)

summary(model1)
summary(model2)
plot(sulfur_hexafluoride_value, ylab="Sulfur Hexafluoride", main="Sulfur Hexafluoride in American Samoa")


res1 <- ts(model1$residuals, start=1997, frequency =12)
plot(res1)
acf(res1)

res2 <- ts(model2$residuals, start=1997, frequency =12)
plot(res2)
acf(res2)

install.packages('Metrics')
library(Metrics)

predict1 = predict(model1, newdata=sulfur_hexafluoride_value, interval='prediction')
predict1

pred_list1 = c(10.39,10.43,10.46,10.46,10.46,10.46,10.49,10.54,
               10.57,10.59,10.63,10.70)
pred_list1
actual_list1 = c(10.39,10.43,10.46,10.46,10.46,10.46,10/49,10.54,10.57,10.59,10.63,10.7)
actual_list1
mae(pred_list1,actual_list1)
mse(pred_list1,actual_list1)
mape(pred_list1,actual_list1)

predict2 = predict(model2, newdata=sulfur_hexafluoride_value, interval='prediction')
predict2

pred_list2 = c(10.378441 ,10.416990,10.453313,10.486022,10.514675 ,10.539886 ,10.563170,
               10.586555,10.612054,10.641151,10.674421,10.711376)
pred_list2
actual_list2 = c(10.39,10.43,10.46,10.46,10.46,10.46,10/49,10.54,10.57,10.59,10.63,10.7)
actual_list2
mae(pred_list2,actual_list2)
mse(pred_list2,actual_list2)
mape(pred_list2,actual_list2)

Box.test(res1, lag = 1, type = "Ljung")
Box.test(res2, lag = 1, type = "Ljung")




