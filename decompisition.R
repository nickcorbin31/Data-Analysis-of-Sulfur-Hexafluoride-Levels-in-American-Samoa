SMO.ts <- ts(SMOdat, start = c(1997, 12), end = c(2021, 12), freq=12)
SMO.2000 <- window(SMO.ts, start = c(2000,1), end = c(2021,12))
fit.2000 <- stl(SMO.2000, s.window = "periodic")
plot(fit.2000)

SMO <- read.table("F:/Jason/Downloads/Book1.xlsx")
SMO.ts <- ts(SMO, start = c(1997, 12), end = c(2021, 12), freq=12)
plot(decompose(SMO.ts))

plot(stl(SMO.ts, s.window="periodic"))

t <- seq(1997, 2021, length=length(SMO.ts))
fit <- lm(SMO.ts ~ t)
coeff <- summary(fit)$coef
summary(fit)

plot(SMO.ts)
abline(reg=fit, col=2, lwd=2)

t2 <- t^2
fit1 <- lm(SMO.ts ~ t + t2)
coeff <- summary(fit1)$coef
summary(fit1)

plot(SMO.ts)
lines(t, coeff[1] + coeff[2]*t, lwd=2, col="red")
lines(t, coeff1[1] + coeff1[2]*t + coeff1[3]*t2, lwd=3, col="blue")

SMO.ts <- ts(SMO, start = c(1997, 12), end = c(2021, 12), freq=12)
SMO.5 <- window(SMO.ts, start = c(1997,1), end = c(2002,12))
fit.5 <- stl(SMO.5, s.window = "periodic")
plot(fit.5)

SMO.ts5 <- ts(SMO, start = c(1997, 12), end = c(2002, 12), freq=12)
plot(decompose(SMO.ts5))

plot(stl(SMO.ts5, s.window="periodic"))

t <- seq(1997, 2002, length=length(SMO.ts5))
fit <- lm(SMO.ts5 ~ t)
coeff <- summary(fit)$coef
summary(fit)

plot(SMO.ts5)
abline(reg=fit, col=2, lwd=2)

t2 <- t^2
fit1 <- lm(SMO.ts5 ~ t + t2)
coeff <- summary(fit1)$coef
summary(fit1)

plot(SMO.ts5)
lines(t, coeff[1] + coeff[2]*t, lwd=2, col="red")
lines(t, coeff1[1] + coeff1[2]*t + coeff1[3]*t2, lwd=3, col="blue")