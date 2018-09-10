# Problem 1
dT = read.csv("Demand-Total-Month.csv")[-1]
dT
pT = dT$AvgPrice
pT

par(mfrow = c(1,1))
plot(pT, dT$Dtotal)

price_demand = lm(dT$Dtotal[61:120]~pT[61:120])
summary(price_demand)

# ?????
dTotalTS <- ts(log(dT$Dtotal[1:131]), start=2001, frequency=12)
dTotalTS
plot(dTotalTS)
par(mfrow = c(1,2))
acf(dTotalTS, main='ACF')
pacf(dTotalTS, main='PACF')


pT = ts(pT, start=2001, frequency=12)

par(mfrow = c(1,2))
acf(pT, main='ACF')
pacf(pT, main='PACF')

model_price = auto.arima(pT)
model_price = arima(pT, order = c(3,1,0), seasonal = c(2,0,1))
summary(model_price)
checkresiduals(model_price)
fPrice = forecast(model_price, h=13, level=c(80,95))
autoplot(fPrice)

price_predict = data.frame(fPrice$mean)

model1 = auto.arima(dTotalTS, xreg=pT[1:131])
#model1 = arima(dTotalTS, order = c(0,1,1), seasonal = c(2,0,0), xreg=pT[1:131])
summary(model1)
checkresiduals(model1)

fDemand = forecast(model1, h=13, level=c(80,95), xreg = price_predict)
fDemand
autoplot(fDemand)