
# Problem 2 
data_Uni = read.csv("Demand-Total-Month-Uni-Combined.csv")[-1]

price_Uni = data_Uni$PRICE[1:131]
price_Uni_ts = ts(price_Uni, start=2001, frequency=12)
plot(price_Uni_ts)
par(mfrow = c(1,2))
acf(price_Uni_ts, main='ACF')
pacf(price_Uni_ts, main='PACF')

model_price_Uni = auto.arima(price_Uni_ts)
#model_price_Uni = arima(pT_Uni, order = c(3,1,0), seasonal = c(2,0,1))
summary(model_price_Uni)
checkresiduals(model_price_Uni)
fPrice_Uni = forecast(model_price_Uni, h=13, level=c(80,95))
autoplot(fPrice_Uni)

price_predict_Uni = data.frame(fPrice_Uni$mean)

####################

plot(data_Uni$PRICE, data_Uni$DEMAND)
plot(log(data_Uni$PRICE), log(data_Uni$DEMAND))

dT_Uni <- ts(data_Uni$DEMAND[1:131], start=c(2001,1), frequency=12)
dT_Uni_log <- ts(log(data_Uni$DEMAND[1:131]), start=c(2001,1), frequency=12)
par(mfrow=c(1,1))
plot(dT_Uni, xlim=c(2006,2010))
plot(diff(dT_Uni))

adf.test(dT_Uni)
adf.test(log(dT_Uni))

par(mfrow = c(1,2))
acf(diff(dT_Uni), main='ACF')
pacf(diff(dT_Uni), main='PACF')

xregs = data_Uni[,c(-1,-4, -6,-7)]
xregs$PRICE = (xregs$PRICE)

model1_Uni = auto.arima(dT_Uni, xreg = xregs[1:131,])
#model1_Uni = arima(dTotalTS_Uni, order=c(0,1,2), seasonal = c(1,0,1))
model1_Uni = arima(dT_Uni, xreg = xregs[1:131,], order=c(4,1,2), seasonal = c(2,1,3))
summary(model1_Uni)
checkresiduals(model1_Uni)

xregsnew = c(mean(data_Uni$FA), mean(data_Uni$FB), mean(data_Uni$PR1))
xregsnew = matrix(rep(xregsnew, 13), 13,3, byrow=TRUE)
xregsnew = data.frame(price_predict_Uni, xregsnew)
xregsnew

fDemand_Uni = forecast(model1_Uni, xreg = xregsnew, h=13, level=c(80,95))
fDemand_Uni
autoplot(fDemand_Uni, xlim = c(2010,2013)) +
  autolayer(fDemand_Uni_partial_ts)

par(mfrow = c(1,2))
plot(fDemand)
plot(fDemand_Uni)

df_price = read.csv("PB_price.csv")
View(df_price)


####################
# Separate demand functions
data1 = data_Uni[1:72,]
data2 = data_Uni[97:131,]

plot(data1$PRICE, data1$DEMAND)
plot(data2$PRICE, data2$DEMAND)
df1 = data.frame(data1$PRICE, data1$DEMAND)
df2 = data.frame(data2$PRICE, data2$DEMAND)

par(mfrow = c(1,1))
plot(df1, pch=19, col="blue", xlim=c(1.3,3))
points(df2, pch=19, col="red")

######################
# Separate demand funtions weekly
data_Uni_week = read.csv("Demand-Total-Uni-Combined.csv")[-1]
View(data_Uni_week)
data1 = data_Uni_week[1:200,]
data2 = data_Uni_week[471:573,]

plot(data1$PRICE, data1$DEMAND)
plot(data2$PRICE, data2$DEMAND)
df1 = data.frame(data1$PRICE, data1$DEMAND)
df2 = data.frame(data2$PRICE, data2$DEMAND)

par(mfrow = c(1,1))
plot(df1, pch=19, col="blue", xlim=c(1.3,3))
points(df2, pch=19, col="red")

###############################
# Problem 2 
data_Uni = read.csv("Demand-Total-Month-Uni-Combined.csv")[-1]

# price_Uni = data_Uni$PRICE[97:131]
# price_Uni_ts = ts(price_Uni, start=2009, frequency=12)
# plot(price_Uni_ts)
# par(mfrow = c(1,2))
# acf(price_Uni_ts, main='ACF')
# pacf(price_Uni_ts, main='PACF')
# 
# #model_price_Uni = auto.arima(price_Uni_ts)
# model_price_Uni = arima(price_Uni_ts, order = c(0,0,1), seasonal = c(0,0,1))
# summary(model_price_Uni)
# checkresiduals(model_price_Uni)
# fPrice_Uni = forecast(model_price_Uni, h=13, level=c(80,95))
# autoplot(fPrice_Uni)
# 
# price_predict_Uni = data.frame(fPrice_Uni$mean)

####################

plot(data_Uni$PRICE, data_Uni$DEMAND)
plot(log(data_Uni$PRICE), log(data_Uni$DEMAND))

dT_Uni <- ts(data_Uni$DEMAND[97:131], start=c(2009,1), frequency=12)
dT_Uni_log <- ts(log(data_Uni$DEMAND[97:131]), start=c(2009,1), frequency=12)
par(mfrow=c(1,1))
plot(dT_Uni, xlim=c(2006,2010))
plot(diff(dT_Uni))

adf.test(dT_Uni)
adf.test(log(dT_Uni))

par(mfrow = c(1,2))
acf(diff(dT_Uni), main='ACF')
pacf(diff(dT_Uni), main='PACF')

xregs = data_Uni[97:131,c(-1,-4,-6,-7)]
xregs$PRICE = (xregs$PRICE)

model1_Uni = auto.arima(dT_Uni_log, xreg = xregs)
model1_Uni = arima(dT_Uni_log, xreg = xregs, order=c(1,0,2), seasonal = c(1,0,1))
summary(model1_Uni)
checkresiduals(model1_Uni)

xregsnew = c(mean(data_Uni$FA), mean(data_Uni$FB), mean(data_Uni$PR1))
xregsnew = matrix(rep(xregsnew, 13), 13,3, byrow=TRUE)
xregsnew = data.frame(price_predict_Uni, xregsnew)
xregsnew

fDemand_Uni_partial = forecast(model1_Uni, xreg = xregsnew, h=13,  level=c(80,95))
fDemand_Uni_partial
fDemand_Uni_partial_ts = ts(exp(fDemand_Uni_partial$mean),start = c(2011,12), frequency = 12 )
autoplot(fDemand_Uni_partial)

# par(mfrow = c(1,2))
# plot(fDemand)
# plot(fDemand_Uni)
