



he third approach was similar to the one we employed in task 2, however this task demanded a forecast on a weekly basis as opposed to monthly in task 2. We began modeling with a basic ARIMA model and then went on to add regressors to make the model more sensitive to factors that might affect Skippy's demand.
The regressors identified were PRICE, FA, FB and PR1. Since the values of these regressors were not available for the next 8 weeks, it was necessary to predict these values using ARIMA or estimate the value as the mean of the prior data.

Using these predicted values, an 8-week ahead forecast of Skippy's demand for the Chicago market was developed.
























# gLA = read.csv("PB-Groc-LA-patched.csv")
View(gLA)
df_uniq <- unique(gLA$VEND)
length(df_uniq)

# dLA = read.csv("PB-Drug-LA-patched.csv")
# gCh = read.csv("PB-Groc-CHICAGO-patched.csv")
# dCh = read.csv("PB-Drug-CHICAGO-patched.csv")

# Problem 1
dT = read.csv("Demand-Total-Month.csv")[-1]
dT

dTotalTS <- ts(dT$Dtotal, start=2001, frequency=12)
dTotalTS

par(mfrow = c(1,2))
acf(dTotalTS, main='ACF')
pacf(dTotalTS, main='PACF')


model1 = auto.arima(dTotalTS)
summary(model1)
checkresiduals(model1)

fDemand = forecast(model1, h=12, level=c(80,95))
fDemand
autoplot(fDemand)


# Problem 2
# upc = read.csv("UPC information.csv")
# upc = data.frame(filter(upc, L3 == "UNILEVER"))
# upc
# 
# upc$VEND = as.factor(upc$VEND)
# UniCodes = levels(upc$VEND)
# 
# dT


# Problem 2 
dT_Uni = read.csv("Demand-Total-Month-Uni.csv")[-1]
dT_Uni

dTotalTS_Uni <- ts(log(dT_Uni$Dtotal), start=2001, frequency=12)
dTotalTS_Uni

par(mfrow=c(1,1))
plot(dTotalTS_Uni)
#plot(diff(dTotalTS_Uni))

adf.test(dTotalTS_Uni)
adf.test(diff(dTotalTS_Uni))

par(mfrow = c(1,2))
acf(dTotalTS_Uni, main='ACF')
pacf(dTotalTS_Uni, main='PACF')


model1_Uni = auto.arima(dTotalTS_Uni)

# 2,1,1 - 2,1,0
model1_Uni = arima(dTotalTS_Uni, order=c(2,1,0), seasonal = c(1,0,1))
summary(model1_Uni)
checkresiduals(model1_Uni)

fDemand_Uni = forecast(model1_Uni, h=12, level=c(80,95))
fDemand_Uni
autoplot(fDemand_Uni)

par(mfrow = c(1,2))
plot(fDemand)
plot(fDemand_Uni)

