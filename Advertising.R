library(xts)
library(dplyr)
library(fpp2)
library(tseries)


w2m <- function(file.name){
  x <- read.csv(paste0(file.name,".csv"))
  # Create an index to translate IRI-Weeks into calendar dates
  CW <- seq(as.POSIXct("1979-9-3", "GMT"), as.POSIXct("2011-12-19", "GMT"), by = "week")
  # create a calendar index with the IRI weeks in the first column of the file
  cal.ix <- CW[x[,"WEEK"]]
  # Create a variable y as an xts object indexed by "cal.ix"
  y <- xts(x[,], order.by=cal.ix)
  # Create a time index with a daily increment and no-data i.e., NA
  st.day <- first(cal.ix)
  end.day <- as.POSIXct(as.Date(last(cal.ix))+6)
  yd <- xts(,seq(st.day,end.day,"day"))
  # Merge both time series to create a series with valies and some NA
  yd <- merge(y,yd)
  # Replace NA with prior available number and divide by 7
  yd<- na.locf(yd)/7
  # Accumulate demand by month
  ym <- apply.monthly(yd,sum)
  ym <- ts(ym, start=2001, frequency=12)
  return(ym[,c(-1,-2)])  
}
# Function to convert weekly to monthly
w2m_d <- function(file.name, col.name){
  x <- read.csv(paste0(file.name,".csv"))
  # Create an index to translate IRI-Weeks into calendar dates
  CW <- seq(as.POSIXct("1979-9-3", "GMT"), as.POSIXct("2011-12-19", "GMT"), by = "week")
  # create a calendar index with the IRI weeks in the first column of the file
  cal.ix <- CW[x[,"WEEK"]]
  # Create a variable y as an xts object indexed by "cal.ix"
  y <- xts(x[,col.name], order.by=cal.ix)
  # Create a time index with a daily increment and no-data i.e., NA
  st.day <- first(cal.ix)
  end.day <- as.POSIXct(as.Date(last(cal.ix))+6)
  yd <- xts(,seq(st.day,end.day,"day"))
  # Merge both time series to create a series with valies and some NA
  yd <- merge(y,yd)
  # Replace NA with prior available number and divide by 7
  yd<- na.locf(yd)/7
  # Accumulate demand by month
  ym <- apply.monthly(yd,sum)
  ym <- ts(ym, start=2001, frequency=12)
  colnames(ym) <- col.name
  return(ym)  
}

upc = read.csv("UPC information.csv")
upc = data.frame(filter(upc, L3 == "UNILEVER"))
upc$VEND = as.factor(upc$VEND)
UniCodes = levels(upc$VEND)
UniCodes

d1 <- read.csv("PB-Drug-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS)

d2 <- read.csv("PB-Groc-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS)

d3 <- read.csv("PB-Drug-LA-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS)

d4 <- read.csv("PB-Groc-LA-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS)

dTotal = full_join(d1,d2)
dTotal = full_join(dTotal,d3)
dTotal = full_join(dTotal,d4)

View(dTotal)
library(dummies)

dummy_F = dummy(dTotal$F)
dTotal = cbind(dTotal, dummy_F)
dTotal$F = NULL
#dTotal

dummy_D = dummy(dTotal$D)
dTotal = cbind(dTotal, dummy_D)
dTotal$D = NULL
#dTotal

dummy_PR = dummy(dTotal$PR)
dTotal = cbind(dTotal, dummy_PR)
dTotal$PR = NULL
#dTotal

dTotal[,"FAplus"] = dTotal$`FA+`
dTotal$`FA+` = NULL

dTotal <- dTotal %>%  
  mutate(VOL = VOL_EQ * UNITS)%>%
  mutate(FA = VOL*FA, FAplus = VOL*FAplus, FB = VOL*FB, D1 = VOL*D1, D2 = VOL*D2, PR1 = VOL*PR1) %>%
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),TOT.DOLLARS= sum(DOLLARS), FA = sum(FA), FAplus = sum(FAplus), FB = sum(FB), D1 = sum(D1), D2 = sum(D2), PR1 = sum(PR1))  %>%
  mutate(PRICE = TOT.DOLLARS/DEMAND) %>%
  select(WEEK, DEMAND, PRICE, FA, FAplus, FB, D1, D2, PR1)
dTotal

#write.csv(dTotal,"Demand-CH-Uni-Week.csv")   # Write the variable created as a CSV file

write.csv(dTotal,"Demand-Total-Uni-Combined.csv")   # Write the variable created as a CSV file

  demand <- data.frame(w2m_d("Demand-Total-Uni-Combined","DEMAND")) # Call the function with the file name 
demand
vars <- data.frame(w2m("Demand-Total-Uni-Combined"))
vars

dTotal = data.frame(demand, vars)
dTotal[,c(-1,-2)] = dTotal[,c(-1,-2)]/dTotal$DEMAND
dTotal

write.csv(dTotal,"Demand-Total-Month-Uni-Combined.csv")   # Write the variable created as a CSV file








dTotal
dTotal1 = dTotal[,c(1,2)]
library(lubridate)
dTotal1$WEEK = lubridate::ymd( "2001-01-01" ) + lubridate::weeks( dTotal1$WEEK - 1114 )
library(prophet)
library(dplyr)
colnames(dTotal1) = c("ds","y")
dTotal1 = as.data.frame(dTotal1)

m1 <- prophet(dTotal1, weekly.seasonality = TRUE)
future <- make_future_dataframe(m1, periods = 56)
forecast <- predict(m1, future)
plot(m1, forecast)
prophet_plot_components(m1, forecast)


m <- prophet(weekly.seasonality=FALSE, interval.width = 0.95, mcmc.samples = 300)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- fit.prophet(m, dTotal1)
future <- make_future_dataframe(m, periods = 56)
forecast <- predict(m, future)
plot(m,forecast)
prophet_plot_components(m, forecast)
dTotal1.cv <- cross_validation(m, horizon = 730, units = 'days')
head(dTotal1.cv)

dTotal2 <- dTotal1 
dTotal2[,c(3)] = dTotal[,c(3)]
sapply(dTotal2,class)
m2 <- prophet(weekly.seasonality=FALSE, interval.width = 0.95)
m2 <- add_seasonality(m2, name='monthly', period=30.5, fourier.order=5)
m2 <- add_regressor(m2, 'PRICE')
m2 <- fit.prophet(m2,dTotal2)
future <- make_future_dataframe(m2,periods = 56)
future <- add_regressor(future, 'PRICE')
forecast <- predict(m2, future)
plot(m2,forecast)
future$nfl_sunday <- nfl_sunday(future$ds)

devtools::install_github('facebook/prophet', subdir='R', ref='master')

