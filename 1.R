library(xts)
library(dplyr)
library(fpp2)
library(tseries)

# The following function reads a weekly demand file and 
# outputs a time-series with monthly figures
#
# You can call it as:
#
#  w2m("input-file-name", "label")
# where the input file is in a .csv format, and 
# the "label" is the name of the weekly demand variable 
# that you want to aggregate into monthly quantities
#
# The function assumes that the data file has ONE row
# for each week.
#
# This calculation allocates weeks that are in the 
# middle of a month wholy to that month, and weeks at the 
# beginning and end of the month are allocated to the
# corresponding month according to the number of days in each month
# (i.e., it assummes that each day of dsales is 1/7 of 
# the weekly sales)
#
# It resturns a monthly time-series starting in 2001
#

w2m <- function(file.name, col.name){
  x <- read.csv(paste0(file.name,".csv"))     # Read the .csv data file
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




# EXAMPLE:
# To illustrate the use we first read a source file and create 
# a simplified summary of demand in punds for every week.
# The we save the file and call the function to 
# aggregate demand into months
#
dCHG <- read.csv("PB-Groc-CHICAGO-patched.csv")[,-1] %>%  # Read file
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL))      # Aggregate sales volume per week

write.csv(dCHG,"Demand-CH-G.csv")   # Write the variable created as a CSV file

md <- w2m("Demand-CH-G","DEMAND") # Call the function with the file name 
plot(md)

md.tr <- window(md, end=c(2009,12))
md.te <- window(md, start=c(2010,1))
m <- auto.arima(md.tr)
summary(m)
checkresiduals(m)
plot(forecast(m, h=12))
lines(fitted(m), col="red")
lines(md.te, pch=19)
