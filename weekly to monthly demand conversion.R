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

w2m <- function(file.name){
  x <- read.csv(paste0(file.name,".csv"))     # Read the .csv data file
  # Create an index to translate IRI-Weeks into calendar dates
  CW <- seq(as.POSIXct("1979-9-3", "GMT"), as.POSIXct("2011-12-19", "GMT"), by = "week")
  # create a calendar index with the IRI weeks in the first column of the file
  cal.ix <- CW[x[,"WEEK"]]
  # Create a variable y as an xts object indexed by "cal.ix"
  y <- xts(x[,c(-1,-2)], order.by=cal.ix)
  # Create a time index with a daily increment and no-data i.e., NA
  st.day <- first(cal.ix)
  end.day <- as.POSIXct(as.Date(last(cal.ix))+7)
  yd <- xts( ,seq(st.day,end.day,"day"))
  # Merge both time series to create a series with valies and some NA
  yd <- merge(y,yd)
  # Replace NA with prior available number and divide by 7
  yd<- na.locf(yd)
  # Accumulate demand by month
  ym <- apply.monthly(yd,mean)
  ym <- ts(ym, start=2001, frequency=12)
return(ym)  
}



# EXAMPLE:
# To illustrate the use we first read a source file and create 
# a simplified summary of demand in punds for every week.
# The we save the file and call the function to 
# aggregate demand into months
#
dCHD <- read.csv("PB-Drug-CHICAGO-patched.csv")[,-1] %>%  # Read file
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL), PRICE = mean(PPU))      # Aggregate sales volume per week
dCHD
dCHG <- read.csv("PB-Groc-CHICAGO-patched.csv")[,-1] %>%  # Read file
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),PRICE = mean(PPU))      # Aggregate sales volume per week
dCHG

dLAD <- read.csv("PB-Drug-LA-patched.csv")[,-1] %>%  # Read file
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),PRICE = mean(PPU))      # Aggregate sales volume per week
dLAD
sapply(dCHD,class)
dLAG <- read.csv("PB-Groc-LA-patched.csv")[,-1] %>%  # Read file
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL), PRICE = mean(PPU))      # Aggregate sales volume per week
dLAG


total = data.frame("WEEK"=dCHD$WEEK,"Dtotal"=(dCHD$DEMAND)+(dCHG$DEMAND)+(dLAD$DEMAND)+(dLAG$DEMAND), "AvgPrice" = (dCHD$PRICE+dCHG$PRICE+dLAD$PRICE+dLAG$PRICE)/4)
total

write.csv(total,"Demand-Total.csv")   # Write the variable created as a CSV file

md <- data.frame(w2m("Demand-Total","Dtotal")) # Call the function with the file name 
md
mp <- data.frame(w2m("Demand-Total","AvgPrice"))
mp

mdata = data.frame(md, mp)
mdata = ts(mdata, start=2001, frequency = 12)
mdata

write.csv(mdata,"Demand-Total-Month.csv")   # Write the variable created as a CSV file

# Problem 2
upc = read.csv("UPC information.csv")
upc = data.frame(filter(upc, L3 == "UNILEVER"))
upc

upc$VEND = as.factor(upc$VEND)
UniCodes = levels(upc$VEND)
UniCodes


# EXAMPLE:
# To illustrate the use we first read a source file and create 
# a simplified summary of demand in punds for every week.
# The we save the file and call the function to 
# aggregate demand into months
#
dCHD1 <- read.csv("PB-Drug-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL), PRICE = mean(PPU))
dCHD1

dCHG1 <- read.csv("PB-Groc-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL), PRICE = mean(PPU))      # Aggregate sales volume per week
View(dCHG1)
dLAD1 <- read.csv("PB-Drug-LA-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL), PRICE = mean(PPU))      # Aggregate sales volume per week
dLAD1
dLAG1 <- read.csv("PB-Groc-LA-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% UniCodes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL), PRICE = mean(PPU))      # Aggregate sales volume per week
dLAG1

total1 = data.frame("WEEK"=dCHD1$WEEK,"Dtotal"=dCHD1$DEMAND+dCHG1$DEMAND+dLAD1$DEMAND+dLAG1$DEMAND, "AvgPrice" = (dCHD1$PRICE+dCHG1$PRICE+dLAD1$PRICE+dLAG1$PRICE)/4)
total1


write.csv(total1,"Demand-Total-Uni.csv")   # Write the variable created as a CSV file

md1 <- data.frame(w2m("Demand-Total-Uni"))
md1
mp1 <- data.frame(w2m("Demand-Total-Uni","AvgPrice"))
mp1

mdata1 = data.frame(md1, mp1)
mdata1 = ts(mdata1, start=2001, frequency = 12)
mdata1

write.csv(md1,"Demand-Total-Month-Uni.csv")   # Write the variable created as a CSV file

# LA
total_LA = data.frame("WEEK"=dLAD1$WEEK, "Dtotal"=dLAD1$DEMAND+dLAG1$DEMAND, "AvgPrice" = (dLAD1$PRICE+dLAG1$PRICE)/2)
write.csv(total_LA,"Demand-Total-Week-LA.csv")   # Write the variable created as a CSV file

# CH
total_CH = data.frame("WEEK"=dCHD1$WEEK, "Dtotal"=dCHD1$DEMAND+dCHG1$DEMAND, "AvgPrice" = (dCHD1$PRICE+dCHG1$PRICE)/2)
write.csv(total_CH,"Demand-Total-Week-CH.csv")   # Write the variable created as a CSV file



