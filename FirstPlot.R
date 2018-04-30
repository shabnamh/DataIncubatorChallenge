#Question 3 - First Plot
#First plot is going to show number of missed early morning connections for flying 
#from Greenville, SC (airport code GSP) to San francisco, CA (airport code SFO) due to delay of the first flight.

library(dplyr)
library(ggplot2)

# The data is gathered based on the months of the year. Thus I first read all of the files and then
# rbind them to make one data frame with the whole data.

Fi1 <- read.csv("Jan.csv")
Fi2 <- read.csv("Feb.csv")
Fi3 <- read.csv("Mar.csv")
Fi4 <- read.csv("Apr.csv")
Fi5 <- read.csv("May.csv")
Fi6 <- read.csv("Jun.csv")
Fi7 <- read.csv("Jul.csv")
Fi8 <- read.csv("Aug.csv")
Fi9 <- read.csv("Sep.csv")
Fi10 <- read.csv("Oct.csv")
Fi11 <- read.csv("Nov.csv")
Fi12 <- read.csv("Dec.csv")
dat <- rbind(Fi1, Fi2, Fi3, Fi4, Fi5, Fi6, Fi7, Fi8, Fi9, Fi10, Fi11, Fi12)

# I am considering Atlanta,GA (airport code ATL) for the connection since it has frequent flights from GSP.
# Therefore, I am considering DELTA Airlines, since its hub is at Atlanta airport and has the most frequent flights there.

sub3 <- subset(dat, ORIGIN == "GSP" & DEST == "ATL" )  
sub4 <- subset(dat, ORIGIN == "ATL" & DEST == "SFO" )

#in this range there is a flight everyday from GSP to ATL
table((subset(sub3, CARRIER == "DL" & CRS_DEP_TIME > 500 & CRS_DEP_TIME < 645))$FL_DATE)

#in this range there is a flight everyday from ATL to SFO
table((subset(sub4, CARRIER == "DL" & CRS_DEP_TIME > 730 & CRS_DEP_TIME < 900))$FL_DATE)

#creating the subsets needed
fFgt2 <- subset(sub3, CARRIER == "DL" & CRS_DEP_TIME > 500 & CRS_DEP_TIME < 645)
sFgt2 <- subset(sub4, CARRIER == "DL" & CRS_DEP_TIME > 730 & CRS_DEP_TIME < 900)

comFgt3 <- merge(fFgt2, sFgt2, by = "FL_DATE")

#subsetting the data the meets the delay criteria
comFgt4 <- subset(comFgt3, (((CRS_DEP_TIME.y %/%100)*60 + CRS_DEP_TIME.y %% 100) - ((ARR_TIME.x %/%100)*60 + ARR_TIME.x %% 100)) < 20)

#the result shows there has been 7 majr delays that caused missing the flight
# to plot a pie chart  
pl <- ggplot(mday, aes(x = factor(1), y= count, fill=factor(c("Feb", "Apr", "Jul", "Aug", "Dec"))) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") + ggtitle ("Number of Missed Early Morning Flights From GSP to SFO due to Major Delays in 2017 ") + xlab("") + scale_fill_discrete(name = "Months")
