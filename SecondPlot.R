#Question 3 - Second Plot

#Second plot is going to show the percentage delay per month of the busiest airports in USA with over 100,000 
#domestic flights in 2017 with the most arrival delay.

library(dplyr)
library(ggplot2)

#First, read all of the files and rbind them at the end to make one data frame with the whole data.
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

#to select the airports meeting the criteria
FreqAt <- dat[ dat$DEST %in% names(which(table(dat$DEST)>100000)), ]    
#perc <- FreqAt %>% group_by(DEST, MONTH) %>% summarise(count = n())
perc1 <- FreqAt %>% group_by(DEST) %>% summarise(count = n())

FreqAt2 <- FreqAt[FreqAt$ARR_DELAY>0,]

#freq <- FreqAt2 %>% group_by(DEST) %>% summarise(count = n())
FreqAt3 <- FreqAt2 %>% group_by(DEST, MONTH) %>% summarise(count = n())

#to get percentage
final <- merge(FreqAt3, perc1, by = "DEST")
final <- final %>% arrange(DEST,MONTH)
final <- mutate(final, percentage = (count.x / count.y)*100)

# The result shows 19 airports. I have shown the percentage delay per month of each airport over the whole year in a Bar chart.

b <- ggplot(final, aes(x= DEST, y= percentage, fill= percentage, order = -percentage)) + geom_bar(stat = "identity" ) +  ggtitle("Arrival Delays of Domestic Flights at 19 Busiest Airports in 2017 ") +xlab("Airport")+ylab("Percentage") + ylim (c(0,100)) + scale_fill_gradient(low="#38A7F1", high="#0E2D48")
