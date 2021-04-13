rm(list=ls()) # clear up data in environment
setwd("")

# Load ggplot2
install.packages(c("tidyverse", "ggplot2")) # install package from CRAN

library(ggplot2)


#########################
###  Ridgeline chart  ###
#########################
install.packages("ggridges")

library(ggplot2)
library(ggridges)

ggplot(
  lincoln_weather, 
  aes(x = `Mean Temperature [F]`, y = `Month`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE') 



#########################
# Plot time series data #
#########################

## Simple line plot 
library(dplyr)
economics %>%
  ggplot(aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")


### Change style add x-axis sclae 
library(scales)
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '5 years', 
               labels = date_format("%b-%y")) +
  labs(title = "Personal Savings Rate",
       subtitle = "1967 to 2015",
       x = "",
       y = "Personal Savings Rate") +
  theme_minimal()


####################
# Plot area charts #
####################
# basic area chart
ggplot(economics, aes(x = date, y = psavert)) +
  geom_area(fill="lightblue", color="black") +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")



#######################
# Stacked area charts #
#######################
install.packages("gcookbook")
# stacked area chart
library(gcookbook)
uspopage <- uspopage
ggplot(uspopage, aes(x = Year,
                     y = Thousands, 
                     fill = AgeGroup)) +
  geom_area() +
  labs(title = "US Population by age",
       x = "Year",
       y = "Population in Thousands")


# Make it prettier 
data(uspopage, package = "gcookbook")
ggplot(uspopage, aes(x = Year,
                     y = Thousands/1000, 
                     fill = forcats::fct_rev(AgeGroup))) +
  geom_area() +
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Population in Millions",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


############################
# multivariate time series #
############################
install.packages("quantmod")
library(xts)
library(zoo)
library(TTR)
library(quantmod)
library(dplyr)

## get apple (AAPL) closing prices
getSymbols("AAPL", return.class = "data.frame", from="2018-01-01")

apple <- AAPL %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, AAPL.Close) %>%
  rename(Close = AAPL.Close) %>%
  mutate(Company = "Apple")

## get facebook (FB) closing prices
getSymbols("FB", return.class = "data.frame", from="2018-01-01")   #download failed? why? unable to import FB

facebook <- FB %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, FB.Close) %>%
  rename(Close = FB.Close) %>%
  mutate(Company = "Facebook")

## Append data for both companies
stockprice <- rbind(apple, facebook)

## plot data
ggplot(stockprice, 
       aes(x=Date, y= Close, color=Company)) + 
  geom_line(size=1) +
  scale_x_date(date_breaks = '1 month', 
               labels = scales::date_format("%b")) +
  scale_y_continuous(limits = c(100, 300), 
                     breaks = seq(100, 300 , 50),
                     labels = scales::dollar) +
  labs(title = "NASDAQ Closing Prices",
       subtitle = "Jan 2018 - March 2021",
       caption = "source: Yahoo Finance",
       y = "Closing Price") +
  theme_minimal() 

+
  scale_color_brewer(palette = "Dark2")


#####################################
# Technical analysis charting tools #
#####################################

install.packages("TTR")

library(TTR)

getSymbols("AAPL",src="yahoo")
chartSeries(to.daily(AAPL)) 

## add Moving Average Convergence Divergence to chart
addMACD()

## add Bollinger Bands
addBBands() 

# Stock analysis: Expended to all S&P500 Stock
library(xml2)
library(rvest)
library(stringr)

## Web-scrape SP500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%  # timeout was reached
  html_node("table.wikitable") %>%  
  html_table() %>%
  select(`Ticker Symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
  as_tibble()

## Format names
names(sp_500) <- sp_500 %>% 
  names() %>% 
  str_to_lower() %>% 
  make.names()

## Show results
sp_500 

## The lapply() function below loops through each column of the data set counting the length of the unique items. The result is a count of distinct values for each column.
sp_500 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist() # show in condensed format

## Summarize the distribution of securities by sector using group_by() and summarise() to get counts
library(forcats) # use to work with categorical factor variable
sp_500 %>%
  ## Summarise data by frequency
  group_by(gics.sector) %>%
  summarise(count = n()) %>%
  # Visualize 
  ggplot(aes(x = gics.sector %>% fct_reorder(count),
             y = count
  )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), size = 3, nudge_y = 4, nudge_x = .1) + 
  scale_y_continuous(limits = c(0,100)) +
  ggtitle(label = "Sector Frequency Among SP500 Stocks") +
  xlab(label = "GICS Sector") +
  theme(plot.title = element_text(size = 16)) + 
  coord_flip()


##########################################
# Change-point Detection: Pettitt’s Test #
##########################################
library(package = dplyr)
library(package = ggplot2)
install.packages("trend")
library(package = trend)

## Plot
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

## Pettitt’s test
pettittTest <- pettitt.test(x = economics[['psavert']])
print(pettittTest)
print(economics[['date']][pettittTest$estimate])

## add the reference line
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate") +
  geom_vline(mapping = aes(xintercept = as.numeric(economics[['date']][pettittTest$estimate])),
             linetype = 2,
             colour = "red",
             size = 2)+
  theme_minimal()
+
  scale_color_brewer(palette = "Dark2")

###########################################
# Detecting and Plotting Sequence Changes #
###########################################
install.packages("devtools")
install.packages("digest")
library(digest)
devtools::install_github("twitter/BreakoutDetection")
library(BreakoutDetection)

data(Scribe)
res = breakout(Scribe, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot

install.packages("cpm")
library(cpm)

# Load Nathan's wakeup data
gmorning <- read.csv("gmorning-processed.tsv", stringsAsFactors=FALSE, sep="\t")   # connot open file gmorning-processed

## Sort it by day and hour
gmorning.o <- gmorning[with(gmorning, order(dayssincestart, hour)),]
attach(gmorning.o)

## Find the change points
changes <- processStream(hour, "Kolmogorov-Smirnov", ARL0=1000, startup=15)
numChanges <- length(changes$changePoints)

## Plot the points and change points
plot(hour, xlab="", ylab="hour", main="Wake Times")
for (i in 1:length(changes$changePoints)) {
  abline(v=changes$changePoints[i], lty=2, col="red")
}

## Various Change Point Models
par(mfrow=c(3,3), mar=c(3,3,3,3))
cpmTypes <- c("Student", "Bartlett", "GLR", "Mann-Whitney", "Mood", "Lepage", "Kolmogorov-Smirnov", "Cramer-von-Mises")
for (i in 1:length(cpmTypes)) {
  changes <- processStream(hour, cpmTypes[i], ARL0=1000, startup=15)
  numChanges <- length(changes$changePoints)
  
  plot(hour, xlab="", ylab="hour", main=paste(cpmTypes[i], "-", numChanges, "change pts"), col="#cccccc")
  for (j in 1:length(changes$changePoints)) {
    abline(v=changes$changePoints[j], lty=1, col="red")
  }
}

##
### Base plot
plot(hour, xlab="", ylab="hour", main="Wake Times", col="gray")

### Change points
changes <- processStream(hour, "Kolmogorov-Smirnov", ARL0=1000, startup=15)
numChanges <- length(changes$changePoints)

### Get means at the beginning and end of sequence
firstChange <- changes$changePoints[1]
firstMean <- mean( hour[ 0:changes$changePoints[1] ] )
lastChange <- changes$changePoints[numChanges]
lastMean <- mean( hour[ lastChange:length(hour) ] )

x0 <- c(0, lastChange)
y0 <- c(firstMean, lastMean)
x1 <- c(firstChange, length(hour))
y1 <- c(firstMean, lastMean)

for (i in 2:length(changes$changePoints)) {
  prevChangePt <- changes$changePoints[i-1]
  currChangePt <- changes$changePoints[i]
  
  # Get the mean for the segment
  currMean <- mean(hour[prevChangePt:currChangePt])
  
  # Add segment
  x0 <- c(x0, prevChangePt)
  y0 <- c(y0, currMean)
  x1 <- c(x1, currChangePt)
  y1 <- c(y1, currMean)
}

### Draw segments
segments(x0, y0, x1, y1, col="red")

## 
plot(dayssincestart, hour, xlab="", ylab="", main="Wake Times", type="n", axes=FALSE, ylim=c(6.5,13))

### Orient labels horizontally, with smaller font
par(las=1, cex=0.7)

### Horizontal axis
dayTicks <- c(1, 405, 770, 1135, 1501, 1957)
dayLabels <- c("Nov. 2008", "2010", "2011", "2012", "2013", "April 2014")
axis(1, at=dayTicks, labels=dayLabels, pos=6.5)

### Vertical axis
hourTicks <- 7:13
hourLabels <- c("7am", "8am", "9am", "10am", "11am", "Noon", "1pm")
axis(2, at=hourTicks, labels=hourLabels, pos=-50, col="white")

### Grid lines
for (i in 1:length(hourTicks)) {
  abline(h=hourTicks[i], lty=1, lwd=0.32, col="#cccccc")
}

### Draw points
points(dayssincestart, hour, pch=20, cex=0.5, col="#999999")

### Mean at each change point
segments(x0, y0, x1, y1, col="orange", lwd=4)


#####################
# Calendar Heat Map #
#####################

library(ggplot2)
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

## install tidyquant 
install.packages('tidyquant', repos = "http://cran.us.r-project.org")
library(tidyquant) 

library(plyr)
library(plotly)

## get data using tidyquant 
amznStock <- as.data.frame(tidyquant::tq_get(c("AMZN"),get="stock.prices"))
## Using data only after 2016
amznStock <- amznStock[year(amznStock$date) > 2016, ] 
## finding the day no. of the week
amznStock$weekday = as.POSIXlt(amznStock$date)$wday

##converting the day no. to factor 
amznStock$weekdayf<-factor(amznStock$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) 

install.packages('lubridate')  # have to install this package or it cannot library
library(lubridate)
## finding the month 
amznStock$monthf<-factor(month(amznStock$date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) 

## finding the year and the month from the date. Eg: Nov 2018 
amznStock$yearmonth<- factor(as.yearmon(amznStock$date)) 

## finding the week of the year for each date 
amznStock$week <- as.numeric(format(amznStock$date,"%W")) 

## normalizing the week to start at 1 for every month 
amznStock<-ddply(amznStock,.(yearmonth),transform, monthweek=1+week-min(week)) 

amznStock %>%
  ggplot(aes(monthweek, weekdayf, fill = amznStock$adjusted)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(amznStock$date)~monthf) + 
  scale_fill_gradient(low="red", high="green") + 
  xlab("Week of Month") + 
  ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: AMZN Stock Prices") + 
  labs(fill = "Price") 

# using calendarHeat()
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")  # cannot find calenderheat
r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384") 

calendarHeat(amznStock$date, amznStock$adjusted, ncolors = 99, color = "r2g", varname="AMZN Adjusted Close")





