
rm(list=ls()) # clear up data in environment

#load ggplot2
install.packages("tidyverse")
install.packages("ggplot2")

#load xlsx
install.packages("openxlsx")
#draw graph
library(openxlsx)
library(ggplot2)
PriceData<-read.xlsx("/Users/a1/Documents/2021/finance & big data/groupwork01/pricedata.xlsx",1) # import raw data
ggplot(PriceData, aes(x=time, y=log(close), color = company, group = company)) + 
  geom_line(size = 0.5) +
  xlab("Date") +
  ylab("Closing Price of Stocks and Index after logarithm")+
  labs(title = "Graphic detal Liquor Investing, 2009 - 2018",
       subtitle = "based on closing price",
       caption = "Wind")