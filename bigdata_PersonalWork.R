

# Clear Global Environment
rm(list = ls())
getwd()
setwd("/Users/a1/Documents/2021/finance & big data/PersonalWork") 


#### 1.Prepare Data ####

### install & library packages ###
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
install.packages("viridis")
library(viridisLite)
library(viridis)
library(hrbrthemes)

# map
install.packages("sf")
library(sf)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("ggspatial")
library(ggspatial)

# multiple linear regression
install.packages("gvlma")
install.packages("car")
install.packages("carData")
install.packages("effects")

# logistic regression
install.packages("pROC")

### load and preprocess ###
data_ori <- read.csv("restaurant18_长沙.csv",header=F, stringsAsFactors=T,sep = ",",na.strings = "")
data_copy <- data_ori[,-1]
data_copy <- data_copy[, -13]
data_copy <- data_copy[, -23]
names(data_copy) <- c("shop_id","is_closed","name","city_id","city","real_city","province","area_code",
                     "phone","region","address","avg_price","big_cate","big_cate_id",
                     "small_cate","small_cate_id","stars","review_count","good_remarks","bad_remarks",
                     "bookable","takeaway","product_rating","environment_rating","service_rating",
                     "longitude","latitude","default_pic","dishes")
data_copy <- data_copy[-1,] #remove first row
table(is.na(data_copy)) #how many NA in avg price
data_omit <- na.omit(data_copy) # remove NA 
data_omit <- as.data.frame(data_omit) # change in to df

### sample ###
id <- c(1:9753)
data_omit <- cbind(data_omit,id)
id_sample <- sample(id,9000,replace=F) #取整
data_sample <- data_copy[id_sample, ]




#### 2.Data Visualization ####

### 2.1 Average Price & Stars & good remarks ###
data_omit$avg_price <- as.numeric(as.character(data_omit$avg_price)) # factor to numeric
data_omit$good_remarks <- as.numeric(as.character(data_omit$good_remarks)) #factor to numeric
## overview
data_omit %>%
  ggplot(aes(x = avg_price, y =good_remarks)) +
  geom_point(alpha=0.3)+    
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")

## remove outliers
outlier_df <- data_omit %>% filter(avg_price>600)
rmoutlier_df <- data_omit %>% filter(avg_price<600)   #remove outliers

data_omit %>%
  ggplot(aes(x = avg_price, y =good_remarks)) +
  geom_point()+
  geom_point(data = outlier_df, aes(x=avg_price, y=good_remarks), color='red',size=2)+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks (Highlight Outliers)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")

## after remove outliers
rmoutlier_df %>%
  ggplot(aes(x = avg_price, y =good_remarks, color=stars, size=good_remarks)) +
  geom_point(alpha=0.3)+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  labs(title = "Average Price and Amount of Good Marks (Remove Outlier)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")

## difference between different assessment
unique(rmoutlier_df$stars)
data_5star <- rmoutlier_df %>% filter(stars == "5.0" )
data_45star <- rmoutlier_df %>% filter(stars == "4.5")
data_4star <- rmoutlier_df %>% filter(stars == "4.0")
data_35star <- rmoutlier_df %>% filter(stars == "3.5")
data_3star <- rmoutlier_df %>% filter(stars == "3.0")
data_2star <- rmoutlier_df %>% filter(stars == "2.0")

p5 <- data_5star %>%
  ggplot(aes(x=avg_price, y =good_remarks, size=good_remarks))+
  geom_point(alpha=0.2, color="blueviolet")+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks(5 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")
p5

p45 <- data_45star %>%
  ggplot(aes(x=avg_price, y =good_remarks, size=good_remarks))+
  geom_point(alpha=0.2,color="chartreuse3")+theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks(4.5 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")
p45

p4 <- data_4star %>%
  ggplot(aes(x=avg_price, y =good_remarks, size=good_remarks))+
  geom_point(alpha=0.2,color="brown4")+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks(4 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")
p4

p35 <- data_35star %>%
  ggplot(aes(x=avg_price, y =good_remarks, size=good_remarks))+
  geom_point(alpha=0.2, color="cadetblue4")+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks(3.5 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")
p35

p3 <- data_3star %>%
  ggplot(aes(x=avg_price, y =good_remarks, size=good_remarks))+
  geom_point(alpha=0.2, color="burlywood3")+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks(3 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")
p3

p2 <- data_2star %>%
  ggplot(aes(x=avg_price, y =good_remarks, size=good_remarks))+
  geom_point(alpha=0.2, color="chocolate3")+
  theme_ipsum()+
  ylab("Amount of Good Marks")+
  xlab("Average Price")+
  theme(legend.position = "none")+
  labs(title = "Average Price and Amount of Good Marks(2 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")
p2

## different fixed line 
data_5star %>%
  ggplot(aes(x=avg_price,y=good_remarks, size= good_remarks))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE, color= "goldenrod2") + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE, color = "brown4") +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2, color="cadetblue4") + # add locally estimated scatterplot smoothing loess line
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  theme(legend.position = "none") +
  labs(title = "Fixed Line of Average Price and Amount of Good Marks(5 Stars)",
       subtitle = "Data points are Price-Amount of good marks",
       caption = "Source: MeiTuan.")

data_45star %>%
  ggplot(aes(x=avg_price,y=good_remarks, size= good_remarks))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE, color= "goldenrod2") + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE, color = "brown4") +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2, color="cadetblue4") + # add locally estimated scatterplot smoothing loess line
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  theme(legend.position = "none") +
  labs(title = "Fixed Line of Average Price and Amount of Good Marks(4.5 Stars)",
       subtitle = "Data points are Price-Amount of good marks",
       caption = "Source: MeiTuan.")


data_4star %>%
  ggplot(aes(x=avg_price,y=good_remarks, size= good_remarks))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE, color= "goldenrod2") + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE, color = "brown4") +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2, color="cadetblue4") + # add locally estimated scatterplot smoothing loess line
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  theme(legend.position = "none") +
  labs(title = "Fixed Line of Average Price and Amount of Good Marks(4 Stars)",
       subtitle = "Data points are Price-Amount of good marks",
       caption = "Source: MeiTuan.")

data_35star %>%
  ggplot(aes(x=avg_price,y=good_remarks, size= good_remarks))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE, color= "goldenrod2") + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE, color = "brown4") +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2, color="cadetblue4") + # add locally estimated scatterplot smoothing loess line
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  theme(legend.position = "none") +
  labs(title = "Fixed Line of Average Price and Amount of Good Marks(3.5 Stars)",
       subtitle = "Data points are Price-Amount of good marks",
       caption = "Source: MeiTuan.")

data_3star %>%
  ggplot(aes(x=avg_price,y=good_remarks, size= good_remarks))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE, color= "goldenrod2") + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE, color = "brown4") +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2, color="cadetblue4") + # add locally estimated scatterplot smoothing loess line
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  theme(legend.position = "none") +
  labs(title = "Fixed Line of Average Price and Amount of Good Marks(3 Stars)",
       subtitle = "Data points are Price-Amount of good marks",
       caption = "Source: MeiTuan.")

data_2star %>%
  ggplot(aes(x=avg_price,y=good_remarks, size= good_remarks))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE, color= "goldenrod2") + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE, color = "brown4") +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2, color="cadetblue4") + # add locally estimated scatterplot smoothing loess line
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  theme(legend.position = "none") +
  labs(title = "Average Price and Amount of Good Marks(2 Stars)",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")

## Distribution of AVG Prices of different assessment

#bar
rmoutlier_df %>%
  ggplot(aes(x= avg_price, fill= stars))+
  geom_bar(position = position_dodge(0.75))+
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  labs(title = "Average Price and Amount of Good Marks:Bar",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")

#density
rmoutlier_df  %>%
  ggplot(aes(x= avg_price, fill= stars, color= stars))+
  geom_density(alpha=0.2)+
  theme_ipsum() +
  ylab("Amount of Good Marks") +
  xlab("Average Price") +
  labs(title = "Average Price and Amount of Good Marks:Density",
       subtitle = "Data points are price-amount of good marks",
       caption = "Source: MeiTuan.")



###  2.2 Average Price & City  ###
unique(data_omit$city_id)

## overview

#Bar
rmoutlier_df %>%
  ggplot(aes(x= avg_price, fill= city_id))+
  geom_bar(position = position_dodge(0.75))+
  scale_fill_discrete(name="City Name",
                      breaks=c("1376", "1379", "344"),
                      labels=c("LiuYang", "NingXiang County", "ChangSha"))+
  theme_ipsum() +
  ylab("Amount of Restaurants") +
  xlab("Average Price") +
  labs(title = "Distribution of Average Price : Bar",
       subtitle = "Group by city",
       caption = "Source: MeiTuan.")


#Density
rmoutlier_df  %>%
  ggplot(aes(x= avg_price, fill= city_id, color= city_id))+
  geom_density(alpha=0.2)+
  scale_fill_discrete(name="City Name",
                      breaks=c("1376", "1379", "344"),
                      labels=c("LiuYang", "NingXiang County", "ChangSha"))+
  scale_color_discrete(name="City Name",
                      breaks=c("1376", "1379", "344"),
                      labels=c("LiuYang", "NingXiang County", "ChangSha"))+
  theme_ipsum() +
  ylab("Amount of Restaurants") +
  xlab("Average Price") +
  labs(title = "Distribution of Average Price : Density",
       subtitle = "Group by city",
       caption = "Source: MeiTuan.")

## grouping by city
data_changs <- rmoutlier_df %>% filter(city_id == 344 )
data_liuy <- rmoutlier_df %>% filter(city_id == 1376 )
data_ningy <- rmoutlier_df %>% filter(city_id == 1379 )

#changsha
data_changs %>%
  ggplot(aes(x=avg_price))+
  geom_density()+
  theme_ipsum()+
  ylab("Amount of Restaurants")+
  xlab("Average Price")+
  labs(title = "Distribution of Average Price : Density",
       subtitle = "City: ChangSha",
       caption = "Source: MeiTuan.")

#liuyang
data_liuy %>%
  ggplot(aes(x=avg_price))+
  geom_density()+
  theme_ipsum()+
  ylab("Amount of Restaurants")+
  xlab("Average Price")+
  labs(title = "Distribution of Average Price : Density",
       subtitle = "City: LiuYang",
       caption = "Source: MeiTuan.")

#ningyang
data_ningy %>%
  ggplot(aes(x=avg_price))+
  geom_density()+
  theme_ipsum()+
  ylab("Amount of Restaurants")+
  xlab("Average Price")+
  labs(title = "Distribution of Average Price : Density",
       subtitle = "City: NingYang County",
       caption = "Source: MeiTuan.")






#### 3.Regression ####
data_omit$product_rating <- as.numeric(as.character(data_omit$product_rating)) #factor to numeric
data_omit$environment_rating <- as.numeric(as.character(data_omit$environment_rating)) #factor to numeric
data_omit$service_rating <- as.numeric(as.character(data_omit$service_rating)) #factor to numeric


### 3.1 Stars & product rating, environment rating, service rating ###

## Multiple Linear Regression ##
library(carData)
library(car)
library(effects)


# Prepare Data #
M1 <- data.frame(cbind(data_omit$stars,data_omit$product_rating,data_omit$environment_rating,data_omit$service_rating))
names(M1) <- c("Y1","X1","X2","X3") #Y1=stars, X1=product, X2=environment,X3=service
M1$Y1 <- as.numeric(as.character(M1$Y1)) # factor to numeric

# Covariance Matrix #
cor(M1) 
scatterplotMatrix(M1,spread = FALSE, lty.soomth = 2, main = "Scatter Plot Matrix")

# Build Model #
MLR <- lm(Y1 ~ X1*X2*X3, data = M1)
MLR1 <- lm(Y1 ~ X1+X2+X3, data = M1)
MLR2 <- lm(Y1 ~ X1*X2+X3, data = M1)
MLR3 <- lm(Y1 ~ X1+X2*X3, data = M1)
MLR4 <- lm(Y1 ~ X1*X3+X2, data = M1)

# Comparison #
anova(MLR,MLR1,MLR2,MLR3,MLR4)
AIC(MLR,MLR1,MLR2,MLR3,MLR4) 

# Choose MLR2 #
summary(MLR2)
plot(effect("X1:X2",MLR,multiline=TRUE))


# Diagnosis #
plot(MLR2)

# QQplot #
qqPlot(MLR2, labels= row.names(states), id.method="identify", simulate=TRUE,main = "Q-Q Plot") 

# Distribution of Errors #
residplot<-function(MLR2,nbreaks=10){
  z<-rstudent(MLR2)
  hist(z,breaks=nbreaks,freq=FALSE,
       xlab="Studnetized Residual",
       main="Distribution of Errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean=mean(z),sd=sd(z)),
        add=TRUE,col="blue",lwd=2)
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2,col=c("blue","red"),cex=0.7)}

residplot(MLR)   

# durbinWastonTest #
durbinWatsonTest(MLR2)  

# Non-constant Variance Score Test #
ncvTest(MLR2) 

# Comprehensive validation of linear model assumptions #
library(gvlma)
gvmodel_MLR2 <- gvlma(MLR2) 
summary(gvmodel_MLR2)


### 3.2 Stars & product rating, environment rating, service rating ###

## Logistic Regression ##

# Prepare Data#
M2 <- data.frame(cbind(data_omit$stars, data_omit$product_rating, data_omit$environment_rating, data_omit$service_rating))
names(M2) <- c("Y1","X1","X2","X3") #Y1=stars, X1=product, X2=environment,X3=service
M2$Y1 <- as.numeric(as.character(M2$Y1)) # factor to numeric
M2$Y1 <- ifelse(M2$Y1 >= 4,1,0)
M2 <- cbind(M2,id)

# Sample #
id_train <- sample(1:9752,6750, replace=F)
logs_train <- M2[which(M2$id %in% id_train),]
logs_test <- M2[which(!(M2$id %in% id_train)),]

# Observe Data #
table(logs_train$Y1) 
table(logs_test$Y1) 

# Build Model #
LR <- glm(formula = Y1 ~ X1 + X2 + X3, family = binomial(), data = logs_train)
summary(LR)

# Diagnosis #
anova(LR, test="Chisq")
coef(LR)
vcov(LR)

# Prediction #
probability <- predict(object = LR, newdata = logs_test, type = "response")
pre_test <- cbind(logs_test, probability)
pre_test <- transform(pre_test, predict = ifelse(probability <= 0.5,0,1))
head(pre_test)
table(pre_test$Y1, pre_test$predict)

# Diagnosis of Prediction #
sum_diag <- sum(diag(table(pre_test$Y1, pre_test$predict)))
sum <- sum(table(pre_test$Y1, pre_test$predict))
accuracy <- sum_diag/sum
accuracy

# Visualization #
library(pROC)
roc_curve <- roc(pre_test$Y1 ~ probability)
roc_curve
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
roc_plot <- plot(x=x, y=y, xlim=c(0,1), ylim=c(0,1),
                   xlab = "1-Specificity", ylab = "sensitivity", main = "ROC Curve")+
  theme_ipsum()
abline(a = 0, b = 1, col= "brown2")
auc <- roc_curve$auc
text(0.5, 0.4, paste('AUC:', round(auc, digits = 4)),col = "cadetblue4")



