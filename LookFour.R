
# Clear Global Environment
rm(list = ls())
getwd()
setwd("/Users/a1/Documents/2021/machine learning/task") 


#### Install & Library Pacakges ####
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
install.packages("viridis")
library(viridisLite)
library(viridis)
library(hrbrthemes)

library("lattice")
library("ggplot2")
install.packages('quanteda.textmodels')
library(quanteda.textmodels)
install.packages("pROC")
library(pROC)
install.packages("caret")
library(caret)


#### Import Data ####

train <- read.csv("all_data_train.csv",header=F, stringsAsFactors=T, sep = ",",na.strings = "")
test <- read.csv("test.csv",header=F, stringsAsFactors=T, sep = ",",na.strings = "")
train_copy <- train[-1,]
test_copy <- test[-1,]
names(train_copy) <- c("serial", "id", "emp", "comp", "LastRatingDate", "left", "Support", "Oppose", "Rating")
names(test_copy) <- c("id", "emp", "comp", "LastRatingDate")

table(is.na(train_copy)) # how many NA in train set
train_omit <- na.omit(train_copy) # remove NA 

rownames(train_omit) <- 1:nrow(train_omit)

#### Logistic Regression ####

# prepare data #
train_omit$Support <- as.numeric(as.character(train_omit$Support))
train_omit$Oppose <- as.numeric(as.character(train_omit$Oppose))
train_omit$Rating <- as.numeric(as.character(train_omit$Rating))
train_omit$left <- as.numeric(as.character(train_omit$left))
sup <- train_omit$Support
opp <- train_omit$Oppose
assess <- sup-opp
train_omit <- cbind(train_omit, assess)

# sample #
id_train <- sample(0:3526,2625, replace=F)
logs_train <- train_omit[which((train_omit$serial %in% id_train)),]
logs_test <- train_omit[which(!(train_omit$serial %in% id_train)),]


# build model #
LR <- glm(formula = left ~ assess + Rating, family = binomial() ,data = logs_train)
summary(LR)

# Diagnosis #
anova(LR, test="Chisq")
coef(LR)
vcov(LR)

# Prediction #
probability <- predict(object = LR, newdata = logs_test, type = "response")
pre_test <- cbind(logs_test, probability)
prediction <- ifelse(pre_test$probability >= .15005 ,0 ,1)
pre_test <- cbind(pre_test, prediction)
head(pre_test)
table(pre_test$left, pre_test$prediction)

# Diagnosis of Prediction #
sum_diag <- sum(diag(table(pre_test$left, pre_test$prediction)))
sum <- sum(table(pre_test$left, pre_test$prediction))
accuracy <- sum_diag/sum
accuracy

# Visualization #
roc_curve <- roc(pre_test$left ~ pre_test$probability)
roc_curve
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
roc_plot <- plot(x=x, y=y, xlim=c(0,1), ylim=c(0,1),
                 xlab = "1-Specificity", ylab = "sensitivity", main = "ROC Curve for Linear Regression")+
  theme_ipsum()
abline(a = 0, b = 1, col= "brown2")
auc <- roc_curve$auc
text(0.5, 0.4, paste('AUC:', round(auc, digits = 4)),col = "cadetblue4")



#### Random Forest ####
install.packages("randomForest")
library(randomForest)

# build data frame#
M1 <- data.frame(cbind(train_omit$serial, train_omit$left, train_omit$Support, train_omit$Oppose, train_omit$Rating, train_omit$assess))
names(M1) <- c("serial","left", "Support", "Oppose", "Rating", "assess")


#sample#
rf_train <- M1[which((M1$serial %in% id_train)),]
rf_test <- M1[which(!(M1$serial %in% id_train)),]


rf_train$left = as.factor(rf_train$left)
rf_test$left = as.factor(rf_test$left)

rf_mod <- randomForest(left ~ Support + Oppose + Rating,
                       data = rf_train,
                       ntree =500,
                       mtry=3,
                       importance=TRUE ,
                       proximity=TRUE)

rf_mod$importance
varImpPlot(rf_mod, main = "variable importance")

# prediction #
rf_pre <- predict(rf_mod, newdata = rf_test)
rf_p_ran = data.frame(prob = rf_pre, obs = rf_test$left)
table(rf_test$left, rf_pre, dnn = c("Actual", "Predict"))
rf_roc <- roc(rf_test$left, as.numeric(rf_pre))
rf_roc
rf_x <- 1-roc_curve$specificities
rf_y <- roc_curve$sensitivities
rf_roc_plot <- plot(x=x, y=y, xlim=c(0,1), ylim=c(0,1),
                 xlab = "1-Specificity", ylab = "sensitivity", main = "ROC Curve for Linear Regression")+
  theme_ipsum()
abline(a = 0, b = 1, col= "brown2")
rf_auc <- rf_roc$auc
text(0.5, 0.4, paste('AUC:', round(rf_auc, digits = 4)),col = "cadetblue4")


#### RSNNS ####

install.packages("RSNNS")
install.packages("Rcpp")
library(Rcpp)
library(RSNNS)
library(grid)
install.packages("MASS")
library(MASS)
install.packages("neuralnet")
library(neuralnet)

#sample#
nn_train <- M1[which((M1$serial %in% id_train)),]
nn_test <- M1[which(!(M1$serial %in% id_train)),]

nn_train$left = as.numeric(as.character(nn_train$left))
nn_test$left = as.numeric(as.character(nn_test$left))

#train
#model <- mlp(nn_train$assess, nn_train$left, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=50, inputsTest=nn_test$assess, targetsTest=nn_test$left)
#model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="BackpropBatch", learnFuncParams=c(10, 0.1), maxit=100, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
#model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="SCG", learnFuncParams=c(0, 0, 0, 0),  maxit=30, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
#nn_pre <- predict(model, newdata = nn_test)


#model#
net_mod <- neuralnet( formula= left ~ assess + Rating, data = nn_train, hidden=c(4 ,2), 
                      threshold=0.01, learningrate = 0.1, 
                      algorithm = "rprop+", err.fct = "sse", act.fct = "logistic",
                      linear.output = FALSE)
print(net_mod)
plot(net_mod)

# predict #
net_pre <- compute(net_mod, nn_test)
ls(net_pre)
print(net_pre$net.result)
net_pre_result <- net_pre$net.result
nn_test <- cbind(nn_test, net_pre_result)
nn_prediction <- ifelse(nn_test$net_pre_result >= .35 ,1 ,0)
nn_test <- cbind(nn_test, nn_prediction)
head(nn_test)
table(nn_test$left, nn_test$nn_prediction)

# Diagnosis of Prediction #
nn_sum_diag <- sum(diag(table(nn_test$left, nn_test$nn_prediction)))
nn_sum <- sum(table(nn_test$left, nn_test$nn_prediction))
accuracy <- nn_sum_diag/nn_sum
accuracy

# Visualization #
nn_roc_curve <- roc(nn_test$left ~ nn_test$net_pre_result)
nn_roc_curve
x_nn <- 1-roc_curve$specificities
y_nn <- roc_curve$sensitivities
nn_roc_plot <- plot(x=x_nn, y=y_nn, xlim=c(0,1), ylim=c(0,1),
                 xlab = "1-Specificity", ylab = "sensitivity", main = "ROC Curve for Neural Network")+
  theme_ipsum()
abline(a = 0, b = 1, col= "brown2")
nn_auc <- nn_roc_curve$auc
text(0.5, 0.4, paste('AUC:', round(nn_auc, digits = 4)),col = "cadetblue4")




