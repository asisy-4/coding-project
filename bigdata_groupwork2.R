# in this work we will use supervised machine learning to build a model (using training set) to make prediction (based on test set) of the rating customers will give

# Clear Global Environment
rm(list = ls())
getwd()
setwd("/Users/a1/Documents/2021/finance & big data/groupwork02") # need reset wd if you want to run code in your own computer!

# install & Library packages
pacman::p_load(dplyr, lattice, ggplot2, quanteda, quanteda.textmodels, remotes, caret) 
install.packages("remotes")
install.packages("stylo")
install.packages("tidyverse")
library(tidyverse)
library(stylo)

## for prepare data
install.packages("magrittr")
install.packages("quanteda")
install.packages("fastmatch")
library(magrittr)
library(quanteda)
library(fastmatch)
library(methods)

## for naive bayes
library("lattice")
library("ggplot2")
install.packages('quanteda.textmodels')
library(quanteda.textmodels)
install.packages("pROC")
library(pROC)
install.packages("caret")
library(caret)

## 1.Prepare Data ##

devtools::install_github("kbenoit/quantedaData")
library(quantedaData)
install.packages("NLP")
library(NLP)
install.packages("tm")
library(tm)
install.packages('data.table')
library(data.table)

# read csv
train_set <- read.csv("train.csv",header=F, stringsAsFactors=T)
test_set <- read.csv("test.csv",header=F, stringsAsFactors=T)
# deal with assessment
train_set$V1 <- ifelse(train_set$V1!=5, 0,1)
test_set$V1 <- ifelse(test_set$V1!=5, 0,1)

# merge
merge_dframe <- rbind(x=train_set,y=test_set)

#sample
id <- c(1:700000)
merge_dframe <- cbind(merge_dframe,id)
id_sample <- sample(id,6000,replace=F) 
sample_dframe <- merge_dframe[id_sample,]
sample_dframe <- sample_dframe[,-3] #去掉第三列

# Change the columns names of the data frame
names(sample_dframe) <- c("Assessment","Comment")

# Change the "Assessment" into type "factor"
sample_dframe$Assessment <- as.factor(sample_dframe$Assessment)

# original corpus
#dirty_cp <- VCorpus(VectorSource(sample_dframe$Assessment))

dirty_cp <- Corpus(VectorSource(sample_dframe$Comment))

#clean data

dirty_cp <- tm_map(dirty_cp,removeNumbers)
dirty_cp <- tm_map(dirty_cp,removePunctuation)
dirty_cp <- tm_map(dirty_cp,content_transformer(tolower))
dirty_cp <- tm_map(dirty_cp,removeWords,stopwords("english")) #去停用词
dirty_cp <- tm_map(dirty_cp,stripWhitespace)
dirty_cp <- tm_map(dirty_cp,stemDocument)
clean_cp <- dirty_cp


# Change corpuses into data frames and add the label "Assessment"
clean_dframe <- data.frame(text=sapply(clean_cp , identity),stringsAsFactors=F)
clean_dframe$Assessment <- sample_dframe$Assessment  


# Change the columns names of the data frame
names(clean_dframe) <- c("Comment","Assessment")

# Change the data frames into dfm and add the label "Assessment"
dfmat <- dfm(tokens(clean_dframe$Comment),remove_punct=TRUE) %>% dfm_select(pattern=("*"))
docvars(dfmat,"Assessment") <- clean_dframe$Assessment

# Shuffle the rows to randomize the order 
set.seed(1234)
id_train <- sample(1:6000,4500, replace=F) #在1-6000里随机抽取4500个数，且相互不重复
head(id_train, 10) #截取上述4500个数里的前十个

# Use the 4500 for a training set and the other 1500 as your test set. Create dfms for each
docvars(dfmat, "id_numeric") <- 1:ndoc(dfmat)
dfmat_train <- dfm_subset(dfmat, docvars(dfmat,"id_numeric") %in% id_train)
dfmat_test <- dfm_subset(dfmat, !(docvars(dfmat,"id_numeric") %in% id_train))


## 2.Classification Naive Bayes ##

# Train the naive bayes model
sentmod.nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "Assessment"), distribution = "Bernoulli")
summary(sentmod.nb)

# Prepare the test data
dfmat_matched <- dfm_match(dfmat_test, features=featnames(dfmat_train))

# Save the actual data
actual_class <- docvars(dfmat_matched, "Assessment")

# Predict with the test set and show the confusionmatrix
predicted_value.nb <- predict(sentmod.nb, newdata=dfmat_matched, type="probability")
predicted_class.nb <- predict(sentmod.nb, newdata=dfmat_matched)
tab_class <- table(actual_class,predicted_class.nb)
tab_class

# Show the assessment of confusionmatrix
confusionMatrix(tab_class, mode="everything")

# Let’s do some sniff tests. What are the most positive and negative words?
## Most positive words
sort(sentmod.nb$param[2,],dec=T)[1:20]

## Most negative words
sort(sentmod.nb$param[2,],dec=F)[1:20]

# Let’s get a birds-eye view
## Plot weights
plot(colSums(dfmat_train),sentmod.nb$param[2,], pch=19, col=rgb(0,0,0,.3), cex=.5, main="Posterior Probabilities, Naive Bayes Classifier, Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances")
text(colSums(dfmat_train),sentmod.nb$param[2,], colnames(dfmat_train),pos=4,cex=5*abs(.5-sentmod.nb$param[2,]), col=rgb(0,0,0,1.5*abs(.5-sentmod.nb$param[2,])))


# Look a little closer at the negative
## Plot weights
plot(colSums(dfmat_train),sentmod.nb$param[2,], pch=19, col=rgb(0,0,0,.3), cex=.5, main="Posterior Probabilities, Naive Bayes Classifier, Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim=c(10,1000),ylim=c(0,.05))
text(colSums(dfmat_train),sentmod.nb$param[2,], colnames(dfmat_train),pos=4,cex=5*abs(.5-sentmod.nb$param[2,]), col=rgb(0,0,0,1.5*abs(.5-sentmod.nb$param[2,])))

# And a little more closely at the positive words:
## Plot weights
plot(colSums(dfmat_train),sentmod.nb$param[2,], pch=19, col=rgb(0,0,0,.3), cex=.5, main="Posterior Probabilities, Naive Bayes Classifier, Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim=c(10,1000),ylim=c(.05,.2))
text(colSums(dfmat_train),sentmod.nb$param[2,], colnames(dfmat_train),pos=4,cex=5*abs(.5-sentmod.nb$param[2,]), col=rgb(0,0,0,1.5*abs(.5-sentmod.nb$param[2,])))

# Let’s look a little more closely at the document predictions
predicted_prob <- predict(sentmod.nb, newdata=dfmat_matched, type="probability")
dim(predicted_prob)
head(predicted_prob)
summary(predicted_prob)

# What’s the most positive/negative review in the test set according to this?
## sort by *least negative* since near zero aren't rounded
sort.list(predicted_prob[,1], dec=F)[1]
id_test <- !((1:6000) %in% id_train)
as.character(dfmat)[id_test][429]

## sort by *least neg* since near zero aren't rounded
sort.list(predicted_prob[,2], dec=F)[1]
as.character(dfmat)[id_test][233]

# Let’s look at closer
sort.list(abs(predicted_prob - .5), dec=F)[1]
predicted_prob[212,]



## 3.Classification ridge ##

install.packages("glmnet")
install.packages("foreach")
install.packages("iterators")
install.packages("parallel")
library(glmnet)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)

cl = makeCluster(4)
registerDoParallel(cl) # parallelize to speed up

sentmod.ridge <- cv.glmnet(x=dfmat_train,
                           y=docvars(dfmat_train)$Assessment,
                           family="binomial", 
                           alpha=0,  # alpha = 0: ridge regression
                           nfolds=5, # 5-fold cross-validation
                           parallel=TRUE, 
                           intercept=TRUE,
                           type.measure="class")
plot(sentmod.ridge)

# Let us look at the performance
predicted_value.ridge <- predict(sentmod.ridge, newx=dfmat_matched,s="lambda.min")[,1]
predicted_class.ridge <- rep(NA,length(predicted_value.ridge))
predicted_class.ridge[predicted_value.ridge>0] <- "1"
predicted_class.ridge[predicted_value.ridge<0] <- "0"
tab_class.ridge <- table(actual_class,predicted_class.ridge)
tab_class.ridge

confusionMatrix(tab_class.ridge, mode="everything")

# plots
plot(colSums(dfmat_train),coef(sentmod.ridge)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Ridge Regression Coefficients, Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),coef(sentmod.ridge)[-1,1], colnames(dfmat_train),pos=4,cex=200*abs(coef(sentmod.ridge)[-1,1]), col=rgb(0,0,0,75*abs(coef(sentmod.ridge)[-1,1])))

plot(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Ridge Regression Coefficients (Impact Weighted), Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1], colnames(dfmat_train),pos=4,cex=50*abs(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1]), col=rgb(0,0,0,25*abs(log(colSums(dfmat_train))*coef(sentmod.ridge)[0,1])))

# Most positive and negative features by impact
sort(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1],dec=T)[1:20]
sort(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1],dec=F)[1:20]

## 4.Classification lasso ##

# LASSO (Logistic with L1-regularization)
sentmod.lasso <- cv.glmnet(x=dfmat_train,
                           y=docvars(dfmat_train)$Assessment,
                           family="binomial", 
                           alpha=1,  # alpha = 1: LASSO
                           nfolds=5, # 5-fold cross-validation
                           parallel=TRUE, 
                           intercept=TRUE,
                           type.measure="class")

# Tuning: To find out the best lambda
plot(sentmod.lasso)

## calculate the minimal MSE
min(sentmod.lasso$cvm)

## the corresonding log(lambda)
log(sentmod.lasso$lambda.min)

## within 1 SE of minimum MSE
sentmod.lasso$cvm[sentmod.lasso$lambda == sentmod.lasso$lambda.1se]

## the largest log(lambda) for within the range of 1 SE of the minimal MSE
sentmod.lasso$lambda.1se

# performance
predicted_value.lasso <- predict(sentmod.lasso, newx=dfmat_matched,s="lambda.min")[,1]
predicted_class.lasso <- rep(NA,length(predicted_value.lasso))
predicted_class.lasso[predicted_value.lasso>0] <- "1"
predicted_class.lasso[predicted_value.lasso<0] <- "0"
tab_class.lasso <- table(actual_class,predicted_class.lasso)
tab_class.lasso
confusionMatrix(tab_class.lasso, mode="everything")


# plots
plot(colSums(dfmat_train),coef(sentmod.lasso)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="LASSO Coefficients, Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim = c(1,20000))
text(colSums(dfmat_train),coef(sentmod.lasso)[-1,1], colnames(dfmat_train),pos=4,cex=2*abs(coef(sentmod.lasso)[-1,1]), col=rgb(0,0,0,1*abs(coef(sentmod.lasso)[0,1])))

plot(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="LASSO Coefficients (Impact Weighted), Yelp", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1], colnames(dfmat_train),pos=4,cex=.8*abs(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1]), col=rgb(0,0,0,.25*abs(log(colSums(dfmat_train))*coef(sentmod.lasso)[0,1])))

# Most positive and negative features by impact
sort(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1],dec=T)[1:20]
sort(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1],dec=F)[1:20]



## 5.Classification Elastic Net ##


# The elastic net estimates not just λ (the overall amount of regularization) but also α (the relative weight of the L1 loss relative to the L2 loss)

# maintain the same folds across all models
fold_id <- sample(x = 1:10, size = length(docvars(dfmat_train)$Assessment), replace = TRUE)

# search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)

for(i in seq_along(tuning_grid$alpha)){
  # fit CV model for each alpha value
  fit <- cv.glmnet(x=dfmat_train,
                   y=docvars(dfmat_train)$Assessment,
                   family="binomial", 
                   alpha = tuning_grid$alpha[i],
                   foldid= fold_id,
                   nfolds=5, # 5-fold cross-validation
                   parallel=TRUE, 
                   intercept=TRUE,
                   type.measure="class")
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

tuning_grid

tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>% # calculate the distance of 1 SE
  ggplot(aes(alpha, mse_min)) + # under different alpha, plot the minimal MSE under cv
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")

sentmod.en <- cv.glmnet(x=dfmat_train,
                        y=docvars(dfmat_train)$Assessment,
                        family="binomial", 
                        alpha=0.1,  
                        nfolds=5, # 5-fold cross-validation
                        parallel=TRUE, 
                        intercept=TRUE,
                        type.measure="class")


# performance
predicted_value.en <- predict(sentmod.en, newx=dfmat_matched,s="lambda.min")[,1]
predicted_class.en <- rep(NA,length(predicted_value.en))
predicted_class.en[predicted_value.en>0] <- "1"
predicted_class.en[predicted_value.en<0] <- "0"
tab_class.en <- table(actual_class,predicted_class.en)
tab_class.en

confusionMatrix(tab_class.en, mode="everything")

sort(log(colSums(dfmat_train))*coef(sentmod.en)[-1,1],dec=T)[1:20]
sort(log(colSums(dfmat_train))*coef(sentmod.en)[-1,1],dec=F)[1:20]



## 6.Classification  Support Vector mechine ##

install.packages("e1071")
library(e1071)

sentmod.svm <- svm(x=dfmat_train,
                   y=as.factor(docvars(dfmat_train)$Assessment),
                   kernel="linear", 
                   cost=10,  # arbitrary regularization cost
                   probability=TRUE)

# Ideally, we would tune the cost parameter via cross-validation or similar, as we did with λ above
predicted_class.svm <- predict(sentmod.svm, newdata=dfmat_matched)
tab_class.svm <- table(actual_class,predicted_class.svm)
tab_class.svm

# That’s actually a bit better than the others, individually if not combined, with accuracy of .834, and a bias toward overpredicting positives
confusionMatrix(tab_class.svm, mode="everything")

## 7.Classification  Random Forests ##

install.packages("randomForest")
library(randomForest)

dfmat.rf <- corpus %>%
  dfm(x=dfmat_train) %>%
  dfm_trim(min_docfreq=50,max_docfreq=300,verbose=TRUE)

dfmatrix.rf <- as.matrix(dfmat.rf)

set.seed(1234)
sentmod.rf <- randomForest(dfmatrix.rf[id_train,], 
                           y=as.factor(docvars(dfmat.rf)$Assessment)[id_train],
                           xtest=dfmatrix.rf[id_test,],
                           ytest=as.factor(docvars(dfmat.rf)$Assessment)[id_test],
                           importance=TRUE,
                           mtry=30,
                           ntree=150
)

predicted_class.rf <- sentmod.rf$test[['predicted']]
tab_class.rf <- table(actual_class,predicted_class.rf)
tab_class.rf
# That did a bit worse – Accuracy .776 – but we did give it considerably less information
confusionMatrix(tab_class.rf, mode="everything")

# Getting marginal effects from a random forest model requires more finesse than I’m willing to apply here. We can get the “importance” of the different features, but this alone does not tell us in what direction the feature pushes the predictions
varImpPlot(sentmod.rf)


## 8.Classification  Ensemble ##


predicted_class.ensemble5 <- rep("neg",length(actual_class))
num_predicted_pos5 <- 1*(predicted_class=="pos") + 1*(predicted_class.ridge=="pos") + 1*(predicted_class.lasso=="pos") + 
  1*(predicted_class.svm=="pos") + 
  1*(predicted_class.rf=="pos")
predicted_class.ensemble5[num_predicted_pos5>2] <- "pos"
tab_class.ensemble5 <- table(actual_class,predicted_class.ensemble5)
tab_class.ensemble5

# And like magic, now we’re up to 86.2% Accuracy in the test set
confusionMatrix(tab_class.ensemble5,mode="everything")



library(devtools)
install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
library(h2o)
h2o.init(nthreads = -1)
h2o.removeAll() 

# Convert to df then h2o
dfmat_train_df  <- as.data.frame(dfmat_train) # this will dispatch quanteda::as.data.frame for dfmSparse
dfmat_train_df$Assessment <- as.factor(docvars(dfmat_train)$Assessment)
dfmat_train_h2o <- as.h2o(dfmat_train_df)

y <- dfmat_train_h2o[42469]
x <- dfmat_train_h2o[1:42469]


ensemble_tree <- h2o.stackedEnsemble(
  x=x,
  y=y,
  training_frame = dfmat_train_h2o, model_id = "my_tree_ensemble",
  base_models = list(sentmod.nb, sentmod.lasso, sentmod.ridge, sentmod.svm),
  metalearner_algorithm = "drf"
)



