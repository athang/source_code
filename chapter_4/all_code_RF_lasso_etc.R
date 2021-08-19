# LASSO method
# setwd("~/PhD/2020 mar paper")
# setwd("~/giannis_diafora/PhD/2020 mar paper")
#### install various packages ####
#install.packages("descr")
library(descr) 
#install.packages("ggplot2")
library(ggplot2)
#install.packages("scales")
library(scales)
#install.packages("glmnet")
library(glmnet)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("ISLR")
library(ISLR)
#install.packages("DMwR")
library(DMwR)
#install.packages("outliers")
library(outliers)
#install.packages("StatMeasures")
library(StatMeasures)
#install.packages("foreach")
library(foreach)
# install.packages("MLmetrics")
library(MLmetrics)
# install.packages("plotmo")
library(plotmo)
# install.packages("olsrr")
library(olsrr)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("nortest")
library(nortest)
# install.packages("moments")
library(moments)
# install.packages("car")
library(car)
# install.packages("tseries")
library(tseries)
# install.packages("lmtest")
library(lmtest)
# install.packages("leaps")
library(leaps)
# install.packages("reshape2")
library(reshape2)
# install.packages("MASS")
library(MASS)
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages("goft")
library(goft)
# install.packages("RCurl")
library(RCurl)
# install.packages("glmnet")
library(glmnet)
# install.packages("corrplot")
library(corrplot)
# install.packages("ModelMetrics")
library(ModelMetrics)
# install.packages("lattice")
library(lattice)
# install.packages("caret")
library(caret)
# install.packages("plyr")
library(plyr)
# install.packages("readr")
library(readr)
# install.packages("repr")
library(repr)
# install.packages("foreign")
library(foreign)
# install.packages("robustbase")
library(robustbase)
# install.packages("robust")
library(robustbase)
# install.packages("sandwich")
library(sandwich)
# install.packages("robust")
library(robust)
# install.packages("psych")
library(psych)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
# install.packages("nnet")
library(nnet)
# install.packages("party")
library(party)
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
# install.packages("ROCR")
library(ROCR)
# install.packages("randomForest")
library(randomForest)
# install.packages("e1071")
library(e1071)
# install.packages("mda")
library(mda)
# install.packages("klaR")
library(klaR)
# install.packages("Metrics")
library(Metrics)
# install.packages("ellipse")
library(ellipse)
# install.packages("kernlab")
library(kernlab)

#### import the dataset ####

# we import the dataset of white wines 
mydataset.1 <- read.csv("tsant_white_new.csv", header=T, sep=",")
# we keep as _orig the original dataset
mydataset_orig <- mydataset.1
## mydataset <- mydataset_orig
str(mydataset.1)
summary(mydataset.1)
# boxplot(mydataset) #useless just for getting an image

#### creating training and testing sets ####
mydataset <- mydataset_orig
train_wine_orig  <- train_wine
test_wine_orig <- test_wine

set.seed(123)
# wine_final <- mydataset_orig
wine_final <- mydataset
t <- 1:nrow(wine_final)
index <- sample(t, round(nrow(wine_final)*.7))

train_wine <- wine_final[index,]
test_wine <- wine_final[-index,]

#### convert to numeric ####
train_wine$qual<- as.numeric(as.character(train_wine$qual))
test_wine$qual<- as.numeric(as.character(test_wine$qual))

#### creating x and y ####
x <- model.matrix(train_wine$qual ~., train_wine)[,-1] # trim off the first column
head(x); dim(x)
y <- train_wine$qual
head(y); dim(y); length(y)

x2 <- model.matrix(test_wine$qual ~., test_wine)[,-1]
y2 <- test_wine$qual
################################################################################
######## LASSO regression #####################
#### find the min & best lambda for Lasso #####
cv.out <- cv.glmnet(x, y, alpha = 1) ; 
plot(cv.out, main="Log(L) with lasso in train set")
min_lam_lasso <- cv.out$lambda.min ; min_lam_lasso # min lambda = 0.02871 (log = -3.5505)
best_lam_lasso <- cv.out$lambda.1se; best_lam_lasso # best lambda =  0.10562 (log = -2.2479)

lasso.mod <- glmnet(x, y, alpha = 1, lambda = min_lam_lasso)
# summary(lasso.mod)
lasso.pred <- predict(lasso.mod, s=min_lam_lasso, newx = x2)
# summary(lasso.pred)
# plotting
plot(lasso.mod, "lambda", label = T, main="with Lasso")
legend("bottomright", lwd = 1, col = 1:9, legend = colnames(x), cex = .6)
# coefficients 
coef(cv.out,s=best_lam_lasso)
coef(cv.out,s=min_lam_lasso)

out <- glmnet(x, y, alpha = 1); # model
# summary(out)
# plotting
plot(out, label=T, main="with lasso")
legend("bottomleft", lwd = 1, col = 1:9, legend = colnames(x), cex = .6)

# find MSE
MSE3 <- mean((lasso.pred - test_wine$qual)^2); MSE3 # 1.9201
# coefficients
lasso.coef <- predict(lasso.mod, type = 'coefficients', s=min_lam)[1:9, ]; lasso.coef

#### looking the coefficients #####

p1_bl <- predict(lasso.mod, type = 'coefficients', s=best_lam)[1:9, ]
round(p1_bl, 4)

p1_ml <- predict(lasso.mod, type = 'coefficients', s=min_lam)[1:9, ]
round(p1_ml, 4)
# evaluation metrics
error_rate_lasso <- regr.eval(lasso.pred, test_b2); error_rate_lasso

#### residuals with Lasso #####
lasso_resid <- lasso.pred - test_wine2$qual
# ridge_resid2 <- ridge.pred - test_b2
ks.test(lasso_resid, "pnorm", mean=mean(lasso_resid), sd=sd(lasso_resid))
hist(lasso_resid)

######## another approach ####
####create training and test data ####
set.seed(123)
wine_final <- mydataset
t <- 1:nrow(wine_final)
index <- sample(t, round(nrow(wine_final)*.7))

train_wine <- wine_final[index,]
test_wine <- wine_final[-index,]

####create training/test data specifically for glmnet ####
train_a <- train_wine[,2:9]
train_b <- train_wine[,1]
test_a <- test_wine[,2:9]
test_b <- test_wine[,1]

# We will use the package glmnet to build the models for ridge, 
# lasso and elastic net regression. We will then compare their 
# respective Root Mean Square Error for the test dataset.

################################################################################
#### mutinomial logistic LASSO ####
cv.out <- cv.glmnet(x,y,alpha=1,family="multinomial",type.measure = "mse" )
# plotting
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min; lambda_min
#best value of lambda
lambda_1se <- cv.out$lambda.1se; lambda_1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#### Hastie - Qian methods of multinomial lasso ####
fit <- glmnet(x, y, family = "multinomial", type.multinomial = "grouped")

plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")

cvfit <- cv.glmnet(x, y, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
plot(cvfit)
coef(cvfit, s=lambda_1se)

x2 <- model.matrix(test_wine$qual ~., test_wine)[,-1]
pr_lasso <- predict(cvfit, newx = x2, s = "lambda.min", type = "class")

#### prediction metrics ####

pr_ls <- as.numeric(as.character(pr_lasso)) # predicted vector
tr_ls <- as.numeric(as.character(test_wine$qual)) # actual vector

regr.eval(pr_ls, tr_ls)
cor(pr_ls, tr_ls)
# pr_ls <- round(pr_ls, digits = 0)
# mean(pr_ls == tr_ls) # same as Accuracy

Accuracy(pr_ls, tr_ls)
actuals_preds <- data.frame(cbind(tr_ls, pr_ls)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy
################################################################################
#### Step 1 - create the evaluation metrics function ####

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print("R^2:"); print(r2)
  print("adj.R^2:") ; print(adj_r2) #Adjusted R-squared
  print("RMSE:") ; print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

#### Step 2 - predicting and evaluating the model on train data ####
predictions = predict(lr, newdata = train)
eval_metrics(lr, train, predictions, target = 'unemploy')


#### Step 3 - predicting and evaluating the model on test data ####
predictions = predict(lr, newdata = test)
eval_metrics(lr, test, predictions, target = 'unemploy')
################################################################################
#### Compute R^2 from true and predicted values ####
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  #### Model performance metrics ####
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

#### Prediction and evaluation on train data ####
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

#### Prediction and evaluation on test data ####
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)

eval_results(tr_ls, pr_ls, test_wine)

################################################################################
######## LASSO ####
lasso_wine <- glmnet(x, y, alpha = 1, lambda = lambda) # model
#### predict lasso ####
lasso <- predict(lasso_wine, s = min_lam_lasso, newx = as.matrix(test_a2))
mse_lasso <- mean((test_b2 - lasso)^2); mse_lasso
lasso_error <- sqrt(mse_lasso); lasso_error
# evaluation metrics
regr.eval(lasso,test_b2)
#### plot residuals ####
lasso_resid <- test_b2 - lasso
plot(lasso_resid, main="residuals with lasso")
abline(h=c(-2,0,2),lty=c(2,1,2))

#### checking number of residuals outside of (-2,2) #####

over <- sum(lasso_resid > 2) ; over
under <- sum(lasso_resid < - 2) ; under
(over+under)/length(lasso_resid) # outside of bounds in % 0.13
#### graphics ####
qplot(lasso_resid, geom="histogram", binwidth = 1,
      main = "Residuals with lasso", xlab = "scores", ylab = "observations", 
      fill=I("blue"), col=I("red"),alpha=I(.2))

#### checking normality of residuals ####
ks.test(lasso_resid, "pnorm", mean=mean(lasso_resid), sd=sd(lasso_resid))

########RANDOM FOREST AND DECISION TREES #######################################
#### dividing and creating training and testing sets ####
mydataset <- mydataset_orig

wine_final <- mydataset.1
set.seed(123)

t <- 1:nrow(wine_final)
index <- sample(t, round(nrow(wine_final)*.7))

train_wine <- wine_final[index,]
test_wine <- wine_final[-index,]

# names(train_wine)
train_wine <- train_wine[ , c(1:5,9)]
test_wine <- test_wine[ , c(1:5,9)]

################################################################################
######## decision trees (with rpart package) ########

# convert to factors
# mydataset$qual <- as.factor(mydataset$qual)
# train_wine$qual <- as.factor(train_wine$qual)
# test_wine$qual <- as.factor(test_wine$qual)

fitnew <- rpart(qual ~., data = train_wine, method = "anova")
# plotting
rpart.plot(fitnew)

predict_unseen <-predict(fitnew, test_wine)
predict_unseen2 <- round(predict_unseen, digits = 0)
#### prediction table of DT - evaluation####
table_mat1 <- table(test_wine$qual, predict_unseen2) 
table_mat1
mean(test_wine$qual == predict_unseen2)

test_qual <- as.numeric(as.character(test_wine$qual))
# evaluation metrics
regr.eval(test_qual, predict_unseen2)
cor(test_qual, predict_unseen2)

actual <- test_wine$qual; preds <- predict_unseen2 
actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

#### plotting DT ####
ggplot(actuals_preds, aes(x=actual, y=predict_unseen2)) + 
  geom_count(aes(size=..prop..), colour="red") +
  geom_count(aes(size=..prop.., group=predict_unseen2), colour="black")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2)) +
  coord_fixed() +
  theme_minimal()

#### another approach of DT and plotting ####
mtree <- rpart(qual~., data = train_wine, method="anova", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
plot(mtree)
text(mtree)
prp(mtree, faclen = 0, cex = 0.8, extra = 1)

tree_fitnew <- ctree(qual ~ alc, data = train_wine)
plot(tree_fitnew)
################################################################################
######## random forest method (with randomForest package) ####
rf1 <- randomForest(qual ~., data = train_wine, ntree = 500, mtry = 2, importance = TRUE)
# printing the results   
print(rf1)

pred_r <- predict(rf1, newdata=test_wine[-1])
pred_r <- round(pred_r, digits = 0)
cm1 = table(test_wine[,1], pred_r); cm1  # table of cross validation
round((sum(diag(cm1))/sum(cm1))*100,4)
mean(pred_r == test_wine$qual)

#### evaluation of metrics####
# converting to numeric
pr_rf <- as.numeric(as.character(pred_r))
tr_rf <- as.numeric(as.character(test_wine$qual))

regr.eval(pr_rf, tr_rf)
cor(pr_rf, tr_rf); print("R^2:"); cor(pr_rf, tr_rf)^2
# pr_rf <- round(pr_rf, digits = 0)
mean(pr_rf == tr_rf)
# cor(pr_rf, tr_rf)
Accuracy(pr_rf, tr_rf) # same as the mean...

#### metrics ####
actuals_preds <- data.frame(cbind(tr_rf, pr_rf)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

eval_results(tr_rf, pr_rf, test_wine)

#### running the RF model with 10-fold CV (with CARET package) ####
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
set.seed(123)
# # Run the model
# # convert to factor the dependent 
# mydataset$qual <- as.factor(mydataset$qual)
train_wine$qual <- as.factor(train_wine$qual)
test_wine$qual <- as.factor(test_wine$qual)

rf_default <- train(qual~.,
                    data = train_wine,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default) # finding the best mtry (here is 2)
# prediction
pred_values1 <- predict(rf_default, test_wine)
table_mat1 <- table(test_wine$qual, pred_values1) 
table_mat1
round((sum(diag(table_mat1))/sum(table_mat1))*100,4)

# convert to numeric
tr_rf_1 <- as.numeric(as.character(test_wine$qual))
pr_rf_1 <- as.numeric(as.character(pred_values1))

regr.eval(pr_rf_1, tr_rf_1)
cor(pr_rf_1, tr_rf_1)
# pr_rf_2 <- round(pr_rf_2, digits = 0)
# mean(pr_rf_1 == tr_rf_1) # accuracy
# cor(pr_rf_1, tr_rf_1)
Accuracy(pr_rf_1, tr_rf_1)
#### metrics ####
actuals_preds1 <- data.frame(cbind(tr_rf_1, pr_rf_1)) ; dim(actuals_preds1)
min_max_accuracy <- mean(apply(actuals_preds1, 1, min) / apply(actuals_preds1, 1, max)); 
min_max_accuracy

#### running other RF model for finding mtry (ntree=300, nodesize=14)####
set.seed(123)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(qual~.,
                 data = train_wine,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry) # finding best mtry (here is 6)
#### predicting####
pred_values2 <- predict(rf_mtry, test_wine)

#### evaluation####
table_mat2 <- table(test_wine$qual, pred_values2) 
table_mat2
round((sum(diag(table_mat2))/sum(table_mat2))*100,4)

# convert to numeric
tr_rf_2 <- as.numeric(as.character(test_wine$qual))
pr_rf_2 <- as.numeric(as.character(pred_values2))

regr.eval(pr_rf_2, tr_rf_2)
cor(pr_rf_2, tr_rf_2)
# pr_rf_2 <- round(pr_rf_2, digits = 0)
# mean(pr_rf_2 == tr_rf_2)
# cor(pr_rf_2, tr_rf_2)
Accuracy(pr_rf_2, tr_rf_2)
#### metrics ####
actuals_preds2 <- data.frame(cbind(tr_rf_2, pr_rf_2)) ; dim(actuals_preds2)
min_max_accuracy <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max)); 
min_max_accuracy
#################################
# mymeans <- c(mean(mydataset$qual),mean(mydataset$alc))
# barplot(mymeans, ylim = c(0,12))
# myt <- table(mydataset$qual)
# barplot(myt)
################################################################################
########COMPARING FIVE ML METHODS ##############################################
#### defining x and y####
x <- mydataset5[,2:9]
y <- mydataset5[,1]

#### some graphics ####
# par(mfrow = c(1,1))
# scatterplot matrix
# featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

#### density plots for each attribute by class value####
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#### another way of creating training and testing set####
# # create a list of 70% of the rows in the original dataset we can use for training
# validation_index <- createDataPartition(dataset$qual, p=0.70, list=FALSE) # using caret Package
# # select 30% of the data for validation
# testing.set <- mydataset[-validation_index,]
# # use the remaining 70% of data to training and testing the models
# training.set <- mydataset[validation_index,]

#### boxplot for each attribute on one image####
# par(mfrow=c(1,4))
# for(i in 1:4) {
#   boxplot(x[,i], main=names(mydataset)[i])
# }

#### Run algorithms using 10-fold cross validation with the Accuracy metric####
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

mydataset$qual <- as.factor(mydataset$qual)

#### testing the models ####
# a) linear algorithms
set.seed(123)
fit.lda <- train(qual~., data=train_wine, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(123)
fit.cart <- train(qual~., data=train_wine, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(123)
fit.knn <- train(qual~., data=train_wine, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(123)
fit.svm <- train(qual~., data=train_wine, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(123)
fit.rf <- train(qual~., data=train_wine, method="rf", metric=metric, trControl=control)

#### summarize accuracy of models####
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# anova(fit.lda, fit.cart, fit.knn, fit.svm, fit.rf) # not working

#### compare accuracy of models graphically####
dotplot(results)

#### summarize Best Model####
print(fit.rf)

#### estimate skill of RF on the testing dataset####
predictions <- predict(fit.rf, test_wine)
confusionMatrix(predictions, test_wine$qual)

#### prediction evaluation metrics ####
# converting to numeric
t1 <- as.numeric(as.character(test_wine$qual))
p1 <- as.numeric(as.character(predictions))

regr.eval(p1, t1)

pred_val_round <- round(p1, digits = 0)
sum(pred_val_round == t1 , na.rm = T) / nrow(test_wine) *100
mean(pred_val_round == t1)
cor(pred_val_round, t1)
# MIN - MAX ACCURACY
actuals_preds <- data.frame(cbind(actuals=t1 , predicteds=p1))
min_max_accuracy <- mean(apply(actuals_preds,1,min)/apply(actuals_preds,1,max))
min_max_accuracy
cor(p1, t1)

