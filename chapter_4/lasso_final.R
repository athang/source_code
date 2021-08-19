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
# train_wine_orig  <- train_wine
# test_wine_orig <- test_wine

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
######## LASSO regression Tbshirani method #####################
# according to Tbshirani
# make the Lambda grid
grid =10^ seq (10,-2, length =100)
# plot the coefs.
lasso.mod =glmnet(x,y,alpha =1, lambda =grid)
plot(lasso.mod)

# make CV prediction with LASSO
set.seed (123)
cv.out =cv.glmnet (x,y,alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min; bestlam  # 0.0287145
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x2)
mse_lasso <- mean(( lasso.pred -y2)^2); mse_lasso

# finding the coefs of vars
out=glmnet(x,y,alpha =1, lambda =grid)
lasso.coef=predict (out ,type ="coefficients",s=bestlam )[1:9 ,]
lasso.coef
######## end of Tbshirani method ########
################################################################################
#### evaluation metrics etc #####
cv.out <- cv.glmnet(x, y, alpha = 1) ; 
plot(cv.out, main="Log(L) with lasso in train set")
min_lam_lasso <- cv.out$lambda.min ; min_lam_lasso # min lambda = 0.0238393
# best_lam_lasso <- cv.out$lambda.1se; best_lam_lasso # best lambda =  

lasso.mod <- glmnet(x, y, alpha = 1, lambda = min_lam_lasso)
lasso.pred <- predict(lasso.mod, s=min_lam_lasso, newx = x2)

plot(lasso.mod, "lambda", label = T, main="with Lasso")
legend("bottomright", lwd = 1, col = 1:9, legend = colnames(x), cex = .6)
# coef(cv.out,s=best_lam)
coef(cv.out,s=min_lam_lasso)

out <- glmnet(x, y, alpha = 1); # 
plot(out, label=T, main="Coefficients with lasso")
legend("bottomleft", lwd = 1, col = 1:9, legend = colnames(x), cex = .6)
# plot_glmnet(out, label = T, main="Coefficients with lasso")
# plot_glmnet(out, label = 8, main="Coefficients with lasso")

# find MSE
MSE3 <- mean((lasso.pred - test_wine$qual)^2); MSE3 # 1.9201
# coefficients
lasso.coef <- predict(lasso.mod, type = 'coefficients', s=min_lam_lasso)[1:9, ]; lasso.coef

#### looking the coefficients #####

# p1_bl <- predict(lasso.mod, type = 'coefficients', s=best_lam)[1:9, ]
# round(p1_bl, 4)

p1_ml <- predict(lasso.mod, type = 'coefficients', s=min_lam_lasso)[1:9, ]
round(p1_ml, 4)

# evaluation metrics
error_rate_lasso <- regr.eval(lasso.pred, test_wine$qual); error_rate_lasso

#### residuals with Lasso #####
lasso_resid <- lasso.pred - test_wine$qual
# checking normality
ks.test(lasso_resid, "pnorm", mean=mean(lasso_resid), sd=sd(lasso_resid))
hist(lasso_resid)
# YES they are normal 

#### prediction evaluation metrics ####
pr_ls <- lasso.pred
tr_ls <- test_wine$qual

pr_ls <- as.numeric(as.character(lasso.pred)) # predicted vector
tr_ls <- as.numeric(as.character(test_wine$qual)) # actual vector

regr.eval(pr_ls, tr_ls)
cor(pr_ls, tr_ls)
# pr_ls <- round(pr_ls, digits = 0)
mean(pr_ls == tr_ls)# same as Accuracy valid only for discrete values
# cor(pr_ls, tr_ls)
Accuracy(pr_ls, tr_ls)
actuals_preds <- data.frame(cbind(tr_ls, pr_ls)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy
