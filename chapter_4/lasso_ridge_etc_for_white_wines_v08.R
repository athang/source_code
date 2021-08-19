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

#### import the dataset #####################

# we import the dataset of white wines 
mydataset.1 <- read.csv("tsant_white_new.csv", header=T, sep=",")
# we keep as _orig the original dataset
mydataset_orig <- mydataset.1
## mydataset <- mydataset_orig
str(mydataset.1)
summary(mydataset.1)
# boxplot(mydataset) #useless just for getting an image

#### checking with backward, forward, stepwise methods ####
y <- mydataset.1$qual
FitAll <- lm(y~., data=mydataset.1[ ,-1])
FitStart <- lm(y~1, data=mydataset.1[ ,-1])
step(FitStart, direction="forward", scope=formula(FitAll)) #forward
step(FitAll, direction="backward", trace=F) #backward
step(FitStart, direction="both", scope=formula(FitAll)) #stepwise

library(MASS)
step.model <- stepAIC(FitAll, direction = "both", trace = F)
summary(step.model)

#### second approach ####
library(leaps)
models <- regsubsets(y~., data = mydataset.1[ , -1], nvmax = 8, method = "seqrep")
# [seqrep, backward, forward]
summary(models)
plot(models, scale="bic")
summary(models)$which[which.min(summary(models)$bic), ]
#stepwise
regfit.full <- regsubsets(y~., data = mydataset.1[ , -1], nvmax = 8) 
summary(regfit.full)
summary(regfit.full)$which[which.min(summary(regfit.full)$bic), ]
plot(regfit.full, scale="bic")
#forward
regfit.fwd <- regsubsets(y~., data = mydataset.1[ , -1], nvmax = 8, method ="forward") 
summary(regfit.fwd)
summary(regfit.fwd)$which[which.min(summary(regfit.fwd)$bic), ]
#backward
regfit.bwd <- regsubsets(y~., data = mydataset.1[ , -1], nvmax = 8, method ="backward") 
summary(regfit.bwd)
summary(regfit.bwd)$which[which.min(summary(regfit.bwd)$bic), ]

#### outliers section 1 ####

# ltOutliers <- outliers(vector = mydataset$fsd); ltOutliers # values of outliers
# numOutliers <- ltOutliers$numOutliers; numOutliers # number of outliers
# idxOutliers <- ltOutliers$idxOutliers; idxOutliers # values of outliers
## valOutliers <- scores[idxOutliers]

#### counting outliers of each variable ####

      ##### finding with iteration all median of vars
      # output <- " "
      # output <- vector("double", ncol(mydataset)); output  # 1. output
      # for (i in seq_along(mydataset)) {                    # 2. sequence
      #   name[[i]] <- names(mydataset)[[i]]
      #   output[[i]] <- median(mydataset[[i]])              # 3. body
      # }
      # name
      # output
      # summary(mydataset)

      ##### finding with iteration all lengths of outliers of vars
      output <- " "
      output <- vector("double", ncol(mydataset)); output           # 1. output
      for (i in seq_along(mydataset)) {                             # 2. sequence
        name[[i]] <- names(mydataset)[[i]]
        output[[i]] <- length(boxplot.stats(mydataset[[i]])$out)    # 3. body
      }
      name
      output

# length(boxplot.stats(mydataset$tsd)$out)
# length(boxplot.stats(mydataset$fsd)$out)
# length(boxplot.stats(mydataset$col)$out)
# length(boxplot.stats(mydataset$sug)$out)
# length(boxplot.stats(mydataset$va)$out)
# length(boxplot.stats(mydataset$ta)$out)
# length(boxplot.stats(mydataset$ph)$out)
# length(boxplot.stats(mydataset$alc)$out)
# length(boxplot.stats(mydataset$qual)$out)

# r <- boxplot.stats(mydataset$sug) ; r
# length(r$out) # number of outliers

y1 <- na.omit(mydataset$qual) # define the dependent variable
summary(y1); str(y1); hist(y1)
##table(y1)
##par(mfrow = c(1,1))


#### outliers section 2 #######

# out_val <- boxplot(y1, main="sug")$out ; out_val    # boxplot and values
# y2 <- which(y1 %in% out_val); y2 ; length(y2)    # show all the outliers index
## y3 <- y1[ (y1 %in%out_val) ] ; y3 ; length(y3)   # show the outliers

# source("http://goo.gl/UUyEzD")
# outlierKD(mydataset, y1)    # outlier removal and replace them with NA
# par(mfrow = c(1,1))


#### removing all outliers ####

outliers <- boxplot(mydataset$sug, plot=FALSE)$out
newset <- mydataset
newset <- newset[-which(newset$sug %in% outliers),] # removing outliers
str(newset)
boxplot(newset)
# x<- newset$fsd
# x<- newset
# create a new vector without outliers
# newx <- x[!x %in% boxplot.stats(x)$out]; length(newx) 
# boxplot(newx)
# from outlier library
rm.outlier(newset$fsd, fill = FALSE, median = FALSE, opposite = FALSE)
str(newset$fsd)
boxplot(newset$fsd)

#### histograms ########

# show the quality histogram
hist(mydataset$qual, main = "Quality of white wines with frequency", 
     breaks=10,  xlab ='Quality', col="gray")

d <- density(mydataset$qual)
plot(d, main="Quality Density of white wines")
polygon(d, col="red", border="blue")

m <- ggplot(mydataset, aes(x = qual))   
m + geom_histogram( bins = 8) 
m + geom_freqpoly(bins = 8) 

ggplot(mydataset, aes(x = qual)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) + 
  geom_density()

# with percentages
truehist(mydataset$qual, h = 1, col="slategray3", 
         main = "Quality of white wines with percentages") 

qplot(mydataset$qual, geom="histogram", binwidth = 1,
      main = "Quality", xlab = "scores", ylab = "observations", 
      fill=I("blue"), col=I("red"),alpha=I(.2))

boxplot.stats(mydataset$qual); summary(mydataset$qual); boxplot(mydataset$qual)
boxplot.stats(mydataset$fsd); summary(mydataset$fsd); 
boxplot(mydataset$fsd) ; "number of outliers:";length(boxplot.stats(mydataset$fsd)$out)
### memory.limit(size=4095) # for windows OS only
### memory.size() # for windows OS only
# ls()
# gc() # view the memory limits
boxplot(mydataset$qual, main="quality")

#### correlation of dataset and graphics ####

mycor <- round(cor(mydataset),3);  round(mycor,3)

pairs(mydataset) #1st WAY 
pairs.panels(mydataset)#2nd WAY
chart.Correlation(mydataset, histogram = T, pch = 19) #3rd WAY
mycor <- cor(mydataset) #4th WAY
corrplot(mycor, type = "upper", method = "pie")
plot_matrix <- function(matrix_toplot){
  corrplot.mixed(matrix_toplot,
                 order = "original", 
                 tl.col='black', tl.cex=.50)
}
plot_matrix(cor(mydataset)) #5th WAY

#### boxplots of all vars #####
      oldpar = par(mfrow = c(2,4))
      for ( i in 2:9 ) {
        boxplot(mydataset[[i]])
        mtext(names(mydataset)[i], cex = 0.8, side = 1, line = 2)
      }
      par(mfrow=c(1,1))

#### showing all histograms of vars #####
      oldpar = par(mfrow = c(3,3))
      for ( i in 1:9 ) {
        truehist(mydataset[[i]], xlab = names(mydataset)[i], col = 'lightgreen', main = paste("Mean =", signif(mean(mydataset[[i]]),3)))
      }
      par(mfrow=c(1,1))

#### showing all outliers #####
        outliers = c()
        for ( i in 1:9 ) {
          stats = boxplot.stats(mydataset[[i]])$stats
          bottom_outlier_rows = which(mydataset[[i]] < stats[1])
          top_outlier_rows = which(mydataset[[i]] > stats[5])
          outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
          outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% outliers ] )
        }
        # outliers; 
        length(outliers)
        length(outliers)/length(mydataset$qual) # percentage of outliers
#### creating training and testing sets ####
    mydataset <- mydataset_orig
    # mydataset$qual <- mydataset_orig$qual
    # first make factor the quality for logistic regression
    # mydataset$qual <- as.factor(mydataset$qual)
#### dataset without excluded vars #############################################
    # new mydataset without unselected variables
    # mydataset <- mydataset[ , c(1:3,5:7,9)]
    # str(mydataset)
    set.seed(123)
    # wine_final <- mydataset_orig
    wine_final <- mydataset
    t <- 1:nrow(wine_final)
    index <- sample(t, round(nrow(wine_final)*.7))
    
    train_wine <- wine_final[index,]
    test_wine <- wine_final[-index,]

    # x<-mydataset
    # y<-mydataset$qual
    # set.seed(123)
    # train <- sample(1:nrow(x), nrow(x)*0.7) # indexes of train set 70% randomly
    # # test <- mydataset[-train, ] # creation of test dataset with the rest 30%
    # # summary(test)
    # test <- (-train)
    # ytest <- y[test] ; ## ytest
    # # ytest <- y[-train] # vector of only dep.var. from train set

################################################################################
######## making multiple linear regression (OLS) ###################
mydataset <- mydataset.1   
    # checking normality of dependent variable qual
    ols_test_normality(mydataset$qual)# NOT NORMAL
    # multiple linear regression in training set
    
    mylm <- lm(qual ~ . , data=train_wine) 
    p0 <- round(coef(mylm), 4); p0 # with OLS
    summary(mylm)
    #### predicting in OLS ####
    pred_wine_ols <- predict(mylm, test_wine) # predicting in testing set
    #### evaluation metrics    ####
    regr.eval(test_wine$qual, pred_wine_ols) # metrics
    cor(test_wine$qual, pred_wine_ols)
    
    actual <- test_wine$qual; preds <- pred_wine_ols
    actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
    min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
    min_max_accuracy #may give Inf
    
    # plot(mylm)
    # plot(mylm,4) # for cook's distance 
    
    #### checking residuals and normality of residuals ####
    ols_plot_resid_hist(mylm)
    title(main="Residuals with OLS")
    skewness(mylm$residuals)
    kurtosis(mylm$residuals)
    summary(mylm$residuals); boxplot(mylm$residuals)
    
    ks.test(mylm$residuals, "pnorm", mean=mean(mylm$residuals), sd=sd(mylm$residuals))
    ols_test_normality(mylm$residuals)
    #### checking residuals removing Cook's distance outliers######
    
    cooksd = cooks.distance(mylm) ### cook's distance
    plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")
    abline(h = 4*mean(cooksd, na.rm = T), col = "red")
    
    head(mydataset[cooksd > 4 * mean(cooksd, na.rm=T), ])
    str(mydataset[cooksd > 4 * mean(cooksd, na.rm=T), ])
    #### remove all the outliers greater than 4 times cook's distance #####
    coutliers = as.numeric(rownames(mydataset[cooksd > 4 * mean(cooksd, na.rm=T), ]))
    outliers = c(outliers , coutliers[ !coutliers %in% outliers ] )
    
    cleanWhiteDat = mydataset[-outliers, ]
    oldpar = par(mfrow=c(3,3))
    for ( i in 1:9 ) {
      truehist(cleanWhiteDat[[i]], xlab = names(cleanWhiteDat)[i], col = 'lightgreen', main = paste("Mean =", signif(mean(cleanWhiteDat[[i]]),3)))
    }
    par(mfrow=c(1,1))
    
    summary(cleanWhiteDat); str(cleanWhiteDat)
    
    #### finding with iteration all lengths of outliers of vars ####
    output <- " "
    output <- vector("double", ncol(cleanWhiteDat)); output  # 1. output
    for (i in seq_along(cleanWhiteDat)) {                    # 2. sequence
      name[[i]] <- names(cleanWhiteDat)[[i]]
      output[[i]] <- length(boxplot.stats(cleanWhiteDat[[i]])$out)  # 3. body
    }
    name
    output
    
    #### boxplots of all vars #####
    oldpar = par(mfrow = c(2,4))
    for ( i in 2:9 ) {
      boxplot(cleanWhiteDat[[i]])
      mtext(names(cleanWhiteDat)[i], cex = 0.8, side = 1, line = 2)
    }
    par(mfrow=c(1,1))
    
    #### creating new datasets #################
    set.seed(123)
    wine_final2 <- cleanWhiteDat
    t <- 1:nrow(wine_final2)
    index2 <- sample(t, round(nrow(wine_final2)*.7))
    
    train_wine2 <- wine_final2[index2,]
    test_wine2 <- wine_final2[-index2,]
    
    #create training/test data specifically for glmnet
    train_a2 <- train_wine2[,2:9]
    train_b2 <- train_wine2[,1]
    test_a2 <- test_wine2[,2:9]
    test_b2 <- test_wine2[,1]
    #the testing matrix
    x2 <- model.matrix(test_wine2$qual ~., test_wine2)[,-1] 
    
    
######## multiple linear regression to the clean dataset ####
    
    set.seed(123)
    wine_final2 <- cleanWhiteDat
    t2 <- 1:nrow(wine_final2)
    index2 <- sample(t2, round(nrow(wine_final2)*.7))
    
    train_wine2 <- wine_final2[index2,]
    test_wine2 <- wine_final2[-index2,]
    
    mylm2 <- lm(qual ~ ., data=train_wine2) # multiple linear regression
    p0 <- round(coef(mylm2), 4); p0 # with OLS
    summary(mylm2)
    
    pred_wine_ols2 <- predict(mylm2, test_wine2) # predicting in testing set
    
    regr.eval(test_wine2$qual, pred_wine_ols2) # metrics
    cor(test_wine2$qual, pred_wine_ols2)
    plot(mylm2)
    plot(mylm2,4)
    
    ### residuals
    summary(mylm2$residuals); boxplot(mylm2$residuals)
    length(boxplot.stats(mylm2$residuals)$out) # length of outliers of residuals
    
    ### normality of residuals
    ks.test(mylm2$residuals, "pnorm", mean=mean(mylm2$residuals), sd=sd(mylm2$residuals))
    
    ols_plot_resid_hist(mylm2)
    title(main="Residuals with OLS after cleaning")
    skewness(mylm2$residuals)
    kurtosis(mylm2$residuals)
    summary(mylm2$residuals); boxplot(mylm2$residuals)
    
    ### residuals t-student
    res.simple <- rstudent(mylm2)
    plot(res.simple, pch=15, cex=.5, ylab="residuals student")
    abline(h=c(-2,0,2),lty=c(2,1,2))
    
    ### standard residuals
    res.standard <- rstandard(mylm2)
    plot(res.standard, pch=15, cex=.5, ylab="residuals standard")
    abline(h=c(-2,0,2),lty=c(2,1,2))
    
    #### checking number of residuals outside of (-2,2) #####
    
    over <- sum(res.simple > 2) ; over
    under <- sum(res.simple < - 2) ; under
    (over+under)/length(res.simple) # outside of bounds in %  0.045
    
    over <- sum(res.standard > 2) ; over
    under <- sum(res.standard < - 2) ; under
    (over+under)/length(res.standard) # outside of bounds in % 0.045
    
    
    
    ## ANOVA FOR QUALITY ##
    model.2 <- aov(alc ~ qual, data = cleanWhiteDat)
    summary(model.2)
    
    #### checking normality of residuals in mylm ####
    ols_plot_resid_qq(mylm2)
    ols_test_normality(mylm2)
    ols_test_correlation(mylm2)
    ols_plot_resid_fit(mylm2)
    ols_plot_resid_hist(mylm2)
    
    # shapiro.test(mylm2$residuals)
    # 
    # ad.test(mylm2$residuals)
    # 
    # jarque.bera.test(mylm$residuals)
    # 
    # res1=residuals(mylm2,type="response")
    # res2=residuals(mylm2,type="pearson")
    # ks.test(res1, "pnorm")
    
    # skewness(mylm2$residuals)
    # kurtosis(mylm2$residuals)
    
    #### checking independence of residuals with Durbin Watson ####
    durbinWatsonTest(mylm2)  ### no independency
    
    dwtest(mylm2) ### independency
    
    
##################################################################

######## checking with multilevel linear regression ####
    
    #### regession alcohol vs. sugar Tsantalis ####
regr_1 <- lm(alc ~ sug, data = mydataset); summary(regr_1)
plot(mydataset$sug, mydataset$alc, main="Sugar vs. Alcohol")
abline(regr_1)
hist(regr_1$residuals)
summary(regr_1$residuals)
boxplot(regr_1$residuals, main="Residuals")
shapiro.test(regr_1$residuals)
ols_test_normality(regr_1)

rstand_resid <- rstandard(regr_1)
plot(rstand_resid, main="Residuals")
abline(h=c(-2,0,2), lty=c(2,1,2))

cor(mydataset$alc, mydataset$sug)

    #### checking regression qual. vs. alc ####
regr_2 <- lm(qual ~ alc, data = mydataset); summary(regr_2)
plot(mydataset$alc, mydataset$qual, main="Quality vs. Alcohol")
abline(regr_2)
hist(regr_2$residuals)
summary(regr_2$residuals)
boxplot(regr_2$residuals, main="Residuals")
shapiro.test(regr_2$residuals)
ols_test_normality(regr_2)

rstand_resid <- rstandard(regr_2)
plot(rstand_resid, main="Residuals")
abline(h=c(-2,0,2), lty=c(2,1,2))
# counting residuals outside of (-2,2)
over <- sum(rstand_resid > 2) ; over
under <- sum(rstand_resid < - 2) ; under
(over+under)/length(rstand_resid) # outside of bounds in %

# correlation
cor(mydataset$qual, mydataset$alc)

    #### regr.alc. vs. sug portugal white wines ####
library(readr)
port_data_w <- read.csv("~/PhD/2018 july/datasets/portugal datasets/winequality-white_portugal.csv",sep=";", dec=".")
names(port_data_w)
regr_2 <- lm(port_data_w$alcohol ~ port_data_w$residual.sugar, data = port_data_w)
summary(regr_2)
plot(port_data_w$residual.sugar, port_data_w$alcohol,main="Alc vs. sug portugal wines")                                     
abline(regr_2)

hist(regr_2$residuals)
summary(regr_2$residuals)
boxplot(regr_2$residuals, main="Residuals of portugal")
shapiro.test(regr_2$residuals)
ols_test_normality(regr_2)

rstand_resid <- rstandard(regr_2)
plot(rstand_resid, main="Residuals of portugal white")
abline(h=c(-2,0,2), lty=c(2,1,2))

cor(mydataset$alc, mydataset$sug)

    #### regression with Forina italian wines ####
wines_forina <- read_csv("~/PhD/datasets/wines_forina.csv")
names(wines_forina)

regr_4 <- lm(wines_forina$alcohol ~ wines_forina$sugar, data = wines_forina)
summary(regr_4)
cor(wines_forina$alcohol,wines_forina$alcohol)
plot(wines_forina$sugar, wines_forina$alcohol,main="Alc vs. sug italian wines")                                     
abline(regr_4)

wines_forina <- wines_forina[ , c(2,4:11)]

mycor <- round(cor(wines_forina), 3)
mycor
# wines_forinaa <- wines_forina[ ,c(1,2,3,4,5,6,12)]
# wines_forinab <- wines_forina[ ,c(7,8,9,10,11,12)]
pairs(wines_forina) #1st WAY 
pairs.panels(wines_forina)#2nd WAY
chart.Correlation(wines_forina, histogram = T, pch = 19) #3rd WAY
mycor <- cor(wines_forina) #4th WAY
corrplot(mycor, type = "upper", method = "pie")
plot_matrix <- function(matrix_toplot){
  corrplot.mixed(matrix_toplot,
                 order = "original", 
                 tl.col='black', tl.cex=.50)
}
plot_matrix(cor(wines_forina)) #5th WAY

###################################################################
######## Various Discriminant Analysis ####
# mydataset <- mydataset_orig
    #### LDA Linear Discriminant Analysis ####
        #### create the model####
model1 <- lda(qual ~ ., data = train_wine)
model1
# ploting the model
plot(model1)
# making the predictions
predictions1 <- model1 %>% predict(test_wine)
names(predictions1)

# Predicted classes
head(predictions1$class, 6)
# Predicted probabilities of class membership.
round(head(predictions1$posterior, 6), digits = 6) 
head(test_wine$qual,6)
# Linear discriminants
round(head(predictions1$x, 6) ,digits = 6)

        #### drawing LDA####
p1 <- predict(model1)
lda.data <- cbind(train_wine, predict(model1)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(colour=qual,shape = qual)) 


plot(predictions1$x[,1],predictions1$x[,2]) # make a scatterplot
text(predictions1$x[,1],predictions1$x[,2], train_wine$qual,cex=0.7,pos=4,col="red") # add labels

# plot(model1)
        #### accuracy####
mean(predictions1$class==test_wine$qual)
test_factor <- as.factor(test_wine$qual)
confusionMatrix(predictions1$class, test_factor)

actual <- test_wine$qual; preds <- predictions1$class
# regr.eval(actual,preds)
actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy #may give Inf

# histograms according to LD1 etc...
# ldahist(data = predictions1$x[,3], g=test_wine$qual)

# contructing ROC evaluation
### CONSTRUCTING ROC AUC PLOT:# Get the posteriors as a dataframe.
# predictions1.posteriors <- as.data.frame(predictions1$posterior)# Evaluate the model
# pred1 <- as.numeric(predictions1$class); test1 <- as.numeric(test_wine$qual)
# pred <- prediction(pred1, test1)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values# Plot
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

    #### Quadratic Discriminant Analysis ####
# Fit the model
model2 <- qda(qual ~., data = train_wine)
model2
# Make predictions
predictions2 <- model2 %>% predict(test_wine)
# Model accuracy
mean(predictions2$class == test_wine$qual)

# test_factor <- as.factor(test_wine$qual)
confusionMatrix(predictions2$class, test_factor)

actual <- test_wine$qual; preds <- predictions2$class
actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy #may give Inf

# drawing QDA
# ? not exist
# drawing QDA
p2 <- predict(model2)
# qda.data <- cbind(train_wine, predict(model2)$x)
# ggplot(qda.data, aes(LD1, LD2)) + geom_point(aes(color = qual)) 

    #### Mixture Discriminant Analysis ####
# library(mda)
# Fit the model
model3 <- mda(qual~., data = train_wine)
model3
# Make predictions
predicted.classes <- model3 %>% predict(test_wine)
# Model accuracy
mean(predicted.classes == test_wine$qual)

# test_factor <- as.factor(test_wine$qual)
confusionMatrix(predicted.classes, test_factor)

actual <- test_wine$qual; preds <- predicted.classes
actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy #may give Inf

# plot(model3)
# p3<- predict(model3, newdata=test_wine)
# mda.data <- cbind(test_wine, p3)
# 
# p3_n <- as.numeric(p3)
# plot(p3_n, pch=15, cex=1, col="red")
# points(actual, col='green', pch=15, cex=1)
# 
# ggplot(mda.data, aes(qual, p3)) + geom_point(aes(color = qual))

    #### Flexible Discriminant Analysis ####
# Fit the model
model4 <- fda(qual~., data = train_wine)
model4
# Make predictions
predicted.classes <- model4 %>% predict(test_wine)

# Model accuracy
mean(predicted.classes == test_wine$qual)

# test_factor <- as.factor(test_wine$qual)
confusionMatrix(predicted.classes, test_factor)

actual <- test_wine$qual; preds <- predicted.classes
actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

# plot(model4)
    #### Regularized Discriminant Analysis ####

# library(klaR)
# Fit the model
model5 <- rda(qual~., data = train_wine)
model5
# Make predictions
predictions <- model5 %>% predict(test_wine)
# Model accuracy
mean(predictions$class == test_wine$qual)
# cor(predictions$class, test_wine$qual)

test_factor <- as.factor(test_wine$qual)
confusionMatrix(predictions$class, test_factor)

actual <- test_wine$qual; preds <- predictions$class
actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

partimat(qual ~ .,data=mydataset,method="lda") 
partimat(qual ~ .,data=mydataset,method="qda") 
partimat(qual ~ .,data=mydataset,method="rda") 


##################################################################
######## Polynomial Regression ####

    #### WE BUILD THE SIMPLE REGRESSION AND THEN 
    #### WE BUILD FOUR DIFFERENT POLYNOMIAL REGRESSIONS####
    mydataset$qual <- as.numeric(as.character(mydataset$qual))
    fit <- lm(mydataset$qual ~ mydataset$alc)
    plot(mydataset$qual ~ mydataset$alc, main="Quality~Alcohol", xlab="Alcohol", ylab="Quality")
    abline(fit, col="red")
    fit2 <- lm(mydataset$qual ~ mydataset$alc + I(mydataset$alc^2))
    fit3 <- lm(mydataset$qual ~ mydataset$alc + I(mydataset$alc^2) +I(mydataset$alc^3))
    fit4 <- lm(mydataset$qual ~ mydataset$alc + I(mydataset$alc^2) +I(mydataset$alc^3) + I(mydataset$alc^4))
    fit5 <- lm(mydataset$qual ~ mydataset$alc + I(mydataset$alc^2) +I(mydataset$alc^3) + I(mydataset$alc^4) + I(mydataset$alc^5))
    summary(fit)
    summary(fit2)
    summary(fit3)
    coef<-round(coef(fit3),3)
    coef
    summary(fit4)
    summary(fit5)
    #### WE FIND OUT WHICH IS THE MOST APPROPRIATE DEGREE FOR OUR REGRESSION####
    anova(fit, fit2, fit3, fit4, fit5) #fit3 is the best form of polynomial for our analysis
    #### WE ADD THE POLYNOMIAL REGRESSION (FIT3) INTO THE PREVIOUS PLOT####
    lines(smooth.spline(mydataset$alc, predict(fit3)), col="blue", lwd=3)
    #### WE CREATE TRAINING AND TESTING SET####
    # # SETTING SEED TO REPRODUCE RESULTS OF RANDOM SAMPLING 
    # set.seed(123)
    # # ROW INDICES FOR TRAINING DATA (70%-30%)
    # trainingRowIndexred <- sample(1:nrow(mydatasetred1), 0.7*nrow(mydatasetred1))
    # training.setred <- mydatasetred1[trainingRowIndexred, ] 
    # test_wine  <- mydatasetred1[-trainingRowIndexred, ]
    #### PREDICTION METRICS####
    # Convert to numeric the factors
    mydataset$qual <- as.numeric(as.character(mydataset$qual))
    train_wine$qual <- as.numeric(as.character(train_wine$qual))
    test_wine$qual <- as.numeric(as.character(test_wine$qual))
    
    fit3.prediction <- lm(qual ~ alc + I(alc^2) +I(alc^3), data=train_wine)
    summary(fit3.prediction)
    predicted_values <- predict(fit3.prediction ,test_wine)
    
    regr.eval(predicted_values, test_wine$qual)
    
    pred_val_round <- round(predicted_values, digits = 0)
    sum(pred_val_round == test_wine$qual , na.rm = T) / nrow(test_wine) *100
    mean(pred_val_round == test_wine$qual)
    cor(pred_val_round, test_wine$qual)
    # MIN - MAX ACCURACY
    dist_predred <- predict(fit3.prediction , test_wine)
    actuals_preds <- data.frame(cbind(actuals=test_wine$qual , predicteds=dist_predred))
    min_max_accuracy <- mean(apply(actuals_preds,1,min)/apply(actuals_preds,1,max))
    min_max_accuracy
    cor(predicted_values, test_wine$qual)
    
    ctable <- table(test_wine$qual, predicted)
    ctable
    mean(test_wine$qual == predicted) # percentage of prediction
    
##################################################################
######## making multinomial logistic regression ####
    
    mydataset$qual <- as.factor(mydataset$qual)
    train_wine$qual <- as.factor(train_wine$qual)
    test_wine$qual <- as.factor(test_wine$qual)
    
    # making relevel of train_wine$qual
    train_wine$qual <- relevel(train_wine$qual, ref = "3")
    
    # we remove intercept with -1
    multinom.fit <- multinom(qual ~ alc + sug + ph -1, data = train_wine)
    
    # Checking the model
    summary(multinom.fit)
    
    ## extracting coefficients from the model and exponentiate
    round(exp(coef(multinom.fit)), digits = 4)
    
    # showing the probability table in the training set
    # head(probability.table <- fitted(multinom.fit))
    
    # Predicting the values for training dataset
    predicted <- predict(multinom.fit, newdata = train_wine, "class")
    
    # Building classification table
    ctable <- table(train_wine$qual, predicted)
    ctable
    mean(train_wine$qual == predicted) # percentage of prediction
    # v12 <- (train_wine$qual == train_wine$predicted)
    # Calculating accuracy - sum of diagonal elements divided by total obs
    round((sum(diag(ctable))/sum(ctable))*100,2) # same percentage of prediction
    
    #### Predicting the values for testing dataset####
    predicted <- predict(multinom.fit, newdata = test_wine, "class")
    test_wine$qual <- factor(test_wine$qual, levels=c(3,4,5,6,7,8,9,10))
    # first we order properly the levels
    predicted <- factor(predicted, levels = c(3,4,5,6,7,8,9,10))
    #### Building classification table####
    ctable2 <- table(test_wine$qual, predicted) # not correct table
    ctable2
    mean(test_wine$qual == predicted)
    # str(test_wine$predicted)
    
    # Calculating accuracy - sum of diagonal elements divided by total obs
    round((sum(diag(ctable2))/sum(ctable2))*100,2) # gives error results
    
    actual <- test_wine$qual; preds <- predicted
    actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
    min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
    min_max_accuracy
    
    t2<- as.numeric(as.character(test_wine$qual)); p2<- as.numeric(as.character(predicted))
    cor(t2 , p2)
    regr.eval(t2,p2)
    
    perc1 <- abs(p2 - t2)/ t2; mean(perc1) # mape
    # summary(perc1)
    perc2 <- abs(t2 - p2)/ p2; mean(perc2)
    # summary(perc2)
    #### 2nd approach of multinomial logistic regression ####
    
    # Fit the model
    ### model <- nnet::multinom(Species ~., data = train.data)
    multinom.fit <- multinom(qual ~ alc + sug + ph + ta -1, data = train_wine)
    
    # Summarize the model
    summary(multinom.fit)
    # Make predictions
    predicted.classes <- multinom.fit %>% predict(test_wine)
    # head(predicted.classes)
    # Model accuracy
    mean(predicted.classes == test_wine$qual)
    
    #### computing z and p values ####
    # computing z score
    z <- summary(multinom.fit)$coefficients/summary(multinom.fit)$standard.errors
    z
    
    # computing p-values
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    round(p, digits=6)
    
    p1 <- predict(multinom.fit, newdata = test_wine, "probs")
    summary(p1)
    
    pp.write <- cbind(mydataset, predict(multinom.fit, newdata = mydataset, type = "probs", se = TRUE))
    by(pp.write[, 10:17], pp.write$qual, colMeans)
    
    # lpp <- melt(pp.write, id.vars = c(1), value.name = "probability")
    # head(lpp)
    # summary(lpp); str(lpp)
    # ggplot(lpp, aes(x = mydataset$qual, y = probability, colour = mydataset$qual)) + geom_line() + facet_grid(variable ~ ., scales = "free")
    
##################################################################
######## Poisson Regression ####
    # convert to factors
    # mydataset$qual <- as.factor(mydataset$qual)
    # train_wine$qual <- as.factor(train_wine$qual)
    # test_wine$qual <- as.factor(test_wine$qual)
    
model.1 <- glm(qual ~ ., family = poisson(link = "log"), data = train_wine)
# model.1 <- glm(qual ~ ., family = poisson, data = train_wine)

summary(model.1)
fit.1 <- fitted(model.1)
pred_1 <- predict(model.1, newdata = test_wine, type = "response")
    # convert to numeric
    mydataset$qual <- as.numeric(as.character(mydataset$qual))
    train_wine$qual <- as.numeric(as.character(train_wine$qual))
    test_wine$qual <- as.numeric(as.character(test_wine$qual))

regr.eval(pred_1r, test_wine$qual)
cor(pred_1, test_wine$qual)
pred_1r <- round(pred_1, digits = 0)
mean(pred_1r == test_wine$qual)
cor(pred_1r, test_wine$qual)
Accuracy(pred_1r, test_wine$qual)
# metrics 
actuals_preds <- data.frame(cbind(test_wine$qual, pred_1r)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

    #### Quasi Poisson Regression ####

# model.1 <- glm(qual ~ ., family = quasipoisson(link = "log"), data = train_wine)
model.2 <- glm(qual ~ ., family = quasipoisson, data = train_wine)

summary(model.2)
fit.2 <- fitted(model.2)
pred_2 <- predict(model.2, newdata = test_wine, type = "response")

regr.eval(pred_2, test_wine$qual)
cor(pred_2, test_wine$qual)
pred_2r <- round(pred_2, digits = 0)
mean(pred_2r == test_wine$qual)
cor(pred_2r, test_wine$qual)
Accuracy(pred_2r, test_wine$qual)

# metrics 
actuals_preds <- data.frame(cbind(test_wine$qual, pred_2)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

    #### Negative Binomial Regression ####

# model.1 <- glm(qual ~ ., family = quasipoisson(link = "log"), data = train_wine)
model.3 <- glm.nb(qual ~ ., data = train_wine)

summary(model.3)
fit.3 <- fitted(model.3)
pred_3 <- predict(model.3, newdata = test_wine, type = "response")

regr.eval(pred_3, test_wine$qual)
cor(pred_3, test_wine$qual)
pred_3r <- round(pred_3, digits = 0)
mean(pred_3r == test_wine$qual)
cor(pred_3r, test_wine$qual)
Accuracy(pred_3r, test_wine$qual)

# metrics 
actuals_preds <- data.frame(cbind(test_wine$qual, pred_3)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
min_max_accuracy

##################################################################
######## decision trees (with rpart package) ########

    # convert to factors
    # mydataset$qual <- as.factor(mydataset$qual)
    # train_wine$qual <- as.factor(train_wine$qual)
    # test_wine$qual <- as.factor(test_wine$qual)

fitnew <- rpart(qual ~., data = train_wine, method = "anova")
# rpart.plot(fitnew, extra = 106)
rpart.plot(fitnew)

predict_unseen <-predict(fitnew, test_wine)
predict_unseen2 <- round(predict_unseen, digits = 0)
    #### prediction table of DT - evaluation####
    table_mat1 <- table(test_wine$qual, predict_unseen2) 
    table_mat1
    mean(test_wine$qual == predict_unseen2)
    
    test_qual <- as.numeric(as.character(test_wine$qual))
    
    regr.eval(test_qual, predict_unseen2)
    cor(test_qual, predict_unseen2)
    
    actual <- test_wine$qual; preds <- predict_unseen2 
    actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
    min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
    min_max_accuracy

# plot(test_qual, predict_unseen2)
# df1 <- data.frame(test_qual, predict_unseen2)
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
##################################################################
######## random forest method (with randomForest package) ####
rf1 <- randomForest(qual ~., data = train_wine, ntree = 500, mtry = 2, importance = TRUE)
# printing the results   
print(rf1)

pred_r = predict(rf1, newdata=test_wine[-1])
# pred_r <- round(pred, digits = 0)
cm1 = table(test_wine[,1], pred_r); cm1  # table of cross validation
round((sum(diag(cm1))/sum(cm1))*100,4)
mean(pred_r == test_wine$qual)

    #### evaluation of metrics####
    # converting to numeric
    pr_rf <- as.numeric(as.character(pred_r))
    tr_rf <- as.numeric(as.character(test_wine$qual))

    regr.eval(pr_rf, tr_rf)
    cor(pr_rf, tr_rf)
    # pr_rf <- round(pr_rf, digits = 0)
    mean(pr_rf == tr_rf)
    cor(pr_rf, tr_rf)
    Accuracy(pr_rf, tr_rf)
    
    #### metrics ####
    actuals_preds <- data.frame(cbind(tr_rf, pr_rf)) ; dim(actuals_preds)
    min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); 
    min_max_accuracy
    
    ####running the RF model with 10-fold CV (with CARET package) ####
    trControl <- trainControl(method = "cv",
                              number = 10,
                              search = "grid")
    set.seed(123)
    # # Run the model
    # # convert to factor the dependent 
    # mydataset$qual <- as.factor(mydataset$qual)
    # train_wine$qual <- as.factor(train_wine$qual)
    # test_wine$qual <- as.factor(test_wine$qual)
    
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
    mean(pr_rf_1 == tr_rf_1)
    cor(pr_rf_1, tr_rf_1)
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
    pr_rf_2 <- as.numeric(as.character(pred_values))
    
    regr.eval(pr_rf_2, tr_rf_2)
    cor(pr_rf_2, tr_rf_2)
    # pr_rf_2 <- round(pr_rf_2, digits = 0)
    mean(pr_rf_2 == tr_rf_2)
    cor(pr_rf_2, tr_rf_2)
    Accuracy(pr_rf_2, tr_rf_2)
    #### metrics ####
    actuals_preds2 <- data.frame(cbind(tr_rf_2, pr_rf_2)) ; dim(actuals_preds2)
    min_max_accuracy <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max)); 
    min_max_accuracy

##################################################################
######## RIDGE regression preparing  ##########
mydataset <- cleanWhiteDat
x <- model.matrix(train_wine2$qual ~., train_wine2)[,-1] # trim off the first column
head(x); dim(x)
y <- train_wine2$qual
# leaving only the predictors
# y = mydataset %>%
#  select(qual) %>%
#  unlist() %>%
#  as.numeric()
head(y); str(y)
# summary(mydataset)

    #### creating the vector lambda  ####
    lambda <- 10^seq(10, -2, length = 100) 
    length(lambda); mean(lambda);
    
# summary(lambda);plot(lambda);hist(lambda);boxplot(lambda)

######## RIDGE regression  ###############################
    
    ridge.mod <- glmnet(x,y,alpha=0, lambda = lambda)
    
    p1_s0 <- predict(ridge.mod, s=0, exact = F, type = 'coefficients')[1:9, ] 
    p1a<- round(p1_s0, 4); p1a #with ridge
    pdif<- p0/p1_s0; pdif
    diff_p0_p1 <- 1 - round(pdif,4); diff_p0_p1
    #we proved that with lambda=0 we have the same results with OLS regression
    
    mylm_train <- lm(qual ~ .,data = train_wine2)
       # summary(mylm_train); round(coef(mylm_train), 4)
    ridge.mod <- glmnet(x, y, alpha = 0)
       # summary(ridge.mod)
    plot_glmnet(ridge.mod)

    #### find the best lambda #####
          cv.out <- cv.glmnet(x, y, alpha = 0) ; 
          plot(cv.out, main="Log(L) with ridge in train set")
          min_lam_ridge <- cv.out$lambda.min ; min_lam # min lambda = 0.08962106 (log = -1.047)
          best_lam_ridge <- cv.out$lambda.1se; best_lam # best lambda = 0.693903 (log = -0.1182)
          coef(cv.out,s=best_lam)
          coef(cv.out,s=min_lam)
      
        
    #### make predictions #####
          ridge.pred <- predict(ridge.mod, s=min_lam, newx = x2) # vector of dep.var. with ridge
          s.pred <- predict(mylm_train, newdata = test_wine2) # vector of dep.var. with OLS
          # summary(ridge.pred); summary(s.pred); 
          # boxplot(ridge.pred, main="with ridge") ; boxplot(s.pred, main="with OLS")
      
    #### check MSE #####
          MSE1 <- mean((s.pred - test_wine2$qual)^2); MSE1 # 1.3031 with OLS
          # cor(s.pred, test_b2)
          MSE2 <- mean((ridge.pred - test_wine2$qual)^2); MSE2 # 1.3098 with Ridge
          # cor(ridge.pred, test_wine2$qual)
          regr.eval(ridge.pred, test_wine2$qual) # all metrics
      
    #### looking the coefficients #####
          out <- glmnet(x, y, alpha = 0); # 
          plot(out, label=T, main="with ridge")
          legend("bottomleft", lwd = 1, col = 1:9, legend = colnames(x), cex = .5)
          # plot_glmnet(out, label = T, main="with ridge")
          # plot_glmnet(out, label = 5, main="with ridge")
          
          p1_bl <- predict(ridge.mod, type = 'coefficients', s=best_lam)[1:9, ]
          round(p1_bl, 4)
          
          p1_ml <- predict(ridge.mod, type = 'coefficients', s=min_lam)[1:9, ]
          round(p1_ml, 4)
      
    #### all metrics #####
      
          error_rate1a <- regr.eval(s.pred, test_b2);  error_rate1a
          
          error_rate2a <- regr.eval(ridge.pred, test_b2);  error_rate2a
          
    #### residuals of ridge regression #####
        ridge_resid <- ridge.pred - test_wine2$qual
        # ridge_resid2 <- ridge.pred - test_b2
        ks.test(ridge_resid, "pnorm", mean=mean(ridge_resid), sd=sd(ridge_resid))
       
#########################################################################

######## LASSO regression ################################
        train_wine2<- train_wine[ , c(1,2,3,5,6,7)]
        test_wine2 <- test_wine[ , c(1,2,3,5,6,7)]
        #### convert to numeric ####
        train_wine2$qual<- as.numeric(as.character(train_wine2$qual))
        test_wine2$qual<- as.numeric(as.character(test_wine2$qual))
        
        x <- model.matrix(train_wine2$qual ~., train_wine2)[,-1] # trim off the first column
        head(x); dim(x)
        x2 <- model.matrix(test_wine2$qual ~., test_wine2)[,-1] # trim off the first column
        
        y <- train_wine2$qual
    #### find the min & best lambda for Lasso #####
    cv.out <- cv.glmnet(x, y, alpha = 1) ; 
    plot(cv.out, main="Log(L) with lasso in train set")
    min_lam_lasso <- cv.out$lambda.min ; min_lam_lasso # min lambda = 0.008554764 (log = -2.067792)
    best_lam_lasso <- cv.out$lambda.1se; best_lam_lasso # best lambda =  0.1530147 (log = -0.8152668)
    
    lasso.mod <- glmnet(x, y, alpha = 1, lambda = best_lam_lasso)
    lasso.pred <- predict(lasso.mod, s=min_lam_lasso, newx = x2)
    
    #### prediction metrics####
    tr_la_1<- test_wine2$qual
    pr_la_1<- lasso.pred
    regr.eval(pr_la_1, tr_la_1)
    cor(pr_la_1, tr_la_1)
    # pr_rf_2 <- round(pr_rf_2, digits = 0)
    # mean(pr_la_1 == tr_la_1)
    # cor(pr_rf_1, tr_rf_1)
    Accuracy(pr_la_1, tr_la_1)
    #### metrics ####
    actuals_preds1 <- data.frame(cbind(tr_la_1, pr_la_1)) ; dim(actuals_preds1)
    min_max_accuracy <- mean(apply(actuals_preds1, 1, min) / apply(actuals_preds1, 1, max)); 
    min_max_accuracy
    
    plot(lasso.mod, "lambda", label = T, main="with Lasso")
    legend("bottomright", lwd = 1, col = 1:9, legend = colnames(x), cex = .6)
    coef(cv.out,s=best_lam_lasso)
    coef(cv.out,s=min_lam_lasso)
    
    out <- glmnet(x, y, alpha = 1); # 
    plot(out, label=T, main="with lasso")
    legend("bottomleft", lwd = 1, col = 1:9, legend = colnames(x), cex = .6)
    # plot_glmnet(out, label = T, main="with lasso")
    # plot_glmnet(out, label = 5, main="with lasso")
    
    # find MSE
    MSE3 <- mean((lasso.pred - test_wine2$qual)^2); MSE3 # 1.3118
    # coefficients
    lasso.coef <- predict(lasso.mod, type = 'coefficients', s=min_lam_lasso)[1:6, ]; lasso.coef
    
    #### looking the coefficients #####
      
    p1_bl <- predict(lasso.mod, type = 'coefficients', s=best_lam)[1:9, ]
    round(p1_bl, 4)
    
    p1_ml <- predict(lasso.mod, type = 'coefficients', s=min_lam)[1:9, ]
    round(p1_ml, 4)
    
    error_rate_lasso <- regr.eval(lasso.pred, test_wine2$qual); error_rate_lasso
    
    #### residuals with Lasso #####
          lasso_resid <- lasso.pred - test_wine2$qual
          # ridge_resid2 <- ridge.pred - test_b2
          ks.test(lasso_resid, "pnorm", mean=mean(lasso_resid), sd=sd(lasso_resid))
          hist(lasso_resid)
    
#########################################################################
######## another approach ####
####create training and test data####
set.seed(123)
wine_final <- mydataset
t <- 1:nrow(wine_final)
index <- sample(t, round(nrow(wine_final)*.7))

train_wine <- wine_final[index,]
test_wine <- wine_final[-index,]

####create training/test data specifically for glmnet####
train_a <- train_wine[,2:9]
train_b <- train_wine[,1]
test_a <- test_wine[,2:9]
test_b <- test_wine[,1]

# We will use the package glmnet to build the models for ridge, 
# lasso and elastic net regression. We will then compare their 
# respective Root Mean Square Error for the test dataset.

######## RIDGE #############################

# accordingly to Tbishirani this method of model is not right
#### ridge_wine <- cv.glmnet(as.matrix(train_a2), train_b2, family = 'gaussian', alpha = 0)
ridge_wine <- glmnet(x,y,alpha=0, lambda = lambda)
#predict ridge
ridge_pred <- predict(ridge_wine, s = min_lam, newx = as.matrix(test_a2))

# p2<- round(ridge_pred, 4); p2

mse_ridge_wine <- mean((test_b2 - ridge_pred)^2); mse_ridge_wine
ridge_error <- sqrt(mse_ridge_wine); ridge_error
regr.eval(ridge_pred, test_b2)

#plot residuals and prediction
ridge_resid_wine <- test_b - ridge
plot(ridge_resid_wine)
abline(h=c(-2,0,2),lty=c(2,1,2))

boxplot(ridge_resid_wine)
# hist(ridge_resid_wine)

# truehist(ridge_resid_wine, h = 1, col="slategray3", main = "Residuals of white wines with Ridge") 

qplot(ridge_resid_wine, geom="histogram", binwidth = 1,
      main = "Residuals with ridge", xlab = "scores", ylab = "observations", 
      fill=I("blue"), col=I("red"),alpha=I(.2))

summary(ridge_resid_wine)
ols_test_normality(ridge_resid_wine)
ks.test(ridge_resid_wine, "pnorm", mean=mean(ridge_resid_wine), sd=sd(ridge_resid_wine))

    #### checking number of residuals outside of (-2,2) #####

over <- sum(ridge_resid_wine > 2) ; over
under <- sum(ridge_resid_wine < - 2) ; under
(over+under)/length(ridge_resid_wine) # outside of bounds in %  0.15

######## LASSO ##############################

# according to Tbishirani this is not the right way to make the model
### lasso_wine <- cv.glmnet(as.matrix(train_a2), train_b2, family = 'gaussian', alpha = 1)
lasso_wine <- glmnet(x, y, alpha = 1, lambda = lambda)
#predict lasso
lasso <- predict(lasso_wine, s = min_lam_lasso, newx = as.matrix(test_a2))
mse_lasso <- mean((test_b2 - lasso)^2); mse_lasso
lasso_error <- sqrt(mse_lasso); lasso_error
regr.eval(lasso,test_b2)
#plot residuals
lasso_resid <- test_b2 - lasso
plot(lasso_resid, main="residuals with lasso")
abline(h=c(-2,0,2),lty=c(2,1,2))

    #### checking number of residuals outside of (-2,2) #####

over <- sum(lasso_resid > 2) ; over
under <- sum(lasso_resid < - 2) ; under
(over+under)/length(lasso_resid) # outside of bounds in % 0.13

qplot(lasso_resid, geom="histogram", binwidth = 1,
      main = "Residuals with lasso", xlab = "scores", ylab = "observations", 
      fill=I("blue"), col=I("red"),alpha=I(.2))

ks.test(lasso_resid, "pnorm", mean=mean(lasso_resid), sd=sd(lasso_resid))

######## elastic net alpha = 0.1 - 0.9 ###################

n = seq(0.1:0.9, by = 0.1)
i = 0.1
for (i in n){
  assign(paste('elastic', i, sep = ''), cv.glmnet(as.matrix(train_a2), 
                                                  train_b2, family = 'gaussian', 
                                                  alpha = i))
}

#predict elasticnet
yhat0.1 <- predict(elastic0.1, elastic0.1$lambda.1se, newx = as.matrix(test_a2))
yhat0.2 <- predict(elastic0.2, elastic0.2$lambda.1se, newx = as.matrix(test_a2))
yhat0.3 <- predict(elastic0.3, elastic0.3$lambda.1se, newx = as.matrix(test_a2))
yhat0.4 <- predict(elastic0.4, elastic0.4$lambda.1se, newx = as.matrix(test_a2))
yhat0.5 <- predict(elastic0.5, elastic0.5$lambda.1se, newx = as.matrix(test_a2))
yhat0.6 <- predict(elastic0.6, elastic0.6$lambda.1se, newx = as.matrix(test_a2))
yhat0.7 <- predict(elastic0.7, elastic0.7$lambda.1se, newx = as.matrix(test_a2))
yhat0.8 <- predict(elastic0.8, elastic0.7$lambda.1se, newx = as.matrix(test_a2))
yhat0.9 <- predict(elastic0.9, elastic0.9$lambda.1se, newx = as.matrix(test_a2))

mselas0.1 <- mean((test_b2 - yhat0.1)^2) ; mselas0.1
mselas0.2 <- mean((test_b2 - yhat0.2)^2) ; mselas0.2
mselas0.3 <- mean((test_b2 - yhat0.3)^2) ; mselas0.3
mselas0.4 <- mean((test_b2 - yhat0.4)^2) ; mselas0.4
mselas0.5 <- mean((test_b2 - yhat0.5)^2) ; mselas0.5
mselas0.6 <- mean((test_b2 - yhat0.6)^2) ; mselas0.6
mselas0.7 <- mean((test_b2 - yhat0.7)^2) ; mselas0.7
mselas0.8 <- mean((test_b2 - yhat0.8)^2) ; mselas0.8
mselas0.9 <- mean((test_b2 - yhat0.9)^2) ; mselas0.9

elastic_error0.1 <- sqrt(mselas0.1) ; elastic_error0.1
elastic_error0.2 <- sqrt(mselas0.2) ; elastic_error0.2
elastic_error0.3 <- sqrt(mselas0.3) ; elastic_error0.3
elastic_error0.4 <- sqrt(mselas0.4) ; elastic_error0.4
elastic_error0.5 <- sqrt(mselas0.5) ; elastic_error0.5
elastic_error0.6 <- sqrt(mselas0.6) ; elastic_error0.6
elastic_error0.7 <- sqrt(mselas0.7) ; elastic_error0.7
elastic_error0.8 <- sqrt(mselas0.8) ; elastic_error0.8
elastic_error0.9 <- sqrt(mselas0.9) ; elastic_error0.9

###alpha = 0.9 has the best rmse

    #### compare with Multiple Linear Regression ###############
    
        lm_wine_train <- cbind(train_a2, train_b2)
        lm_wine_test <- cbind(test_a2, test_b2)
        linear <- lm(train_b2~., lm_wine_train); summary(linear)
    #### predict with linear regression #####
        pred_wine_lm <- predict(linear, lm_wine_test)
        mselm_wine <- mean((lm_wine_test$test_b - pred_wine_lm)^2); mselm_wine
        linear_error <- sqrt(mselm_wine); linear_error
    #### plot residuals####
        resid <- lm_wine_test$test_b2 - pred_wine_lm
        summary(linear$residuals)
        summary(resid)
        plot(resid)
        abline(h=c(-2,0,2),lty=c(2,1,2))
        
    #### checking number of residuals outside of (-2,2) #####
        
        over <- sum(resid > 2) ; over
        under <- sum(resid < - 2) ; under
        (over+under)/length(resid) # outside of bounds in % 0.15
        
        qplot(resid, geom="histogram", binwidth = 1,
              main = "Residuals with OLS in testing set", xlab = "scores", ylab = "observations", 
              fill=I("blue"), col=I("red"),alpha=I(.2))
        
        qplot(linear$residuals, geom="histogram", binwidth = 1,
              main = "Residuals with OLS in the training set", xlab = "scores", ylab = "observations", 
              fill=I("blue"), col=I("red"),alpha=I(.2))
        
        ols_test_normality(linear$residuals)
        ols_test_normality(resid)
        
        ks.test(linear$residuals, "pnorm", mean=mean(linear$residuals), sd=sd(linear$residuals))
        
    #### comparing results #########
    
    errors <- rbind(ridge_error, lasso_error, elastic_error0.1, elastic_error0.2, 
                    elastic_error0.3, elastic_error0.4, elastic_error0.5, 
                    elastic_error0.6, elastic_error0.7, elastic_error0.8, 
                    elastic_error0.9, linear_error)
    bestModel <- min(errors); bestModel
    errors
    
########################################################################

######## elastic net iterations ##########

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(qual ~ .,
                     data = train_wine2,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)


# Best tuning parameter
elastic_reg$bestTune

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
y_train <- train_wine2$qual; 
eval_results(y_train, predictions_train, train_wine2) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x2)
eval_results(test_b2, predictions_test, test_wine2)

######## making anova with all models #######

# all_anova <- anova(ridge.mod)
# summary(all_anova)

######## END  ##################################################
    #### making training and testing sets ####
set.seed(123)

train = mydataset %>%
  sample_frac(0.7)

test = mydataset %>%
  setdiff(train)

x_train = model.matrix(qual~., train)[,-1]
x_test = model.matrix(qual~., test)[,-1]

y_train = train %>%
  select(qual) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(qual) %>%
  unlist() %>%
  as.numeric()
########################################################################

######## RIDGE ######
ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12) # with alpha=0 doing Ridge, with 1 Lasso
ridge_pred = predict(ridge_mod, s = 4, newx = x_test)
mean((ridge_pred - y_test)^2)

mean((mean(y_train) - y_test)^2)

ridge_pred = predict(ridge_mod, s = 1e10, newx = x_test)
mean((ridge_pred - y_test)^2)

ridge_pred = predict(ridge_mod, s = 0, newx = x_test, exact = F)
mean((ridge_pred - y_test)^2)

multi.reg <- lm(qual~., data = train); summary(multi.reg)
predict(ridge_mod, s = 0, exact = F, type="coefficients")[1:9,]

set.seed(123)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) # Fit ridge regression model on training data
bestlam = cv.out$lambda.min  # Select lambda that minimizes training MSE
bestlam

plot(cv.out, main="training MSE as a function of lambda") # Draw plot of training MSE as a function of lambda

ridge_pred = predict(ridge_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((ridge_pred - y_test)^2) # Calculate test MSE

out = glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:9,] # Display coefficients using lambda chosen by CV



######## LASSO #####

lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1, 
                   lambda = grid) # Fit lasso model on training data

plot(lasso_mod, main="coeff in LASSSO model")

set.seed(123)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out, main="training MSE as a function of lambda") # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lambda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE

out = glmnet(x, y, alpha = 1, lambda = grid) # Fit lasso model on full dataset with alpha = 1
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:9,] # Display coefficients using lambda chosen by CV
lasso_coef

lasso_coef[lasso_coef != 0] # Display only non-zero coefficients


######## 3rd approach ##########

# library(glmnet)
x_vars <- model.matrix(qual ~. , mydataset)[,-1] ; dim(x_vars)
y_var <- mydataset$qual; head(y_var); length(y_var)
lambda_seq <- 10^seq(2, -2, by = -.1); head(lambda_seq) ; length(lambda_seq)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), nrow(x_vars)*0.7) ; head(train) ; length(train) # indexes of records
test <- setdiff(1:nrow(x_vars), train) ; head(test); length(test)

x_test = mydataset[-train,-1 ] ; head(x_test) ; length(x_test)
y_test = mydataset[-train,1 ] ; head(y_test); length(y_test)
# finding best lambda
cv_output_lasso <- cv.glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda_seq); 
head(cv_output_lasso); length(cv_output_lasso)

# identifying best lambda
best_lam <- cv_output_lasso$lambda.min; best_lam

# Rebuilding the model with best lambda value identified (LASSO because alpha = 1)
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[test,]); head(pred) ; length(pred)

final <- cbind(y_var[test], pred)
# Checking the first six obs
head(final)

actual <- y_var[test] ; head(actual); length(actual)
preds <- pred ; head(preds); length(preds)
rss <- sum((preds - actual) ^ 2); rss
tss <- sum((actual - mean(actual)) ^ 2) ; tss
rsq <- 1 - rss/tss
rsq

# Inspecting beta coefficients
coef(lasso_best)


######## metrics of prediction #####

MAPE(preds,actual) ; #MAPE mean absolute percentage error may give Inf
MAE(preds, actual) #MAE mean absolute error
MSE(preds, actual) #MSE mean squared error
RMSE(preds, actual) #RMSE root mean squared error
Accuracy(preds, actual) #Accuracy may give 0 when actual = 0
mape_2 <- mean((preds - actual)/ actual); mape_2 #may give Inf

actuals_preds <- data.frame(cbind(actual, preds)) ; dim(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)); min_max_accuracy #may give Inf

correlation_accuracy <- cor(actuals_preds); correlation_accuracy #showing the correlation between actual & predicted

# MAPE Calculation :
mape3 <- mean(abs((actuals_preds$actual - actuals_preds$X1))/actuals_preds$actual); mape3 #may give Inf

cor(preds, actual) # correlation between values

# if some of actual = 0 then MAPE gives Inf in that case we use :
# library(Metrics)
smape(preds, actual) #symmetric mean absolute error
percent_bias(preds, actual) #difference error in percentage
precision(preds, actual) #precision in percentage
accuracy(preds, actual) #accuracy in percentage
rae(preds, actual) #relative absolute error

# all the metrics
# library(DMwR)
DMwR::regr.eval(actual, preds) 


########################################################################
######## Making Robust regression #########

# plot cooks distance and qqplot
plot(cooks.distance(lm(qual ~ ., data = mydataset)))
qqnorm(mydataset$qual); qqline(mydataset$qual)

# ols regression
ols1 <- lm(qual ~ ., data = mydataset); summary(ols1)  

# compute cooks distance
dist <- cooks.distance(ols1)
dist <- data.frame(dist)

s <- stdres(ols1)
a <- cbind(mydataset, dist, s)

# sort in order of standard residuals
sabs <- abs(s)
a <- cbind(mydataset, dist, s, sabs)
asorted <- a[order(-sabs), ]
asorted[1:10, ]

# finding robust regression with Huber weights
rr.huber <- rlm(qual ~. , data = mydataset)
summary(rr.huber)

# making huber dataset for finding Huber weight
huber <- data.frame(qual=mydataset$qual, resid2 = rr.huber$residuals, weight2 = rr.huber$w)
huber2 <- huber[order(huber$weight2), ]
huber2[1:10, ]

# finding bisquare weighting with robust regression
rr.bisquare <- rlm(qual ~., data = mydataset, psi=psi.bisquare)
summary(rr.bisquare)

# make bisquare as dataframe
bisqr <- data.frame(qual=mydataset$qual, resid=rr.bisquare$residuals, weight = rr.bisquare$w )
bisqr2 <- bisqr[order(bisqr$w), ]
bisqr2[1:10, ]
bisqr3 <- bisqr[which(bisqr$weight == max(bisqr$weight)), ]; dim(bisqr3)
# bisqr4 <- subset(bisqr, bisqr$weight > 0.5); dim(bisqr4)
# bisqr4 %>% filter(bisqr4$weight ==1) ; dim(bisqr4)
########################################################################
######## Making Robust Regression v.2 ########

# library(robustbase)
model_rob_2 <- lmrob(qual ~. , data = mydataset)
summary(model_rob_2)

# library(robust)
model_rob_3 <- lmRob(qual ~. , data = mydataset)
summary(model_rob_3)

# Least trimmed squares regression
ltsFit <- ltsReg(qual ~. , data = mydataset)
summary(ltsFit)

########################################################################
######## various graphics #####
histogram(cleanWhiteDat$qual, type = "density", main="Residuals with Ridge",
          panel = function(x, ...) {
                  panel.histogram(x, ...)
                  xn <- seq(min(x), max(x), length.out =1000)
                  yn <- dnorm(xn, mean(x), sd(x))
                  panel.lines(xn, yn, col = "red")
})

# install.packages("UsingR")
mydataset_orig <- mydataset.1
library(UsingR)
hist(mydataset_orig$qual, freq = FALSE)
x3 <- seq(3, 10, length.out = 100)
y3 <- with(mydataset_orig ,dnorm(x3, mean(qual), sd(qual)))
lines(x3, y3, col = "red")

densityPlot(mydataset_orig$qual)
densityPlot(ridge_resid, main="Residuals wth Ridge")

# hist(Galton$parent, freq = FALSE)
# x3 <- seq(64, 74, length.out=100)
# y3 <- with(Galton, dnorm(x3, mean(parent), sd(parent)))
# lines(x3, y3, col = "red")

hist(mydataset.1$qual, main = "Quality of white wines with frequency", breaks=10,  xlab ='Quality', col="gray")
truehist(mydataset.1$qual, h = 1, col="slategray3", main = "Quality of white wines with percentages") # with percentages
qplot(mydataset.1$qual, geom="histogram", binwidth = 1,main = "Quality",
      xlab = "scores", ylab = "observations", fill=I("blue"), col=I("red"),alpha=I(.2),)
qplot(mydataset.1$qual, geom="histogram", binwidth = 1,main = "Quality",
      xlab = "scores", ylab = "observations", fill=I("black"), col=I("red"),alpha=I(.2),)
