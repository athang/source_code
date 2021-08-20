data3 <- read.csv("data3.csv", header=T, sep=",")
#we get only the white wines
dfwnew <- subset(data3, colour == "white")
str(dfwnew)
#we remove the "colour" field
dfwnew2 <- dfwnew[,-2]
View(dfwnew2)
str(dfwnew2)
#we save our dataset
write.csv(dfwnew2, file="dfwnew2.csv", row.name=T)
save(dfwnew2, file="dfwnew2.RData")
#loading various libraries
#maybe need to be installed with install.packages("...")
library("corrplot")  # graphical display of the correlation matrix
library("caret")     # classification and regression training
library("klaR")      # naive bayes
library("nnet")      # neural networks (nnet and avNNet)
library("kernlab")   # support vector machines (svmLinear and svmRadial)
library("randomForest")  # random forest, also for recursive feature elimination
library("gridExtra") # save dataframes as images
library("doSNOW")    # parallel processing
#some graphical libraries
library("descr")
library("ggplot2")
library("aod")
library("psych")
library("scales")
#show the quality histogram
hist(dfwnew2$quality, main = "Quality of white wines", breaks=5,  xlab ='Quality', col="gray")
truehist(dfwnew2$quality, h = 1, col="slategray3")
#we divide the dataset into two datasets
#the training set consisting of 75% of the whole dataset
dfwnew2_train <- dfwnew2[1:1736, ]
#and the testing set with the rest 25%
dfwnew2_test <- dfwnew2[1737:2315, ]
str(dfwnew2_train)
str(dfwnew2_test)
hist(dfwnew2_train$quality, main = "Quality of white wines", breaks=5,  xlab ='Quality', col="gray")
hist(dfwnew2_test$quality, main = "Quality of white wines", breaks=5,  xlab ='Quality', col="gray")

install.packages("neuralnet")
library("neuralnet")

memory.limit(size=4095)

nn1model <- neuralnet(quality ~ alcohol + total.acidity + pH + sugar, data = dfwnew2_train)

#we create the model in NN with quality versus alcohol, total.acidity and sugar
nn1model <- neuralnet(quality ~ alcohol + total.acidity + pH + sugar, data = dfwnew2_train)
#visualize the results
plot(nn1model)
#we have to normalize the dataset so we introduce
#the function normalize
normalize <- function(x) {
return((x - min(x)) / (max(x) - min(x)))
}
#we use it to normalize the dfwnew2 dataset
dfw_norm1 <- as.data.frame(lapply(dfwnew2, normalize))
#now we can see the normalization in all variables (fields)
summary(dfw_norm1$quality)
summary(dfw_norm1)
#we create the training and testing sets
dfw_norm1_train <- dfw_norm1[1:1736, ]
### this command does not work : dfw_norm1_test <- dfw_norm1[1737, 2315]
# this works :
dfw_norm1_test <- dfw_norm1[1737:2315, ]
#we create the NN model with quality vs. all others
n_train <- names(dfw_norm1_train)
f_train <- as.formula(paste("quality ~", paste(n_train[!n_train %in% "quality"], collapse = " + ")))
#we construct the models with 1 neuron for each layer
nn_model1 <- neuralnet(f_train,data=dfw_norm1_train,linear.output=T)
plot(nn_model1)
# two neurons for each layer
nn_model2 <- neuralnet(f_train,data=dfw_norm1_train,hidden=2,linear.output=T)
plot(nn_model2)
# same way for the 3,4,5 neurons

#we make a 5 , 3 neurons hiddens with two layers 
nn_model53 <- neuralnet(f_train,data=dfw_norm1_train,hidden=c(5,3),linear.output=T)
plot(nn_model53)

#we generate predictions on the testing dataset
model_results_1 <- compute(nn_model1, dfw_norm1_test[,1:8])
# same way for 3,4,5 neurons

#the  compute() function works a bit differently from the  predict()
#functions we've used so far. It returns a list with two components:  $neurons , which
#stores the neurons for each layer in the network, and  $net.results , which stores
#the predicted values

#let's see the latter
predicted_quality <- model_results_1$net.result
cor(predicted_quality, dfw_norm1_test$quality)
#the correlation here of about 0.74 indicates a fairly strong relationship

#let's try with 1 neuron per layer
nn_model1 <- neuralnet(f_train,data=dfw_norm1_train,linear.output=T)
model_results_1 <- compute(nn_model1, dfw_norm1_test[,1:8])

#let's try with 2 neurons per layer
nn_model2 <- neuralnet(f_train,data=dfw_norm1_train,hidden=2,linear.output=T)
model_results_2 <- compute(nn_model2, dfw_norm1_test[,1:8])

#let's try with 3 neurons per layer
nn_model3 <- neuralnet(f_train,data=dfw_norm1_train,hidden=3,linear.output=T)
model_results_3 <- compute(nn_model3, dfw_norm1_test[,1:8])

predicted_quality3 <- model_results_3$net.result
cor(predicted_quality3, dfw_norm1_test$quality)
#we have minor value 0.71 vs. 0.74

#try with 2 neurons per layer
nn_model2 <- neuralnet(f_train,data=dfw_norm1_train,hidden=2,linear.output=T)
model_results2 <- compute(nn_model2, dfw_norm1_test[,1:8])
predicted_quality2 <- model_results2$net.result
cor(predicted_quality2, dfw_norm1_test$quality)
#worst value 0.60 vs.0.74 

### we find the MAPE errors...
nn_model1 <- neuralnet(f_train,data=dfw_norm1_train,linear.output=T)

# for the nn with 1 neuron
pr.nn1 <- compute(nn_model1,dfw_norm1_test[,1:8])
pr.nn_11 <- pr.nn1$net.result*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
test.r_11 <- (dfw_norm1_test$quality)*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
# finding MSE and MAPE
MSE.nn11 <- sum((test.r_11 - pr.nn_11)^2)/nrow(dfw_norm1_test)
print(MSE.nn11)
MAPE.nn11 <- sum(abs(test.r_11 - pr.nn_11))/nrow(dfw_norm1_test)
print(MAPE.nn11)

# for the nn with 2 neurons
nn_model2 <- neuralnet(f_train,data=dfw_norm1_train,hidden=2,linear.output=T)

pr.nn2 <- compute(nn_model2,dfw_norm1_test[,1:8])
pr.nn_22 <- pr.nn2$net.result*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
### test.r_11 <- (dfw_norm1_test$quality)*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
# finding MSE and MAPE
MSE.nn22 <- sum((test.r_11 - pr.nn_22)^2)/nrow(dfw_norm1_test)
print(MSE.nn22)
MAPE.nn22 <- sum(abs(test.r_11 - pr.nn_22))/nrow(dfw_norm1_test)
print(MAPE.nn22)

# for the nn with 3 neurons
nn_model3 <- neuralnet(f_train,data=dfw_norm1_train,hidden=3,linear.output=T,stepmax=1e6)

pr.nn3 <- compute(nn_model3,dfw_norm1_test[,1:8])
pr.nn_33 <- pr.nn3$net.result*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
### test.r_11 <- (dfw_norm1_test$quality)*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
# finding MSE and MAPE
MSE.nn33 <- sum((test.r_11 - pr.nn_33)^2)/nrow(dfw_norm1_test)
print(MSE.nn33)
MAPE.nn11 <- sum(abs(test.r_11 - pr.nn_33))/nrow(dfw_norm1_test)
print(MAPE.nn33)

# for the nn with 4 neurons
nn_model4 <- neuralnet(f_train,data=dfw_norm1_train,hidden=4,linear.output=T,stepmax=1e6)

pr.nn4 <- compute(nn_model4,dfw_norm1_test[,1:8])
pr.nn_44 <- pr.nn4$net.result*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
### test.r_11 <- (dfw_norm1_test$quality)*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
# finding MSE and MAPE
MSE.nn44 <- sum((test.r_11 - pr.nn_44)^2)/nrow(dfw_norm1_test)
print(MSE.nn44)
MAPE.nn44 <- sum(abs(test.r_11 - pr.nn_44))/nrow(dfw_norm1_test)
print(MAPE.nn44)

# for the nn with 5 neurons
nn_model5 <- neuralnet(f_train,data=dfw_norm1_train,hidden=5,linear.output=T,stepmax=1e6)

pr.nn5 <- compute(nn_model5,dfw_norm1_test[,1:8])
pr.nn_55 <- pr.nn5$net.result*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
### test.r_11 <- (dfw_norm1_test$quality)*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
# finding MSE and MAPE
MSE.nn55 <- sum((test.r_11 - pr.nn_55)^2)/nrow(dfw_norm1_test)
print(MSE.nn11)
MAPE.nn55 <- sum(abs(test.r_11 - pr.nn_55))/nrow(dfw_norm1_test)
print(MAPE.nn55)

# for the nn with 5,3 neurons
nn_model53 <- neuralnet(f_train,data=dfw_norm1_train,hidden=c(5,3),linear.output=T,stepmax=1e6)

pr.nn53 <- compute(nn_model53,dfw_norm1_test[,1:8])
pr.nn_53 <- pr.nn53$net.result*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
### test.r_11 <- (dfw_norm1_test$quality)*(max(dfw_norm1$quality)-min(dfw_norm1$quality))+min(dfw_norm1$quality)
MSE.nn53 <- sum((test.r_11 - pr.nn_53)^2)/nrow(dfw_norm1_test)
print(MSE.nn53)
MAPE.nn53 <- sum(abs(test.r_11 - pr.nn_53))/nrow(dfw_norm1_test)
print(MAPE.nn53)

