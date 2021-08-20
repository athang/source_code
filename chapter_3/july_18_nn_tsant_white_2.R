#set working directory
setwd("~/Έγγραφα/phd/paper_july_18/R_code")

#we import the whole dataset 
load("~/Έγγραφα/phd/paper_july_18/R_code/data3.RData")
#we get the whole dataset
### data3 <- read.csv("data3.csv", header=T, sep=",")
### save(data3, file="data3.RData") # in case we want to save in .RData format
#we get only the white wines
dfwnew <- subset(data3, colour == "white")
str(dfwnew)
#we remove the "colour" field
dfwnew2 <- dfwnew[,-2]
### View(dfwnew2)
str(dfwnew2)
#we save our dataset
write.csv(dfwnew2, file="dfwnew2.csv", row.name=T)
save(dfwnew2, file="dfwnew2.RData")
#loading various libraries
#maybe need to be installed with install.packages("...")

#some graphical libraries
### install.packages("descr")
library("descr") 
library("ggplot2")

library("scales")
library("MASS")
#show the quality histogram
hist(dfwnew2$quality, main = "Quality of white wines with frequency", breaks=5,  xlab ='Quality', col="gray")
truehist(dfwnew2$quality, h = 1, col="slategray3", main = "Quality of white wines with percentages") # with percentages

#we install the package "neuralnet" for Neural Networks
### install.packages("neuralnet")
library("neuralnet")

### memory.limit(size=4095) # for windows OS only
### memory.size() # for windows OS only
ls()
gc() # view the memory limits
#visualize the results

#import the function from Github for using plot.nnet
# install.packages("devtools")
# library(devtools)
# install.packages("reshape")
# library(reshape)
# source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

### plot.nnet(nn1model) ### maybe get error
### better give the following command
# nueralModel <- neuralnet(formula = f, hidden = c(4,2), linear.output = F, data = train_nn) # no warning this time
# plot(nueralModel)

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

#we divide the dataset into two datasets
#the training set consisting of 75% of the whole dataset and the rest 25% the test set

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dfw_norm1$quality, SplitRatio = 3/4)
dfw_norm1_train = subset(dfw_norm1, split == TRUE)
dfw_norm1_test = subset(dfw_norm1, split == FALSE)

# viewing summaries and structures
str(dfw_norm1_train); summary(dfw_norm1_train)
str(dfw_norm1_test); summary(dfw_norm1_test)
hist(dfw_norm1_train$quality, main = "Quality of training set of white wines", breaks=5,  xlab ='Quality', col="gray")
hist(dfw_norm1_test$quality, main = "Quality of testing set of white wines", breaks=5,  xlab ='Quality', col="gray")

#training session

#we create the NN model with quality vs. all others
n_train <- names(dfw_norm1_train)
f_train <- as.formula(paste("quality ~", paste(n_train[!n_train %in% "quality"], collapse = " + ")))

#we construct the models with 1 neuron for each layer
nn_model1 <- neuralnet(f_train,data=dfw_norm1_train,linear.output=T)
summary(nn_model1)
### plot.nnet(nn_model1)

### dev.print(pdf, 'nn_model_1.pdf') ### saving in .pdf format
### plot(nn_model1)
### dev.off()
plot(nn_model1)

# one neuron for each layer with linear.output=T we do regression else classification

# testing session

#we generate predictions on the testing dataset
model_results_1 <- compute(nn_model1, dfw_norm1_test[,2:9])
#the  compute() function works a bit differently from the  predict()
#functions we've used so far. It returns a list with two components:  $neurons , which
#stores the neurons for each layer in the network, and  $net.results , which stores
#the predicted values
#let's see the latter
predicted_quality <- model_results_1$net.result
# summary(predicted_quality)
# View(dfw_norm1)
# View(dfw_norm1_train)
# View(dfw_norm1_test)

### Validation of results

cor(predicted_quality, dfw_norm1_test$quality)
#the correlation with 1 neuron here is about 0.69 that indicates a fairly strong relationship

### with two neurons....................................................................

# two neurons for each layer
nn_model2 <- neuralnet(f_train,data=dfw_norm1_train,hidden=2,linear.output=T)
# summary(nn_model2)
### dev.off()
plot(nn_model2)

# testing session 
model_results_2 <- compute(nn_model2, dfw_norm1_test[,2:9])

predicted_quality_2 <- model_results_2$net.result

cor(predicted_quality_2, dfw_norm1_test$quality)
#the correlation with 2 neurons here is about 0.72 that indicates a fairly strong relationship

#let's try with 3 neurons per layer ..................................................
nn_model3 <- neuralnet(f_train,data=dfw_norm1_train,hidden=3,linear.output=T, stepmax = 1e6)
summary(nn_model3)
# testing session
model_results_3 <- compute(nn_model3, dfw_norm1_test[,2:9]) 
predicted_quality_3 <- model_results_3$net.result 
### memory.size() # maybe problem with the memory

cor(predicted_quality_3, dfw_norm1_test$quality)

#the correlation with 3 neurons here is about 0.74 that indicates a fairly strong relationship

#let's try with 4 neurons per layer ..................................................
nn_model4 <- neuralnet(f_train,data=dfw_norm1_train,hidden=4,linear.output=T, stepmax = 1e6)
# summary(nn_model4)
# testing session
model_results_4 <- compute(nn_model4, dfw_norm1_test[,2:9]) 
predicted_quality_4 <- model_results_4$net.result 
### memory.size() # maybe problem with the memory

cor(predicted_quality_4, dfw_norm1_test$quality)

#the correlation with 4 neurons here is about 0.77 that indicates a fairly strong relationship

#let's try with 5 neurons per layer ..................................................
nn_model5 <- neuralnet(f_train,data=dfw_norm1_train,hidden=5,linear.output=T, stepmax = 1e6)
# summary(nn_model5)
# testing session
model_results_5 <- compute(nn_model5, dfw_norm1_test[,2:9]) 
predicted_quality_5 <- model_results_5$net.result 
### memory.size() # maybe problem with the memory

cor(predicted_quality_5, dfw_norm1_test$quality)

#the correlation with 5 neurons here is about 0.76 that indicates a fairly strong relationship