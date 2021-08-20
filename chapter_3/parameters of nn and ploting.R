library(neuralnet)
library(plyr)

CreditCardnn <- read.csv("https://raw.githubusercontent.com/621-Group2/Final-Project/master/UCI_Credit_Card.csv")


#Normalize dataset
maxValue <- apply(CreditCardnn, 2, max)
minValue <- apply(CreditCardnn, 2, min)

CreditCardnn <- as.data.frame(scale(CreditCardnn, center = minValue, scale = maxValue - minValue))

#Rename to target variable
colnames(CreditCardnn)[25] <- "target"


smp <- floor(0.70 * nrow(CreditCardnn))
set.seed(4784)

CreditCardnn$ID <- NULL
train_index <- sample(seq_len(nrow(CreditCardnn)), size = smp, replace = FALSE)

train_nn <- CreditCardnn[train_index, ]
test_nn <- CreditCardnn[-train_index, ]

allVars <- colnames(CreditCardnn)
predictorVars <- allVars[!allVars%in%'target']
predictorVars <- paste(predictorVars, collapse = "+")
f <- as.formula(paste("target~", predictorVars, collapse = "+"))

nueralModel <- neuralnet(formula = f, hidden = c(4,2), linear.output = T, data = train_nn)

plot(nueralModel)

# Which gives the following error:
#   
#   Error in plot.nn(nueralModel) : weights were not calculated 

## Answer to the problem ->.............................................................

# Before the error you report, most probably you also got a warning:

# > nueralModel <- neuralnet(formula = f, hidden = c(4,2), linear.output = T, data = train_nn)
# Warning message:
#   algorithm did not converge in 1 of 1 repetition(s) within the stepmax 
# 
# This message is important, effectively warning you that your neural network did not converge. Given this message, the error further downstream, when you try to plot the network, is actually expected:
#   
# > plot(nueralModel)
# Error in plot.nn(nueralModel) : weights were not calculated
# 
# Looking more closely into your code & data, it turns out that the problem lies in your choice for linear.output = T in fitting your neural network; from the docs:
#   
#   linear.output logical. If act.fct should not be applied to the output neurons set linear output to TRUE, otherwise to FALSE.
# 
# Keeping a linear output in the final layer of a neural network is normally used in regression settings only; in classification settings, such as yours, the correct choice is to apply the activation function to the output neuron(s) as well. Hence, trying the same code as yours but with linear.output = F, we get:
#   
# > nueralModel <- neuralnet(formula = f, hidden = c(4,2), linear.output = F, data = train_nn) # no warning this time
# > plot(nueralModel)