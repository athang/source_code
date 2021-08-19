# Viewing as time series quality vs. dates of production 
# setwd("~/PhD/2020 mar paper")
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
# install.packages("timelineR")
library(timelineR)
# install.packages("grid")
library(grid)
# library(ggplot2)
# install.packages("scales")
library(scales)
# install.packages("lubridate")
library(lubridate)
library(dplyr)
# install.packages("zoo")
library(zoo)
# install.packages("hrbrthemes")
library(hrbrthemes)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("rstatix", dependencies = TRUE)
library(rstatix)
#### import the dataset ####
# we import the dataset of white wines 
mydataset <- read.csv("tsantalis_white_005.csv", header=T, sep=",")
# we keep as _orig the original dataset
mydataset_orig <- mydataset
## mydataset <- mydataset_orig
str(mydataset)
summary(mydataset)
# boxplot(mydataset) #useless just for getting an image
# selecting variables
mydataset <- mydataset[ , c(2,4:11,13,14)]
# renaming variables
names(mydataset) <- c("qual","date", "alc", "ph","ta","va","fe","sug","col","fsd","tsd")
mydataset <- mydataset[ , c(1:6,8:11)]
# removing all NA records
mydataset <- na.omit(mydataset)
mydataset2 <- mydataset[ , c(2,1,3:10)]
str(mydataset2)
mydataset <- mydataset2

dfq <- dfq[ -c(489,1307,1641), ] # delete false outliers
rownames(dfq) <- NULL # reset rownames
str(dfq)
################################################################################
#### converting to date format ####
str(mydataset2$date)
betterDates <- as.Date(mydataset2$date, format="%d/%m/%Y")
hist(betterDates)
num_dates <- as.numeric(betterDates)
str(num_dates)
hist(num_dates)
norm_dates <- as.Date(num_dates, origin = "1970-01-01")
str(norm_dates)

mydataset3 <- data.frame(norm_dates, mydataset2[ , c(2:10)])
View(mydataset3); str(mydataset3)

# mydataset3$norm_dates <- as.POSIXct(mydataset3$norm_dates)

data_cols3 <- mydataset3[ ,c(1, 2)]
View(data_cols3)  ; str(data_cols3)
################################################################################
#### graphics histograms####
plot_grob <- plot_timeline(data_cols3)
# plot(mydataset3$qual ~ mydataset3$norm_dates, type="l")

ggplot(data = mydataset3, aes(x = norm_dates, y = qual)) + stat_smooth()+ 
  geom_line()+
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Total daily quality wine series",
       subtitle = "2004-2014",
       x = "Date", y = "Daily Quality")

qplot(x=norm_dates, y=qual,
       data=data_cols3, 
       main="wines quality",
       xlab="Date", ylab="quality")

p1 <- ggplot(data_cols3, aes(norm_dates, qual)) + stat_smooth(colour="green") +
  geom_point(na.rm=TRUE, color="purple", size=1) + 
  ggtitle("wine quality") +
  xlab("Dates") + ylab("quality")
p1

data_cols4 <- data.frame(mydataset3$norm_dates, mydataset3$qual)
names(data_cols4) <- c("norm_dates", "qual")
p <- ggplot(data_cols4, aes(norm_dates,qual)) + xlab("") +geom_point() + stat_smooth(colour="green")
p
p2 <- p + scale_x_date(date_labels = "%b")
p2
p + scale_x_date(date_labels = "%b/%Y")
p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)
# plot.ts( data_cols4$norm_dates , data_cols4$qual , type="l")
data_cols5 <- data_cols4[order(norm_dates), ]
plot(data_cols5, type="b")

myts1 <- ts(data_cols5$qual, frequency=104, start=c(2004,6))
myts1
plot.ts(myts1)
################################################################################
#### selecting period of time ####
# Define Start and end times for the subset as R objects that are the time class
startTime <- as.Date("2005-06-01")
endTime <- as.Date("2006-06-01")
# create a start and end time R object
start.end <- c(startTime,endTime)
start.end

## [1] "2004-06-01" "2008-06-01"

# View data for 2011 only
# We will replot the entire plot as the title has now changed.
p6 <- ggplot(mydataset3, aes(norm_dates, qual)) + geom_line() +
  geom_point(na.rm=TRUE, color="purple", size=1) + geom_smooth() +
  ggtitle("wine quality") +
  xlab("Date") + ylab("quality")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%b %y")))

p6
################################################################################
# grouping by year ----

mydataset3$year <- year(mydataset3$norm_dates)

mydataset3 %>% 
  group_by(year) %>%  # group by year
  tally() # count measurements per year

table(mydataset3$year)

#### saving datasets ####
write.csv(mydataset3, file = "mydata_w_dates_v3.csv", row.names = F)
# write.csv(data_cols5, file = "dates_qual.csv", row.names = F)
################################################################################
# import dataset with varieties ----
mydataset4 <- read.csv("white_wines_w_dates_varieties.csv")
mydataset4 <- mydataset4[ , -11]
str(mydataset4)
summary(mydataset4)
mydataset4 <- na.omit(mydataset4)
mydataset4$eidos <- as.factor(mydataset4$eidos)
table(mydataset4$eidos)
plot(mydataset4$eidos,ylim=c(0,300),  main="Histogram of Varieties")
mydataset4$eid_num <- as.numeric(as.character(mydataset4$eidos))
p3 <- prop.table(table(mydataset4$eid_num))
hist(mydataset4$eid_num)
################################################################################
# reading csv----
mydf <- read.csv("mydata_w_dates_v3.csv")
# making Chi square test----
table(mydf$qual,mydf$year)
chi_1 <- chisq.test(mydf$qual,mydf$year); chi_1
chi_1$observed
round(chi_1$expected,2)
round(chi_1$residuals, 3)
# correlation plots----
corrplot(chi_1$residuals, is.cor = FALSE)
corrplot(chi_1$observed, is.cor = FALSE)
corrplot(chi_1$expected, is.cor = FALSE)
# contribution ----
contrib <- 100*chi_1$residuals^2/chi_1$statistic
round(contrib, 3)
# Visualize the contribution----
corrplot(contrib, is.cor = FALSE)
chi_1$p.value
chi_1$estimate
# import csv ----
dfq <- read.csv("qual_vs_years_03.csv", sep=";",dec=",")
plot(dfq$aa, dfq$qual)

dfq_orig <- dfq
# converting to Date format----
dfq$norm_dates <- as.Date(dfq$norm_dates)
str(dfq)
# plotting ----
plot(dfq$norm_dates, dfq$qual)
barplot(dfq$qual)

ggplot( data = dfq, aes( norm_dates, qual )) + geom_line() 

plot(qual ~ norm_dates, dfq, xaxt = "n", type = "l")
axis(1, dfq$norm_dates, format(dfq$norm_dates, "%b %Y"), cex.axis = .7)

# install.packages("zoo")
# library(zoo)
plot(zoo(dfq$qual,dfq$norm_dates))

plot(zoo(dfq$qual,as.Date(dfq$norm_dates,"%d/%m/%Y")),
     xlab="Time", ylab="Concentration (ppb)",
     main="Time trend of Quality")

plot(dfq$qual~as.Date(dfq$norm_dates,"%d/%m/%Y"),type="l",
     xlab="Time", ylab="Concentration (ppb)",
     main="Time trend of Quality")
################################################################################
# grouping by year----
#### selecting period of time ####
# Define Start and end times for the subset as R objects that are the time class
startTime <- as.Date("2005-01-01")
endTime <- as.Date("2005-12-31")
# create a start and end time R object
start.end <- c(startTime,endTime)
start.end

## [1] "2004-06-01" "2008-06-01"

# View data for 2011 only
# We will replot the entire plot as the title has now changed.
p6 <- ggplot(dfq, aes(norm_dates, qual)) + geom_line() +
  geom_point(na.rm=TRUE, color="purple", size=1) + geom_smooth() +
  ggtitle("wine quality") +
  xlab("Date") + ylab("quality")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%b %y")))

p6

# plotting points ----
# plot the data using ggplot
ggplot(data = dfq, aes(x = norm_dates, y = qual)) +
  geom_point() +
  labs(x = "Date",
       y = "Quality",
       title = "Qual vs Time",
       subtitle = "...")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%b %y")))

# with barplots ----
# quickly plot the data and include a title using main = ""
# use '\n' to force the string to wrap onto a new line

ggplot(data = dfq, aes(x = norm_dates, y = qual)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Qual vs Time",
       subtitle = "2005",
       x = "Date", y = "Quality")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%b %y")))

ggplot(dfq, aes(x = norm_dates, y = qual)) +            # Draw ggplot2 plot
  geom_line() + 
  scale_x_date(date_labels = "%Y-%m")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%b %y")))

# using interval----
# install.packages("hrbrthemes")
# library(hrbrthemes)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot----
p <- ggplot(dfq, aes(x=norm_dates, y=qual)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2007-01-01"),as.Date("2007-03-31")),breaks=date_breaks("1 month"),labels=date_format("%b %y")) +
  ylim(3,10)

p
################################################################################
# finding means of qual per year ----

aggregate(dfq[, 11], list(dfq$year), mean)

aggregate(x = dfq$qual,                # Specify data column
          by = list(dfq$year),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

################################################################################
# selection by year ----
df05 <- subset(dfq, dfq$year == 2009) # selection with criteria
# df05 <- dfq
mydate <- df05$norm_dates; myq <- df05$qual
mydata05 <- data.frame(mydate, myq)

# Get center line (avergae)
x_bar <- mean(mydata05$myq);x_bar
# Get sigma (standard deviation)
sigma05 <- sd(mydata05$myq); sigma05
table(mydata05$myq) # see the histogram with table
# LCL (Lower Control Limit) Column
LCL <- rep((x_bar - (3*sigma05)), length(mydata05$mydate))
# -2 sigma05 Column
lower_two_sigma05 <- rep((x_bar - (2*sigma05)), length(mydata05$mydate))
# -1 sigma05 Column
lower_one_sigma05 <- rep((x_bar - (1*sigma05)), length(mydata05$mydate))
# +1 sigma05 Column
upper_one_sigma05 <- rep((x_bar + (1*sigma05)), length(mydata05$mydate))
# +2 sigma05 Column
upper_two_sigma05 <- rep((x_bar + (2*sigma05)), length(mydata05$mydate))
# UCL (Upper Control Limit) Column
UCL <- rep((x_bar + (3*sigma05)), length(mydata05$mydate))
# Add control limits columns to mydata05 frame
mydata05 <- cbind(mydata05, LCL, lower_two_sigma05, lower_one_sigma05, x_bar, upper_one_sigma05, upper_two_sigma05, UCL)
################################################################################
# Build control chart----
plot(mydata05$myq,
     type = "b",
     pch = 16,
     axes = FALSE,
     main = "Control Chart : 2004 - 2013",
     xlab = "Day",
     ylab = "Quality",
     xlim = c(0, length(mydata05$mydate) + 1),
     ylim = c(x_bar - (3.5*sigma05), x_bar + (3.5*sigma05)))
axis(1,
     at = 1:length(mydata05$mydate),
     cex.axis = 0.6)
axis(2)
box()
abline(h = x_bar, lwd = 1)
abline(h = UCL, lwd = 1, col = "#7e7e7e", lty = 2)
abline(h = upper_two_sigma05, lwd = 1, col = "#7e7e7e", lty = 3)
abline(h = upper_one_sigma05, lwd = 1, col = "#7e7e7e", lty = 3)
abline(h = lower_one_sigma05, lwd = 1, col = "#7e7e7e", lty = 3)
abline(h = lower_two_sigma05, lwd = 1, col = "#7e7e7e", lty = 3)
abline(h = LCL, lwd = 1, col = "#7e7e7e", lty = 2)
text(1, x_bar, "Mean", pos = 2)
text(1, x_bar + (1*sigma05), (expression(paste("+1", sigma))), pos = 2)
text(1, x_bar + (2*sigma05), (expression(paste("+2", sigma))), pos = 2)
text(1, x_bar + (3*sigma05), (expression(paste("+3", sigma))), pos = 2)
text(1, x_bar - (1*sigma05), (expression(paste("-1", sigma))), pos = 2)
text(1, x_bar - (2*sigma05), (expression(paste("-2", sigma))), pos = 2)
text(1, x_bar - (3*sigma05), (expression(paste("-3", sigma))), pos = 2)

################################################################################
# histograms and graphics----
hist(mydata05$myq)
table(mydata05$myq)

qplot(mydata05$myq,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Quality 2005", 
      xlab = "Quality",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.3),
      xlim=c(3,11))

ggplot(mydata05, aes(myq)) +
  geom_histogram(bins=7)

ggplot(mydata05, aes(myq))+
  geom_bar() +
  scale_x_binned()

p<-ggplot(mydata05, aes(x=myq)) + 
  geom_histogram(color="black", fill="white")
p

ggplot(mydata05, aes(x=myq)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=8, alpha=.2)+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Quality plot 2004-2013",x="Quality", y = "Percentage")+
  geom_vline(xintercept=x_bar, color="red")
  theme_classic()

hist(mydata05$myq, 
     main="Histogram for Quality 2005", 
     xlab="quality", 
     border="blue", 
     col="green",
     xlim=c(3,11),
     las=1,
     breaks=9)

ggp <- ggplot(dfq, aes(year)) +    # Draw histogram
  geom_histogram(bins = 10)
ggp

ggplot(dfq, aes(x=year)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=10, alpha=.2)+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Wines per year",x="Quality", y = "Percentage")+
theme_classic()
################################################################################
# making t-test or anova for means of years----

means_1 <- c(7.2,7.15,6.86,6.72,6.42,6.25,6.48,6.31,6,6.04)
years_1 <- c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013)
years_1 <- as.factor(years_1)

mydf1 <- data.frame(means_1, years_1)

res.aov <- aov(means_1 ~ years_1, data = mydf1)
summary(res.aov)

t.test(means_1, mu=6.56)

# Box plots----
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")

dfq2 <- dfq[ ,c(3,12)]
dfq2$year <- as.factor(dfq2$year)

ggboxplot(dfq2, x = "year", y = "qual", 
          color = "year", 
          ylab = "Quality", xlab = "Years",
          main="all years boxplots")

# plotting the means
ggline(dfq2, x = "year", y = "qual", 
       add = c("mean_se", "jitter"), 
       ylab = "quality", xlab = "years",
       main="All years means")
# anova ----
# Compute the analysis of variance
res.aov <- aov(qual ~ year, data = dfq2)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

# summary stats----
dfq2 %>%
  group_by(year) %>%
  get_summary_stats(qual, type = "mean_sd")

# boxplots----
ggboxplot(dfq2, x = "year", y = "qual", main="all years boxplots")

# outliers----
dfq2 %>% 
  group_by(year) %>%
  identify_outliers(qual)

# normality----
# Build the linear model
model  <- lm(qual ~ year, data = dfq2)
# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model)) # NO NORMALITY

# normality by groups----
dfq2 %>%
  group_by(year) %>%
  shapiro_test(qual)  # NO NORMALITY

ggqqplot(dfq2, "qual", facet.by = "year")

plot(model, 1)

# levene test for variance homogeneity----
dfq2 %>% levene_test(qual ~ year) # there is significant difference in variances
# no homogeneity p < 0.05

# computation of anova----
res.aov <- dfq2 %>% anova_test(qual ~ year)
res.aov

# Pairwise comparisons----
pwc <- dfq2 %>% tukey_hsd(qual ~ year)
pwc
write.csv(pwc, file="tukey_table.csv", row.names = FALSE)

# visualization----
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "year")
ggboxplot(dfq2, x = "year", y = "qual") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# with no homogeneity ----
res.aov2 <- dfq2 %>% welch_anova_test(qual ~ year)

# Pairwise comparisons (Games-Howell) ONLY if there is homogeneity
# pwc2 <- PlantGrowth %>% games_howell_test(weight ~ group)
# # Visualization: box plots with p-values
# pwc2 <- pwc2 %>% add_xy_position(x = "group", step.increase = 1)
# ggboxplot(PlantGrowth, x = "group", y = "weight") +
#   stat_pvalue_manual(pwc2, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(res.aov2, detailed = TRUE),
#     caption = get_pwc_label(pwc2)
#   )

# with no homogeneity ----
pwc3 <- dfq2 %>% 
  pairwise_t_test(
    qual ~ year, pool.sd = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc3
