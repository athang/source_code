# Clusters and Plots for envelope curves of USA wages----
# import packages####
library(nnet)
library(corrplot)
library(olsrr)
library(ggplot2)
library(cluster)
library(factoextra)
# install.packages("animation")	
library(animation)
library(tidyr)
library(RColorBrewer)
library(tidyverse)  # data manipulation
# install.packages("pvclust")
library(pvclust)
# install.packages("mclust")
library(mclust)
# install.packages("fpc")
library(fpc)
# install.packages("NbClust",dependencies = TRUE)
library(NbClust)

# import dataset ####
# set working directory
setwd("~/PhD/lefteris_project/envelope curves of usa")
# import dataset
# newdata <- read.csv("pca_2019.csv")
# newdata <- newdata[, 2:4]

newdata <- pca.data[,2:3]
newdata_orig <- newdata
# newdata <- newdata[ , c(2:71)]
str(newdata)
summary(newdata)
# write.csv(newdata,"data_2002_prices.csv")
# input <- newdata[,2:3]
input <- newdata
# scaling data
scaled_data <- scale(input)
# simple plot####
ggplot(newdata, aes(x=X, y=Y, label=rownames(newdata))) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA Graph - USA 2019")

# all three methods to find best number of clusters #########################
# https://predictivehacks.com/how-to-determine-the-number-of-clusters-of-k-means-in-r/

# library(factoextra)
# library(NbClust)

# Elbow method
# fviz_nbclust(scaled_data, kmeans, method = "wss") +
#   geom_vline(xintercept = 3, linetype = 2)+
#   labs(subtitle = "Elbow method")

# Silhouette method ----
fviz_nbclust(scaled_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method  2002")

fviz_nbclust(input, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method . 2019")

# Gap statistic----
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(scaled_data, kmeans, nstart = 25,  method = "gap_stat", nboot = 200)+
#   labs(subtitle = "Gap statistic method")
# 
# fit10 <- kmeans(input, centers = 4); fit10
# fit11 <- kmeans(scaled_data, centers = 4);fit11

# visualizing 
# fviz_cluster(kmeans(input, centers = 2), geom = "point", data = input)
# 
# fviz_cluster(kmeans(scaled_data, centers = 2), geom = "point", data = scaled_data)

# finding best k value of clusters (TDS)--------
# https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967 #

# As the initial centroids are defined randomly,
# we define a seed for purposes of reprodutability
set.seed(123)

# Let's remove the column with the names, so it won't be used in the clustering
# input <- newdata[,2:3]

# The nstart parameter indicates that we want the algorithm to be executed 20 times.
# This number is not the number of iterations, it is like calling the function 20 times and then
# the execution with lower variance within the groups will be selected as the final result.

# kmeans(input, centers = 4, nstart = 20)

#### elbow method -----------------------------------
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group", main="Scree plot 2019")
  }

wssplot(input, nc = 20)
wssplot(scaled_data, nc = 20)

#### we try different k with no negative values ####
# set.seed(123)
# clustering <- kmeans(input, centers = 2, nstart = 20)
# clustering
#### silhouette method -------------------------------
# sil <- silhouette(clustering$cluster, dist(input))
# fviz_silhouette(sil)

#### other methods selecting number of clusters ################################
# https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/

# load required packages

#### Elbow method ------------------------------------
# fviz_nbclust(input, kmeans, method = "wss") +
#   geom_vline(xintercept = 2, linetype = 2) + # add line for better visualisation
#   labs(subtitle = "Elbow method") # add subtitle

#### Silhouette method --------------------------------
# fviz_nbclust(input, kmeans, method = "silhouette") +
#   labs(subtitle = "Silhouette method with unscaled data")

#### Gap statistic ####
# set.seed(123)
# fviz_nbclust(input, kmeans,
#              nstart = 25,
#              method = "gap_stat",
#              nboot = 500
# ) + # reduce it for lower computation time (but less precise results)
#   labs(subtitle = "Gap statistic method")

#### comparing many methods ####
# input <- newdata[,2:3]
# input2 <- scale(input)

nbclust_out <- NbClust(
  data = input,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 7, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)
par(mfrow=c(1,1))

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 5)
table(nbclust_plot)
#### create barplot of clusters number ####
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

#### checking silhouette method####
set.seed(123)
km_res <- kmeans(input, centers = 3, nstart = 20); km_res
km_res$size
sil <- silhouette(km_res$cluster, dist(input))
fviz_silhouette(sil)

#### plotting clusters ####
fviz_cluster(km_res, input, ellipse.type = "norm", main="Clusters 2019")
fviz_cluster(km_res, input , main="Clusters 2019", xlab = "PC1-...%", ylab = "PC2-...%")

m1 <- newdata[km_res$cluster==1, ]
rownames(m1);dim(m1)
m2 <- newdata[km_res$cluster==2, ]
rownames(m2);dim(m2)
m3 <- newdata[km_res$cluster==3, ]
rownames(m3);dim(m3)
#### END #######################################################################
#### another approach ----
# https://www.guru99.com/r-k-means-clustering.html #

set.seed(123)
#### animate clusters####
# kmeans.ani(input, 3)
# 
# pc_cluster <-kmeans(input, 3); pc_cluster
# 
# kmean_withinss <- function(k) {
#   cluster <- kmeans(input, k)
#   return (cluster$tot.withinss)
# }

#### elbow method ####
kmean_withinss(3)

# Set maximum cluster 
max_k <-10 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss);elbow

#### Plot the elbow graph with ggplot####
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))
# number of clusters ----
pc_cluster <-kmeans(input, 3)

pc_cluster$cluster
pc_cluster$centers
pc_cluster$size	

center <-pc_cluster$centers; center

#### create dataset with the cluster numbers####
cl1 <- newdata[pc_cluster$cluster==1, ]; cl1
cl2 <- newdata[pc_cluster$cluster==2, ]; cl2
cl3 <- newdata[pc_cluster$cluster==3, ]; cl3

cl1$cl_num <- "2019_1"
cl2$cl_num <- "2019_2"
cl3$cl_num <- "2019_3"

all_cl_2019 <- rbind(cl1, cl2, cl3)
table(all_cl_2019$cl_num)
all_cl_2019$cl_num <- factor(all_cl_2019$cl_num)
summary(all_cl_2019)
# writing in .csv file
write.csv(all_cl_2019, "all_cl_2019_new.csv", row.names = T)
# plotting every cluster individually ----
# plot(all_cl_2019$X,all_cl_2019$Y)
# 
# plot(cl1$X,cl1$Y, main="cluster 1")
# plot(cl2$X,cl2$Y, main="cluster 2")
# plot(cl3$X,cl3$Y, main="cluster 3")

#### plotting clusters with color ####
ggplot(all_cl_2019, aes(x = X, y = Y, shape = cl_num, colour = cl_num)) +
  scale_shape_manual(values = c(1,2,3)) +
  scale_colour_brewer(palette = "Set1") +
  geom_text(aes(label=rownames(all_cl_2019))) +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  ggtitle("PCA 2019")

#### cluster plot ####
# input <- newdata[,2:3]
# raw data
set.seed(123)
final <- kmeans(input, 3, nstart = 25)
print(final)

g1 <- fviz_cluster(final, data = input, main="cluster plot 2019", xlab = (paste("PC1 :",pca.var.per[1],"%")), ylab=(paste("PC2 :",pca.var.per[2],"%"))); g1

final$size

#### with scaled data ####

scaled_data <- scale(input)

set.seed(123)
final <- kmeans(scaled_data, 3, nstart = 25)
print(final)

g1 <- fviz_cluster(final, data = scaled_data, main="cluster plot 2007 scaled"); g1

final$size

#### similarities ####
# finding similarities, distance and plot clusters
df <- newdata
distance <- get_dist(df)
# cross value similarity visualization
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(input, centers = 3, nstart = 25);k2

fviz_cluster(k2, data = input, main="cluster plot 2019")
k2$size
#### END #######################################################################
#### another approach comparing methods of cluster's choices ###################
# https://www.r-bloggers.com/2017/02/finding-optimal-number-of-clusters/
#### with raw data #### finding the best number of clusters----
nb <- NbClust(input, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=7, method = "kmeans", 
              index = "all", alphaBeale = 0.1)

table(nb$Best.partition)

par(mfrow=c(1,1))

hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

#Let us apply kmeans for k clusters 
kmm = kmeans(input,3,nstart = 50,iter.max = 15) 
#we keep number of iter.max=15 to ensure the algorithm converges and nstart=50 to 
#ensure that at least 50 random sets are chosen  
kmm
kmm$size
# plotting clusters----
g1 <- fviz_cluster(kmm, data = input, main="cluster plot 2007"); g1

#### with scaled data ####
# scaled_data <- scale(input)
#D index and Hubert for number of clusters proposal 
nb <- NbClust(scaled_data, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=7, method = "kmeans", 
              index = "all", alphaBeale = 0.1)

table(nb$Best.partition)

par(mfrow=c(1,1))

hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

#Let us apply kmeans for k clusters 
kmm = kmeans(scaled_data,3,nstart = 50,iter.max = 15) 
#we keep number of iter.max=15 to ensure the algorithm converges and nstart=50 to 
#ensure that at least 50 random sets are chosen  
kmm
kmm$size
# plotting clusters----
g2 <- fviz_cluster(kmm, data = scaled_data, main="cluster plot 2007"); g2

summary(input); hist(input$X)
summary(scaled_data); hist(scaled_data)
#### END #######################################################################
#### from this point we must have factor associated (NOT NECCESSARY) ###########

# cluster <- c(1:2)
# center_df <- data.frame(cluster, pc_cluster$centers)

# Reshape the data
# 
# center_reshape <- gather(center_df, features, values, X: Y)
# head(center_reshape)
# 
# # Create the palette
# hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
# 
# # Plot the heat map
# ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
#   scale_y_continuous(breaks = seq(1, 4, by = 1)) +
#   geom_tile() +
#   coord_equal() +
#   scale_fill_gradientn(colours = hm.palette(90)) +
#   theme_classic()

#### END #######################################################################
#### another approach with distance (NOT NECCESSARY) ############################################
# https://uc-r.github.io/kmeans_clustering #
# optimum number of clusters----
# elbow method
set.seed(123)
# function to compute total within-cluster sum of square---- 
wss <- function(k) {
  kmeans(input, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15----
k.values <- 1:15
# extract wss for 2-15 clusters----
# problem ???
wss_values <- map_dbl(k.values, wss)
# plot number of clusters----
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#### Silhouette method####
set.seed(123)

fviz_nbclust(input, kmeans, method = "silhouette")

#### wss method####
fviz_nbclust(input, kmeans, method = "wss")

#### silhouette method####
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(input, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15----
k.values <- 2:15

# extract avg silhouette for 2-15 clusters----
# problem ???
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#### compute gap statistic####
set.seed(123)
gap_stat <- clusGap(input, FUN = kmeans, nstart = 2,
                    K.max = 15, B = 50)
#### Print the result####
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)
#### END ######################################################################
#### another approach  with dendrogram (SOMETHING HERE GOES WRONG) ########################################
# https://www.statmethods.net/advstats/cluster.html

# scaling dataset----
mydata_sc <- scale(input)
plot(mydata_sc)

mydata <- input
plot(mydata)

# Ward Hierarchical Clustering----
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit, main="Dendrogram 2019") # display dendogram
groups <- cutree(fit, k=3) # cut tree into k clusters
# draw dendogram with red borders around the k clusters
rect.hclust(fit, k=3, border="red") 

# Ward Hierarchical Clustering with Bootstrapped p values----
# library(pvclust)
# fit2 <- pvclust(mydata, method.hclust="ward.D2",method.dist="euclidean")
# plot(fit2) # dendogram with p values PROBLEM ???
# add rectangles around groups highly supported by the data
# pvrect(fit2, alpha=.95)  # PROBLEM ???
################################################################################
#### Determine number of clusters####
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
# dendrogram ----

# sub ---------------------------------------------------------------------


#### subsub ------------------------------------------------------------------


# K-Means Cluster Analysis====
fit <- kmeans(mydata,3) # k cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster) 
plot(mydata$X, mydata$Y)
#### Dendrograms #####################
#### Ward Hierarchical Clustering ####
d <- dist(mydata, method = "euclidean") # distance matrix
fit3 <- hclust(d, method="ward.D2")
plot(fit3, main = "Cluster dendrogram for 2007") # display dendogram
groups <- cutree(fit3, k=3) # cut tree into 4 clusters
#### draw dendogram with red borders around the 4 clusters####
rect.hclust(fit3, k=3, border="red") 

#### Ward Hierarchical Clustering with Bootstrapped p values####
fit4 <- pvclust(mydata, method.hclust="ward.D2",
               method.dist="euclidean")
plot(fit4) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit4, alpha=.95) 

#### Model Based Clustering####
mydata <- input
fit5 <- Mclust(mydata)
# plot(fit) # plot results
summary(fit5) # display the best model 

#### K-Means Clustering with 3 or 4 clusters####
fit <- kmeans(mydata, 3)
fit1 <- kmeans(mydata,2); fit2 <- kmeans(mydata, 3)

# Cluster Plot against 1st 2 principal components----
# vary parameters for most readable graph

clusplot(mydata, fit$cluster, color=TRUE, shade=T,
         labels=2, lines=4)

# Centroid Plot against 1st 2 discriminant functions----
fit$size
plotcluster(mydata, fit$cluster) 

mydata[fit$cluster==1,]
#### comparing 2 cluster solutions####

cluster.stats(d, fit1$cluster, fit2$cluster) 
#### END #######################################################################
#### hierarchical with dendrogram (NN) ####
# https://www.r-bloggers.com/2016/01/hierarchical-clustering-in-r-2/

par(mfrow=c(1,1))

clusters1 <- hclust(dist(input))
clusters2 <- hclust(dist(scaled_data))

plot(clusters1)
plot(clusters2)
clusterCut <- cutree(clusters2, 3); clusterCut ; table(clusterCut)
# table(clusterCut, iris$Species)

clusters <- hclust(dist(input), method = 'average')
plot(clusters)

#### function clusterplot (NN) ####
# https://www.r-bloggers.com/2019/07/learning-data-science-understanding-and-using-k-means-clustering/
# clusterplot <- function(M, C, txt) {
#   plot(M, main = txt, xlab = "", ylab = "")
#   for(i in 1:n) {
#     points(C[i, , drop = FALSE], pch = 23, lwd = 3, col = colors[i])
#     points(M[A == i, , drop = FALSE], col = colors[i])
#   }
# }
# cl <- kmeans(input, 3)
# clusterplot(input, cl$centers, "Base R") # problem ?

#### dendrogram (NN)############################################################
# https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html

k.means.fit <- kmeans(input, 3) # k = 4

k.means.fit$size
# plotting clusters----
clusplot(input, k.means.fit$cluster, main='Clusters 2012',
         color=TRUE, shade=F,
         labels=2, lines=0)

wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }

wssplot(input, nc=6) 

# table(input[,1],k.means.fit$cluster)

# hierarchical dendrogram ----
d <- dist(input, method = "euclidean") # Euclidean distance matrix.
d2 <- dist(scaled_data, method = "euclidean") # Euclidean distance matrix.

H.fit <- hclust(d, method="ward.D2")
H.fit2 <- hclust(d2, method="ward.D2")

plot(H.fit, main="Cluster dendrogram 2007") # display dendogram
plot(H.fit2, main="Cluster dendrogram 2007") # display dendogram

groups <- cutree(H.fit, k=3) # cut tree into 4 clusters
groups2 <- cutree(H.fit2, k=3) # cut tree into 4 clusters

table(groups)
table(groups2)

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 
rect.hclust(H.fit2, k=3, border="red") 

###### END of clustering ----
###### plotting sets of years together ----
#### import dataset ####
# set working directory
setwd("~/PhD/lefteris_project/envelope curves of usa")
# import dataset
df <- read.csv("pca_2012_ab_2.csv")

df_orig1 <- df
# df_orig <- df[71:280, ]
df_orig <- df
df_orig$year <- as.factor(df_orig$year)

# df <- df_orig
# newdata <- newdata[ , c(2:71)]
#### assign different sets ####
df1 <- df_orig[c(1:10, 71:80), ]
df2 <- df_orig[c(11:20, 81:90), ]
df3 <- df_orig[c(21:30, 91:100), ]
df4 <- df_orig[c(31:40, 101:110), ]
df5 <- df_orig[c(41:50, 111:120), ]
df6 <- df_orig[c(51:60, 121:130), ]
df7 <- df_orig[c(61:70, 131:140), ]

df1 <- df_orig[c(1:10, 71:80, 141:150), ]
df2 <- df_orig[c(11:20, 81:90, 151:160), ]
df3 <- df_orig[c(21:30, 91:100, 161:170), ]
df4 <- df_orig[c(31:40, 101:110, 171:180), ]
df5 <- df_orig[c(41:50, 111:120, 181:190), ]
df6 <- df_orig[c(51:60, 121:130, 191:200), ]
df7 <- df_orig[c(61:70, 131:140, 201:210), ]

df1 <- df_orig[c(1:10, 71:80, 141:150, 211:220), ]
df2 <- df_orig[c(11:20, 81:90, 151:160, 221:230), ]
df3 <- df_orig[c(21:30, 91:100, 161:170, 231:240), ]
df4 <- df_orig[c(31:40, 101:110, 171:180, 241:250), ]
df5 <- df_orig[c(41:50, 111:120, 181:190, 251:260), ]
df6 <- df_orig[c(51:60, 121:130, 191:200, 261:270), ]
df7 <- df_orig[c(61:70, 131:140, 201:210, 271:280), ]

str(df_orig)
summary(df_orig)
# write.csv(newdata,"data_2002_prices.csv")
# make factor the year
# df_orig$year <- as.factor(df_orig$year)
#### scaling the variables####
df$X <- scale(df$X)
df$Y <- scale(df$Y)
#### various plots ####
# simple plot----
ggplot(data=df, aes(x=X, y=Y)) +
  geom_text(aes(label=Sample, colour=year)) +
  scale_colour_brewer(palette = "Set1") +
  xlab("PC1") +
  ylab("PC2") +
  theme_bw() +
  ggtitle("My PCA Graph - 2019")

ggplot(df1, aes(x = x, y = y, shape = year, colour = year)) + 
  geom_text(aes(label = var))

#### best plot----
ggplot(df7, aes(x = x, y = y, shape = year, colour = year)) +
  scale_shape_manual(values = c(1,2,3,4)) +
  scale_colour_brewer(palette = "Set1") +
  geom_text(aes(label=var)) +
  labs(
    title = "Plot 2002 vs. 2007 vs. 2012 vs. 2019",
    subtitle = "61-70",
    x = "PC-1",
    y = "PC-2"
  ) 

# with path----
ggplot(df1, aes(x = x, y = y, shape = year, colour = year)) +
  scale_shape_manual(values = c(1,2,3)) +
  scale_colour_brewer(palette = "Set1") +
  geom_text(aes(label=var)) +
  geom_path() +
  labs(
    title = "Plot 2007 vs. 2012 vs. 2019",
    subtitle = "01-10",
    x = "x-values",
    y = "y-values"
  ) 
# END###########################################################################
# from Datacamp ----

x <- pca.data[ ,2:3]

# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(123)

# km.out <- kmeans(x, centers=3, nstart=1)

for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(x, centers=3, nstart=1)
  
  # Plot clusters
  plot(x, col = km.out$cluster, 
       main = km.out$tot.withinss)
}
# END########################################################################
# elbow method ----
# Initialize total within sum of squares error: wss
par(mfrow=c(1,1))
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Set k equal to the number of clusters corresponding to the elbow location
k <- 2  # 3 is probably OK, too
# END#########################################################################
# pokemon example ----

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 2

# Build model with k clusters: km.out
km.out <- kmeans(pokemon, centers = 2, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")