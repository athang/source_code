## In this example, the data is in a matrix called
## data.matrix
## columns are individual samples (i.e. cells)
## rows are measurements taken for all the samples (i.e. genes)
## Just for the sake of the example, here's some made up data...
# install packages----
library(ggplot2)
# define dataset----
# data.matrix <- matrix(nrow=100, ncol=10)
# colnames(data.matrix) <- c(
#   paste("wt", 1:5, sep=""),
#   paste("ko", 1:5, sep=""))
# rownames(data.matrix) <- paste("gene", 1:100, sep="")
# for (i in 1:100) {
#   wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
#   ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
#  
#   data.matrix[i,] <- c(wt.values, ko.values)
# }
# head(data.matrix)
# dim(data.matrix)
# import the dataset ----
df <- read.csv("datasets/2002/AB_matrix_2002_70X70_new_3.csv", header = T, sep=";",dec=",")
str(df); summary(df)
data.matrix <- as.matrix(df)
head(data.matrix);dim(df); summary(data.matrix); 
# using PRCOMP----
# pca <- prcomp(t(data.matrix), scale. =T) # maybe not correct

pca <- prcomp((data.matrix), scale. =FALSE) 

summary(pca)

pca$x[,1] <- -pca$x[,1]  # for some reason x must be converted to -x
 
## plot pc1 and pc2----
plot(pca$x[,1], pca$x[,2], main = "2019")  
 
## make a scree plot----
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, digits = 2); pca.var.per
 
barplot(pca.var.per, main="Scree Plot 2002", xlab="Principal Components", ylab="Percent Variation", ylim = c(0,30), cex.axis = 1.5)
 
## now make a fancy looking plot that shows the PCs and the variation:----
# library(ggplot2)
 
pca.data <- data.frame(Sample=rownames(df),
  X=pca$x[,1],
  Y=pca$x[,2])
head(pca.data)
 
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA 2019")

# write.csv(pca.data, file="PCs_2019_01.csv", row.names = FALSE)

## get the name of the top 10 measurements (genes) that contribute----
## most to pc1.
# loading_scores <- pca$rotation[,1]
# gene_scores <- abs(loading_scores) ## get the magnitudes
# gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
# top_10_genes <- names(gene_score_ranked[1:10])
#  
# top_10_genes ## show the names of the top 10 genes
#  
# pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)

## NOTE: Everything that follow is just bonus stuff.
## It simply demonstrates how to get the same
## results using "svd()" (Singular Value Decomposition) or using "eigen()"
## (Eigen Decomposition).
################################################################################
## Now let's do the same thing with svd() SOMETHING HERE GOES WRONG----
##
## svd() returns three things
## v = the "rotation" that prcomp() returns, this is a matrix of eigenvectors
##     in other words, a matrix of loading scores
## u = this is similar to the "x" that prcomp() returns. In other words,
##     sum(the rotation * the original data), but compressed to the unit vector
##     You can spread it out by multiplying by "d"
## d = this is similar to the "sdev" value that prcomp() returns (and thus
##     related to the eigen values), but not
##     scaled by sample size in an unbiased way (ie. 1/(n-1)).
##     For prcomp(), sdev = sqrt(var) = sqrt(ss(fit)/(n-1))
##     For svd(), d = sqrt(ss(fit))
##
#svd----
# svd.stuff <- svd(scale(t(data.matrix), center=TRUE)) # not containing 0 (zero)
svd.stuff <- svd(data.matrix)

## calculate the PCs----
svd.data <- data.frame(
  X=(svd.stuff$u[,1] * svd.stuff$d[1]),
  Y=(svd.stuff$u[,2] * svd.stuff$d[2]),
  Z=(svd.stuff$u[,3] * svd.stuff$d[3]))

head(svd.data)
 
## alternatively, we could compute the PCs with the eigen vectors and the----
## original data
# svd.pcs <- t(t(svd.stuff$v) %*% t(scale(t(data.matrix), center=TRUE)))# no ZERO
svd.pcs <- (t(svd.stuff$v) %*% t(data.matrix))

head(svd.pcs[,1:2]) ## the first two principal components
 
svd.df <- ncol(data.matrix) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var/sum(svd.var)*100, 1); svd.var.per
# we change X to -X 
ggplot(data=svd.data, aes(x=-X, y=Y, label=rownames(svd.data))) +
  geom_text() +
  xlab(paste("PC1 - ", svd.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", svd.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("svd(data.matrix) - 2007")
 
################################################################################
## Now let's do the same thing with eigen() SOMETHING HERE GOES WRONG----
##
## eigen() returns two things...
## vectors = eigen vectors (vectors of loading scores)
##           NOTE: pcs = sum(loading scores * values for sample)
## values = eigen values
##
# finding cov ----
# 
# cov.mat <- cov(scale(t(data.matrix), center=TRUE)); head(cov.mat)# maybe FALSE
cov.mat <- cov(scale((data.matrix), center=TRUE)); head(cov.mat)

dim(cov.mat)
 
## since the covariance matrix is symmetric, we can tell eigen() to just
## work on the lower triangle with "symmetric=TRUE"
# dinfing eigenvectors----
eigen.stuff <- eigen(cov.mat, symmetric=TRUE)
dim(eigen.stuff$vectors)
head(eigen.stuff$vectors[,1:2])
 
eigen.pcs <- t(t(eigen.stuff$vectors) %*% t(scale(t(data.matrix), center=TRUE))) # maybe FALSE
eigen.pcs <- t(t(eigen.stuff$vectors) %*% t(scale((data.matrix), center=T)))

head(eigen.pcs[,1:2])
 
eigen.data <- data.frame(
  X=(-1 * eigen.pcs[,1]), ## eigen() flips the X-axis in this case, so we flip it back
  Y=eigen.pcs[,2]) ## X axis will be PC1, Y axis will be PC2
head(eigen.data)
 
eigen.var.per <- round(eigen.stuff$values/sum(eigen.stuff$values)*100, 1)
# plotting Pc1, PC2----
ggplot(data=eigen.data, aes(x=X, y=Y, label=rownames(eigen.data))) +
  geom_text() +
  xlab(paste("PC1 - ", eigen.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", eigen.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("eigen on cov(t(data.matrix))")
################################################################################
# https://rpubs.com/cbolch/531355
# another approach by RPUBS----
################################################################################
# Before standardizing the dataset, need to drop the date column----

data <- data[-c(1)]
data <- df; head(data); dim(data)
# Create a function that creates a new data frame with centered variables----
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

# Apply the function----
data_centered <- center_apply(data); head(data_centered)
# find the cov matrix----
data_centered_cov <- cov(data_centered); head(data_centered_cov)

# Calculate the eigenvalues of the matrix----
data_centered_eigen <- eigen(data_centered_cov); #head(data_centered_eigen)

# Structure of the object contains the ordered eigenvalues and the corresponding eigenvector matrix
str(data_centered_eigen)

PVE <- data_centered_eigen$values / sum(data_centered_eigen$values)
# percentage of eigenvalues----
round(PVE, 2)

n <- length(PVE);n

# PVE plot (aka scree plot)----

PVEplot <- qplot(c(1:n), PVE) + 
  geom_line() +
  xlab("Principal Component") +
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0,1)

PVEplot
# Cumulative PVE plot----
cumPVE <- qplot(c(1:n), cumsum(PVE)) +
  geom_line() +
  xlab("Principal Component") +
  ylab(NULL) +
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

cumPVE
library(ggpubr)
library(gridExtra)
# plotting the two graphics together----
grid.arrange(PVEplot, cumPVE, ncol = 2) # the two scree plots
################################################################################
# https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff
# another approach (tumor data) using prcomp ----
# wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
d <- df;
wdbc.pr <- prcomp(d, center = TRUE, scale = F) # maybe the correct is scale=F
summary(wdbc.pr)
# making scree plot----
screeplot(wdbc.pr, type = "l", npcs = 5, main = "Screeplot of the first 5 PCs - 2012")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

# plotting pc1 and pc2 as X and Y----
plot(-wdbc.pr$x[,1],wdbc.pr$x[,2], xlab="PC1 ", ylab = "PC2 ", main = "PCA 2002")

# library("factoextra") # PLOTTING PC ----
# fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
#              pointsize = 2, 
#              fill.ind = wdbc$diagnosis, 
#              col.ind = "black", 
#              palette = "jco", 
#              addEllipses = TRUE,
#              label = "var",
#              col.var = "black",
#              repel = TRUE,
#              legend.title = "Diagnosis") +
#   ggtitle("2D PCA-plot from 30 feature dataset") +
#   theme(plot.title = element_text(hjust = 0.5))
# plotting and biplotting ----
fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("PCA-plot 2002") +
  theme(plot.title = element_text(hjust = 0.5))

fviz_pca_biplot(wdbc.pr)
# head(wdbc.pr$x)
# END ----