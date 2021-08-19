#### Principal Component Analysis and Factor Analysis in R----

#### install.packages("REdaS")----
library(REdaS)
library(psych)
#### define the dataset----
# mydata<- read.csv("pca_gsp.csv")
# attach(mydata)
# newdata <- read.csv("test_data.csv", sep = ";", dec = ",")
#### Define variables----
# X <- cbind(Ag, Mining, Constr, Manuf, Manuf_nd, Transp, Comm, Energy, TradeW, TradeR, RE, Services, Govt)

#### Descriptive statistics----

# X <- newdata # import dataset
# X <- AB
# summary(X)
# cor(X)
################################################################################
# using linear algebra finding cov and centered matrix----
# d <- read.csv("AB_matrix_2012.csv");d
# data <- d; data
# data <- X; dim(data)
data <- df
# Create a function that creates a new data frame with centered variables----
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

# Apply the function finding centered matrix----
data_centered <- center_apply(data); head(data_centered)
# find the cov matrix----
data_centered_cov <- cov(data_centered); head(data_centered_cov)

# Calculate the eigenvalues of the matrix----
data_centered_eigen <- eigen(data_centered_cov);# head(data_centered_eigen)
################################################################################
#### Principal component analysis using princomp----
X <- df;
pca1 <- princomp(X, scores=TRUE, cor=F)

summary(pca1)

pca1.var <- pca1$sdev^2
pca1.var.per <- round(pca1.var/sum(pca1.var)*100, digits = 2); #pca1.var.per

head(pca1.var.per)
# -pca1$scores # converting for compatibility with prcomp
# -pca1$scores*(sqrt(9/10)) # for compatibility with prcomp * sqrt(n-1/n)
# head(pca1$loadings)

fviz_eig(pca1)
fviz_pca_ind(pca1)
fviz_pca_var(pca1)
fviz_pca_biplot(pca1)

mydata1 <- data.frame(pca1$scores[ ,c(1,2)]); mydata1

ggplot(data=mydata1, aes(x=Comp.1, y=-Comp.2, label=rownames(mydata1))) +
  geom_text() +
  xlab(paste("PC1 - ", pca1.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca1.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle(" USA 2019 princomp")# we change axis Y
################################################################################
#### using prcomp----
X <- df
pca2 <- prcomp(X,scale=F)

summary(pca2)
head(pca2$x)
head(pca2$sdev)
head(pca2$rotation)

fviz_eig(pca2)
fviz_pca_ind(pca2)
fviz_pca_var(pca2)
fviz_pca_biplot(pca2)

# Eigenvalues----
eig.val <- get_eigenvalue(pca2)
head(eig.val)

# Results for Variables----
res.var <- get_pca_var(pca2)
summary(pca2)

head(res.var$coord)          # Coordinates
head(res.var$contrib)        # Contributions to the PCs
head(res.var$cos2)           # Quality of representation 
# Results for individuals----
res.ind <- get_pca_ind(pca2)
head(res.ind$coord)          # Coordinates
head(res.ind$contrib)        # Contributions to the PCs
head(res.ind$cos2)           # Quality of representation 

mydata2 <- data.frame(pca2$x[ ,c(1,2)]); head(mydata2)

ggplot(data=mydata2, aes(x=-PC1, y=PC2, label=rownames(mydata2))) +
  geom_text() +
  xlab(paste("PC1 - ", round(res.ind$contrib[1]*100, digits = 2), "%", sep="")) +
  ylab(paste("PC2 - ", round(res.ind$contrib[2]*100, digits = 2), "%", sep="")) +
  theme_bw() +
  ggtitle(" USA 2012 prcomp") # we change the axis X
################################################################################
#### using PCA----
pca3 <- PCA(X, scale.unit = F)
summary(pca3)
head(pca3$var)
head(pca3$eig)
head(pca3$svd)

fviz_eig(pca3)
fviz_pca_ind(pca3)
fviz_pca_var(pca3)
fviz_pca_biplot(pca3)

mydata3 <- data.frame(pca3$ind$coord[ ,c(1,2)]); mydata3

ggplot(data=mydata3, aes(x=Dim.1, y=Dim.2, label=rownames(mydata3))) +
  geom_text() +
  xlab(paste("PC1 -",round(pca3$eig[1,2],digits = 2) ,"%")) +
  ylab(paste("PC2 -",round(pca3$eig[2,2],digits = 2) ,"%")) +
  theme_bw() +
  ggtitle(" USA 2012 PCA")
################################################################################
#### using principal SOMETHING HERE GOES WRONG----

pca4 <- principal(X, nfactors = 5, rotate = "none", covar=T)
pca4$scores
pca4$values
pca4$loadings

mydata4 <- data.frame(pca4$scores[ ,c(1,2)]); mydata4

ggplot(data=mydata4, aes(x=PC1, y=PC2, label=rownames(mydata4))) +
  geom_text() +
  xlab(paste("PC1 - %")) +
  ylab(paste("PC2 - %")) +
  theme_bw() +
  ggtitle(" USA 2007 principal")

################################################################################
#### Scree plot of eigenvalues using princomp----
plot(pca1, ylim=c(0,0.03))

screeplot(pca1, type="line", main="Scree Plot 2012", ylim=c(0,0.02))
summary(pca1)
#### Biplot of score variables----
# biplot(pca1)
#### Scores of the components----
# pca1$scores[1:3,]
#### plotting using prcomp----
mydata <- data.frame(pca2$x[ ,c(1,2)]); head(mydata)

ggplot(data=mydata, aes(x=-PC1, y=PC2, label=rownames(mydata))) +
  geom_text() +
  xlab(paste("PC1 - ", pca1.var.per[1], "%")) +
  ylab(paste("PC2 - ", pca1.var.per[2], "%")) +
  theme_bw() +
  ggtitle(" USA 2002 ") # we change the axis X

#### Rotation----
v1 <- varimax(pca1$loadings[,c(1:3)]);v1
p1 <- promax(pca1$loadings[,c(1:3)]);p1

bar <- eigen(cov(X))  ## cov()
bar$values <- sqrt(bar$values)
# head(bar)
################################################################################
#### Factor analysis ----
fa1 <- factanal(X, factor=3, rotation="none")
fa1

fa2 <- factanal(X, factor=3, rotation="varimax")
fa2

fa3 <- factanal(X, factors=3, rotation="varimax", scores="regression")
fa3
fa3$scores

fa4 <- factanal(X, factor=3, rotation="promax")
fa4

# KMO Statistics and Bartlett's Test of Sphericity
## install.packages("REdaS")
# library(REdaS)
KMOS(X)
bart_spher(X)

detach(mydata)

################################################################################
#### PCA with linear algebra ----
# https://davetang.org/muse/2012/02/01/step-by-step-principal-components-analysis-using-r/
# https://stats.stackexchange.com/questions/90331/step-by-step-implementation-of-pca-in-r-using-lindsay-smiths-tutorial
#### dataset----
# d = data.frame(x=c(2.5,0.5,2.2,1.9,3.1,2.3,2.0,1.0,1.5,1.1),
#                y=c(2.4,0.7,2.9,2.2,3.0,2.7,1.6,1.1,1.6,0.9))
# d <- read.csv("test_new.csv",sep = ";",dec=","); d

d <- newdata;d
d <- AB;head(d)
d <- data.frame(AB);head(d)
#### mean-adjusted values ----
# d$x_adj = d$X1 - mean(d$X1)
# d11 <- d$X - mean(d$X)
# # d$y_adj = d$X2 - mean(d$X2)
# d22 <- d$Y - mean(d$Y)
# # d$z_adj = d$X3 - mean(d$X3)

#### calculate covariance matrix and eigenvectors/values----
round((cm = cov(d[,1:3])),digits = 6)
cov(d)
round(cov(d), digits = 6)

#### outputs cov matrix#############
#          x         y
# x 0.6165556 0.6154444
# y 0.6154444 0.7165556
#### eigenvalues ######################
cm <- cov(d)
e = eigen(cm);e

round(e$values, digits =6 )
round(e$vectors, digits = 6)
#### outputs eigenvectors & values----
# $values
# [1] 1.2840277 0.0490834
#
# $vectors
#          [,1]       [,2]
# [1,] 0.6778734 -0.7351787
# [2,] 0.7351787  0.6778734
################################################################################
#### principal component vector slopes----
s1 = e$vectors[1,1] / e$vectors[2,1] # PC1
s2 = e$vectors[1,2] / e$vectors[2,2] # PC2

# plot(d11, d22, asp=T, pch=16, xlab='x', ylab='y')
# abline(a=0, b=s1, col='red')
# abline(a=0, b=s2)

# PCA data = rowFeatureVector (transposed eigenvectors) * RowDataAdjust (mean adjusted, also transposed)
feat_vec = t(e$vectors); feat_vec

# row_data_adj = t(d[,4:5])# equals to data_centered
# final_data = data.frame(t(feat_vec %*% row_data_adj)) # ?matmult for details
# data centered and PC----
data_centered

final_data <- data.frame((data_centered)%*%t(feat_vec))
head(final_data)
final_data2 <- data.frame((data_centered)%*%(e$vectors))
head(final_data2)
# final_data = data.frame(t(row_data_adj %*% feat_vec )) # ?matmult for details
# str(row_data_adj)
names(final_data) = c(1:70)
final_data # the PCs
#### outputs PC ###############
# final_data
#              x           y
# 1   0.82797019 -0.17511531
# 2  -1.77758033  0.14285723
# 3   0.99219749  0.38437499
# 4   0.27421042  0.13041721
# 5   1.67580142 -0.20949846
# 6   0.91294910  0.17528244
# 7  -0.09910944 -0.34982470
# 8  -1.14457216  0.04641726
# 9  -0.43804614  0.01776463
# 10 -1.22382056 -0.16267529
############################
# final_data[[1]] = -final_data[[1]] # for some reason the x-axis data is negative the tutorial's result
#### plotting with final data----
plot(final_data[, 1], final_data[,2],  xlab="PCA 1", ylab="PCA 2", pch=16)

ggplot(data=final_data, aes(x=final_data[, 1], y=final_data[,2], label=rownames(final_data))) +
  geom_text() +
  xlab(paste("PC1 - ", pca1.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca1.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("2002 w. linear algebra, center=TRUE)")

# converting pca for viewing compatibility with prcomp----
conv_final_data <- -final_data; conv_final_data

ggplot(data=conv_final_data, aes(x=conv_final_data[, 1], y=conv_final_data[,2], label=rownames(conv_final_data))) +
  geom_text() +
  xlab(paste("PC1 - ", pca1.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca1.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("conv_final_w. linear algebra, center=TRUE)")
#############################################################################
# transpose data NOT VALID----
trans_data = final_data
trans_data[,2] = 0

row_orig_data = t( t(feat_vec) %*% t(trans_data) )
plot(row_orig_data, asp=T, pch=16, cex=.5)
abline(a=0, b=s1, col='red')
#############################################################################
# lefteris data----
d2 <- read.csv("left_data.csv");d2

d3 <- data.frame(-d2$Y, d2$X);d3
colnames(d3) <- c("X1","X2"); d3

ggplot(data=d2, aes(x=X, y=Y, label=rownames(d2))) +
  geom_text() +
  xlab(paste("PC1 - ", round(pca3$var$cos2[1,1]*100,digits = 2), "%", sep="")) +
  ylab(paste("PC2 - ", round(pca3$var$cos2[1,2]*100,digits = 2), "%", sep="")) +
  theme_bw() +
  ggtitle("d2 w. linear algebra, center=TRUE)")

ggplot(data=d3, aes(x=X2, y=X1, label=rownames(d3))) +
  geom_text() +
  xlab(paste("PC1 - ", round(pca3$var$cos2[1,1]*100,digits = 2), "%", sep="")) +
  ylab(paste("PC2 - ", round(pca3$var$cos2[1,2]*100,digits = 2), "%", sep="")) +
  theme_bw() +
  ggtitle("d3 w. linear algebra, center=TRUE)")

d4 <- read.csv("pc_2012_rminer.csv"); d4

# d4$pc1 <- round(d4$pc1, digits = 0)
# d4$pc2 <- round(d4$pc2, digits = 0)

ggplot(data=d4, aes(x=pc1, y=pc2, label=rownames(d4))) +
  geom_text() +
  xlab("PC1 - %") +
  ylab("PC2 - %") +
  theme_bw() +
  ggtitle("d4 w. linear algebra, center=TRUE)")

plot(d4$pc1, d4$pc2)
################################################################################
#### another approach----
#### use a simple two dimensional dataset to illustrate PCA----
x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)

# (x1,x2)={(1,2),(3,3),(3,5),(5,4),(5,6),(6,5),(8,7),(9,8)}

x11 <- c(1,3,3,5,5,6,8,9)
y11 <- c(2,3,5,4,6,5,7,8)

X11 <- data.frame(x11,y11);X11

plot(X11$x11, X11$y11)
cov(X11)

plot(x, y, pch = 19)
X <- data.frame(AB);X
#### scale initial dataset----
mean(X$X1)
# [1] 1.81
mean(X$X2)
# [1] 1.91
mean(X$X3)

new_X <- scale(X); new_X

x1 <- X$X1 - mean(X$X1)
x1
# [1]  0.69 -1.31  0.39  0.09  1.29  0.49  0.19 -0.81 -0.31 -0.71
summary(x1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.310  -0.610   0.140   0.000   0.465   1.290

y1 <- X$X2 - mean(X$X2)
y1
# [1]  0.49 -1.21  0.99  0.29  1.09  0.79 -0.31 -0.81 -0.31 -1.01

z1 <- X$X3 - mean(X$X3)
z1
summary(y1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.210  -0.685  -0.010   0.000   0.715   1.090
# centered matrix----
df <- data.frame(x1,y1,z1) #we can put them in a data frame this is centered matrix

D <- as.matrix(df);D

plot(x1, y1, pch = 19)
#### calculating COV----
# direct way of calculating cov
s1 <- sum(x1*y1);s1
cov_x1_y1 <- s1/3; cov_x1_y1 # 0.6154444

cov(x1, y1)
#[1] 0.6154444

cov(x1, x1)
#[1] 0.6165556

cov(y1, y1)
#[1] 0.7165556

m <- matrix(c(cov(x1, x1), cov(x1, y1), cov(y1, x1),cov(y1, y1)),
            nrow=2,
            ncol=2,
            byrow=TRUE,
            dimnames=list(c("x","y"),c("x","y")))

m
#           x         y
# x 0.6165556 0.6154444
# y 0.6154444 0.7165556

C <-  1/3*t(D) %*% D; C

m1 <- cov(df) ; m1 #is the same
#### calculating eigenvectors & eigenvalues----
e <- eigen(C)
e
# eigen() decomposition
# $values
# [1] 1.2840277 0.0490834
# 
# $vectors
#           [,1]       [,2]
# [1,] 0.6778734 -0.7351787
# [2,] 0.7351787  0.6778734
#### calculating PC----
pc1 <- x1 * e$vectors[1,1] + y1 * e$vectors[2,1]
pc1
#  [1]  0.82797019 -1.77758033  0.99219749  0.27421042  1.67580142  0.91294910 -0.09910944 -1.14457216 -0.43804614
# [10] -1.22382056

pc2 <- x1 * e$vectors[1,2] + y1 * e$vectors[2,2]
pc2
#  [1] -0.17511531  0.14285723  0.38437499  0.13041721 -0.20949846  0.17528244 -0.34982470  0.04641726  0.01776463
# [10] -0.16267529

data.frame(PC1 = pc1, PC2 = pc2)
#            PC1         PC2
# 1   0.82797019 -0.17511531
# 2  -1.77758033  0.14285723
# 3   0.99219749  0.38437499
# 4   0.27421042  0.13041721
# 5   1.67580142 -0.20949846
# 6   0.91294910  0.17528244
# 7  -0.09910944 -0.34982470
# 8  -1.14457216  0.04641726
# 9  -0.43804614  0.01776463
# 10 -1.22382056 -0.16267529
#### plotting----
plot(pc1, pc2, pch = 19)
################################################################################
#### using prcomp----
data <- data.frame(x,y)
data.pca <- prcomp(X)
data.pca
# Standard deviations (1, .., p=2):
# [1] 1.1331495 0.2215477
# 
# Rotation (n x k) = (2 x 2):
#          PC1        PC2
# x -0.6778734  0.7351787
# y -0.7351787 -0.6778734

names(data.pca)
# [1] "sdev"     "rotation" "center"   "scale"    "x"

data.pca$x
#              PC1         PC2
# [1,] -0.82797019  0.17511531
# [2,]  1.77758033 -0.14285723
# [3,] -0.99219749 -0.38437499
# [4,] -0.27421042 -0.13041721
# [5,] -1.67580142  0.20949846
# [6,] -0.91294910 -0.17528244
# [7,]  0.09910944  0.34982470
# [8,]  1.14457216 -0.04641726
# [9,]  0.43804614 -0.01776463
#[10,]  1.22382056  0.16267529

plot(data.pca$x[,1], data.pca$x[,2], pch = 19)

eigen(m)
# eigen() decomposition
# $values
# [1] 1.2840277 0.0490834
# 
# $vectors
#           [,1]       [,2]
# [1,] 0.6778734 -0.7351787
# [2,] 0.7351787  0.6778734

data.pca
summary(data.pca)
# Standard deviations (1, .., p=2):
# [1] 1.1331495 0.2215477
# 
# Rotation (n x k) = (2 x 2):
#          PC1        PC2
# x -0.6778734  0.7351787
# y -0.7351787 -0.6778734
################################################################################