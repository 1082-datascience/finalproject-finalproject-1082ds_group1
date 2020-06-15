#
#refer to "https://www.kaggle.com/shravank/predicting-breast-cancer-using-pca-lda-in-r"

library('tidyverse')
library('caret')
library('ggcorrplot')
library('GGally')
library('dplyr')

wdbc <- read.csv("data.csv", stringsAsFactors = F) 

glimpse(wdbc)


# first collect all the 30 numeric variables into a matrix
# Convert the features of the data: wdbc.data
wdbc.data <- as.matrix(wdbc[,c(3:32)])

# Set the row names of wdbc.data
row.names(wdbc.data) <- wdbc$id

# Create diagnosis vector
diagnosis <- as.numeric(wdbc$diagnosis == "M")


# Questions:
# How many observations have benign or malignant diagnosis ?
table(wdbc$diagnosis)

# What is the mean of each of the numeric columns ?
round(colMeans(wdbc.data),2)

# What is the sd of each of the numeric columns ?
roundSD <- function(x){
  round(sd(x), 2)
}
apply(wdbc.data, 2, roundSD)

# How are the variables related to each other ?
library(corrplot)

corMatrix <- wdbc[,c(3:32)]

# Rename the colnames
cNames <- c("rad_m","txt_m","per_m",
            "are_m","smt_m","cmp_m","con_m",
            "ccp_m","sym_m","frd_m",
            "rad_se","txt_se","per_se","are_se","smt_se",
            "cmp_se","con_se","ccp_se","sym_se",
            "frd_se","rad_w","txt_w","per_w",
            "are_w","smt_w","cmp_w","con_w",
            "ccp_w","sym_w","frd_w")

colnames(corMatrix) <- cNames

# Create the correlation matrix
M <- round(cor(corMatrix), 2)

# Create corrplot
corrplot(M, diag = FALSE, method="color", order="FPC", tl.srt = 90)

#PCA
#covariance matrix is used to calculate the eigen values and eigen vectors
wdbc.pcov <- princomp(wdbc.data, scores = TRUE)
summary(wdbc.pcov)

#Bi-plot using covariance matrix
#“area_mean” and “area_worst” are large values for both mean and standard deviation
cex.before <- par("cex")
par(cex = 0.7)
biplot(wdbc.pcov)

par(cex = cex.before)

# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))
# Calculate variability of each component
#The square of the sdev’s gives us the eigen value of each component.
pr.cvar <- wdbc.pcov$sdev ^ 2
# Variance explained by each principal component: pve
pve_cov <- pr.cvar/sum(pr.cvar)

#calculate the cumulative proportion explained at each principal component
# Eigen values
round(pr.cvar, 2)

# Percent variance explained
round(pve_cov, 2)

# Cummulative percent explained
round(cumsum(pve_cov), 2)

#Scree plot using covariance matrix:
# Plot variance explained for each principal component
plot(pve_cov, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve_cov), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")


#Running PCA using correlation matrix:
#correlation matrix is used to calculate the eigen values and eigen vectors
wdbc.pr <- prcomp(wdbc.data, scale = TRUE, center = TRUE)
summary(wdbc.pr)

#visualize this using a Scree plot
# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wdbc.pr$sdev ^ 2

# This is done for reporting purposes.
names(pr.var) <- names(pr.cvar)

# Variance explained by each principal component: pve
pve <- pr.var/sum(pr.var)

# This is done to be consistent with princomp.
names(pve) <- names(pve_cov)

#Before creating the plot, let’s see the values
# Eigen values
round(pr.var, 2)
# Percent variance explained
round(pve, 2)

# Cummulative percent explained
round(cumsum(pve), 2)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")


#Next, let’s create a scatter plot observations by principal components 1 and 2:
# Scatter plot observations by components 1 and 2
plot(wdbc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))

ls(wdbc.pr)

wdbc.pcs <- wdbc.pr$x[,1:6]
head(wdbc.pcs, 20)


#Here, diagnosis == 1 represents malignant and diagnosis == 0 represents benign.
wdbc.pcst <- wdbc.pcs
wdbc.pcst <- cbind(wdbc.pcs, diagnosis)
head(wdbc.pcst)

#Split the dataset into training/test data
# Calculate N
N <- nrow(wdbc.pcst)
# Create a random number vector
rvec <- runif(N)

# Select rows from the dataframe
wdbc.pcst.train <- wdbc.pcst[rvec < 0.75,]
wdbc.pcst.test <- wdbc.pcst[rvec >= 0.75,]

# Check the number of observations
nrow(wdbc.pcst.train)
nrow(wdbc.pcst.test)

library(MASS)
wdbc.pcst.train.df <- wdbc.pcst.train

# convert matrix to a dataframe
wdbc.pcst.train.df <- as.data.frame(wdbc.pcst.train)

# Perform LDA on diagnosis
wdbc.lda <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = wdbc.pcst.train.df)

#Let’s summarize the LDA output:
wdbc.lda
#Let’s use this to predict by passing the predict function’s newdata as the testing dataset.
wdbc.pcst.test.df <- wdbc.pcst.test

# convert matrix to a dataframe
wdbc.pcst.test.df <- as.data.frame(wdbc.pcst.test)
wdbc.lda.predict <- predict(wdbc.lda, newdata = wdbc.pcst.test.df)

ls(wdbc.lda.predict)

# print the predictions
(wdbc.lda.predict.class <- wdbc.lda.predict$class)

library("ROCR")
# Get the posteriors as a dataframe.
wdbc.lda.predict.posteriors <- as.data.frame(wdbc.lda.predict$posterior)

# Evaluate the model
pred <- prediction(wdbc.lda.predict.posteriors[,2], wdbc.pcst.test.df$diagnosis)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
