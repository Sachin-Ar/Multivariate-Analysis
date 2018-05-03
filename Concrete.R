################################################
#####    MULTIVARIATE DATA AND ANALYSIS    #####
#####               EXERCISES              #####
################################################
#Sachin - different Data Cleaning techniques, Scatterplot, histogram, boxplot, Outliers, PCA, findings
#using the libraries
library("MVA")
library("lattice")

#####################################Data Intergrity Check- up########################################
#Reading the data from csv file 
#install.packages("readxl")
library(readxl)
concreteData <- read_excel("Concrete_Data.xls")
View(concreteData)
 
#checking the structure of data 
str(concreteData)

#checking the rows and column of the same 
nrow(concreteData)
ncol(concreteData)

#Checking the class of the data
class(concreteData)

#Checking missing values
is.na(concreteData)

# covariance matrix is
round(cov(concreteData[,names(concreteData)[1:9]]),digits=2)

# correlation matrix is
round(cor(concreteData[,names(concreteData)[1:9]]),digits=2)


#Checking the normality of the data
# Probability plots for each separate variable
layout(matrix(1:9, nc = 3))
# Iterate over all variables with sapply() that
# loops over variable names except kiln.
# Notice are using an anonymous function as
# second argument to sapply()
sapply(names(concreteData)[1:9], function(x) {
  qqnorm(concreteData[[x]], main = x, col = "green")
  qqline(concreteData[[x]], col = "red")
})

# several of the univariate plots show
# problematic 'patternistic' deviations: Cement, Fine Aggregate,and CCS
# does not deviate considerably from
# linearity; Actually all nine plots show some
# degree of deviation

# and the chi-square plot of the data. Do the 
# plots suggest anything unusual about the data?

# just want first nine variables (columns)
x <- concreteData
# derive means of each column
cm <- colMeans(x)
# calculate the covariance matrix
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 9), main = "Chi-Square plot",
     sd <- sort(d),
     xlab = expression(paste(chi[9]^2, " Quantile")), 
     ylab = "Ordered distances",col = "green" 
     ,xlim = range(qc) * c(1, 1.1))
abline(a = 0, b = 1, col = "red")
#the multivariate plot looks better but still the data shows deviation from normality

#Creating Scatterplot
attach(concreteData)
with(concreteData,cor(Cement,`Concrete compressive strength`))
with(concreteData,cor(Water, Superplasticizer))

#plotting correlation variables 
library(ggplot2)

p <- ggplot(concreteData, aes(x= jitter(Water)
                              , y= jitter(Superplasticizer)))

p + geom_point(col = "red") + xlab("Water") + ylab("SuperPlasticizer") +
  ggtitle("Scatter Plot") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )



#Histograms Plotting
layout(matrix(1:9, nc = 3))
sapply(names(concreteData)[1:9], function(x) {
  hist(concreteData[[x]], main = x, col = "red", xlab = "")
  
})


#Fine Aggregate , Course Aggregate, and CPA are somewhat outliers with some skewness

#Box Plot
layout(matrix(1:9, nc = 3))
boxplot(concreteData$Cement, xlab = expression(bold("Cement")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$`Blast Furnace Slag`,  xlab = expression(bold("Blast Furnace Slag")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$`Fly Ash`,  xlab = expression(bold("Fly Ash")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$Water,  xlab = expression(bold("Water")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$Superplasticizer,  xlab = expression(bold("SuperPlasticizer")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$`Coarse Aggregate`, xlab = expression(bold("Coarse Aggregate")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$`Fine Aggregate`, xlab = expression(bold("Fine Aggregate")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$`Age (day)`, xlab = expression(bold("Age(day)")), outline = TRUE, notch = TRUE, col = "red")
boxplot(concreteData$`Concrete compressive strength`,xlab = expression(bold("Concrete Compressive Strength")), outline = TRUE, notch = TRUE, col = "red")

#Principal Component Analysis
concreteDataPca <- cor(concreteData[,1:8]) 

concretePca <- prcomp(concreteDataPca, scale = T)
print(concretePca)
summary(concretePca,loadings = T)

#plotting pca
plot(concretePca, main = "")

  
#80% of the variance upto PC5
#Now taking rotations
concretePca$rotation[,1]
concretePca$rotation[,2]
concretePca$rotation[,3]

#plotting both of the PCA
x <- concretePca$x[,1]
y <- concretePca$x[,2]

plot(x, y, xlab = "PC1", 
     ylab = "PC2",
     xlim = range(x)*1.2, type = "n", main = "Principal Component Plot")
text(x, y, labels = colnames(concreteDataPca), 
     cex = 1, col = "red")


#Doesn't make sense 


# or, by extracting the first from all pre-
# computed principal components:
predict(concretePca)[,1]
predict(concretePca)[,2]
predict(concretePca)[,3]

#EFA and Cluster Analysis
#Cluster Analysis
X <- concreteData[,-9]
S <- scale(X,center = FALSE, scale = T)
conc_dist <- dist(S)


trellis.par.set(standard.theme(color = FALSE))
plot(levelplot(as.matrix(conc_dist), 
               xlab = "Sample Mixture No", 
               ylab = "Sample Mixture No",
               scales = list(x = list(draw = FALSE), 
                             y = list(draw = FALSE))))

#Doesn't make sense 

  n <- nrow(S)
  wss <- rep(0, 8)
  wss[1] <- (n - 1) * sum(sapply(S, var))
  for (i in 2:8)
    wss[i] <- sum(kmeans(S,
                         centers = i)$withinss)
  plot(1:8, wss, type = "b", xlab = "Number of groups",   
       ylab = "Within groups sum of squares")

#Number of clusters 4 in data
###########################################
### Hierarchical Clustering
### Plots for Single, Complete, 
### Average Linkages
###########################################
# use hclust() function to 
# plot example dendograms
# layout(matrix(1:3, ncol = 3))
# conc_single <- hclust(conc_dist, method = "single")
# conc_complete <- hclust(conc_dist, method = "complete")
# conc_average <- hclust(conc_dist, method = "average")
# plot(conc_single, main = "Single Cluster Dendogram")
# plot(conc_complete,main = "Complete Cluster Dendogram")
# plot(conc_average,main = "Average Cluster Dendogram")


#PCA
concretePca <- prcomp(concreteDataPca, scale = T)
plot(concretePca$x[, 1:2], 
     pch = kmeans(concreteDataPca, centers = 4)$cluster)


#kmeans
set.seed(123)
X <- concreteData[,-9]
scaledKmeansdata <- scale(X,center = FALSE, scale = T)
kmeans_concrete <- kmeans((scaledKmeansdata), centers = 4)
table(kmeans_concrete$cluster)


###################################################
### We create a function to compute the centers
### of the clusters for the untransformed data
###################################################
ccent <- function(cl) {
  f <- function(i) colMeans(X[cl == i,])
  x <- sapply(sort(unique(cl)), f)
  colnames(x) <- sort(unique(cl))
  return(x)
}

###################################################
### And then we apply it to the four-cluster
### solution obtained by k-means
###################################################
round(ccent(kmeans_concrete$cluster),3)

########################################
###   EXPLORATORY FACTOR ANALYSIS    ###
########################################

# Both "intelligence" and "social class" are 
# what are generally referred to as "latent
# variables (concepts that cannot be measured 
# directly but can be assumed to relate to a 
# number of measurable or manifest variables. 

# The method of analysis most generally used
# to help uncover the relationships between the
# assumed latent variables and the manifest 
# variables is factor analysis. 

# The model on which the method is based is 
# essentially that of multiple regression,
# except now the manifest variables are 
# regressed on the unobservable latent variables
# (often referred to in this context as common 
# factors), so that direct estimation of the
# corresponding regression coefficients 
# (factor loadings) is not possible.

# Are two varieties of factor analysis:

# Exploratory where make no assumptions about
# which manifest variables belong to which
# latent variables; and


# To begin, we use the formal test for the number
# of factors using mle:
concFactAnal <- concreteData[,-9]
sapply(1:4, function(f) factanal(concFactAnal, factors = f, 
                                 method ="mle")$PVAL)

# Results suggest 4-factor solution might be
# adequate to account for observed covariances
# in the data.

#Testing further to calculate the number of factors
#install.packages("nFactors")
library(nFactors)
library(psych)
X <- concreteData[,-9]
ev <- eigen(cor(X)) # get eigenvalues
ap <- parallel(subject=nrow(X),var=ncol(X),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#4 factors
h <- factanal(X, factors = 4, method = "mle")
print(h$loadings, cut = 0.5 )


#Using pysch package for EFA #using fa function
#install.packages("GPArotation")
library(GPArotation)
corr_data <- cor(X)
fourfactor <- fa(corr_data,nfactors = 5,rotate = "oblimin",fm="minres")
print(fourfactor$loadings,cutoff = 0.5)
fa.diagram(fourfactor)
fa.plot(fourfactor)
summary(fourfactor)

#removing age(day)
corr_data_removed_age <- cor(X[,1:7])
fourfactor1 <- fa(corr_data_removed_age,nfactors = 5,rotate = "oblimin",fm="minres")
print(fourfactor1$loadings,cutoff = 0.5)
fa.diagram(fourfactor1)
fa.plot(fourfactor1)

