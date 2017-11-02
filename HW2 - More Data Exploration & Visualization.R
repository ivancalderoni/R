# asbio contains the Con.Dis.matrix() function needed for to calculate concordant and
# discordant pairs
library(asbio)
# needed to use the melt function
library(reshape2)
library(ggplot2)
# load "outliers" package to 'test' for outliers
library(outliers)
# to get the "Animals" dataset
library(MASS)
# needed to create the adjusted box plot
library(robustbase)
# grubbs test
library(outliers)
library(fitdistrplus)
# contains the freetrade dataframe needed for problem 5
library(Amelia)
# to use the "aggr" function
library(VIM)
# the following package has a dataset needed for problem 5 (b)
library(HSAUR2)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(jpeg)

# 1) CONCORDANCE AND DISCORDANCE

x = c(3, 4, 2, 1, 7, 6, 5)
y = c(4, 3, 7, 6, 5, 2, 1)

z <- ConDis.matrix(x, y)

# view z
z

# this will add all the 1s in the matrix which is the total number of concordant pairs
Concordant <- sum(z == 1, na.rm = TRUE)

# print the total of Concordant pairs
Concordant

# this will add all the -1s in the matrix which is the total number of discordant pairs
Discordant <- sum(z == -1, na.rm = TRUE)

# print the total of Concordant pairs
Discordant

# There are 6 concordant pairs and 15 discordant pairs.
#-------------------------------------------------------------------------------------

# 2) OUTLIER EXAMPLE

# outliers example and some R functionality
# including basic bivariate outlier labeling

# load the "Animals" dataset from the MASS package
data(Animals)

# look at the first few records
head(Animals)

# plots animal's brain weight vs. body weight
plot(Animals)

# use the interactive functionality to identify
identify(Animals)

# a few data points you think might be outliers
# - just scroll the mouse over the plot and left-click on the points
# - press ESC when finished

# do it again, but this time the names of the animals will display
plot(Animals)

# can label, and save indices in vector
v <- identify(Animals,labels=row.names(Animals))

# these are the records you selected with the mouse
Animals[v,]

# we could easily delete all of these values
Animals<-Animals[-v,]

# and repeat the process...

# plots animal's brain weight vs. body weight
plot(Animals)

# interactivley point and click
v <- identify(Animals,labels=row.names(Animals))  

# display animals selected when finished
Animals[v,]  


# let's try something else...

# re-load full data set and  examine the outliers 
data(Animals)

# univariate test for 'brain' outliers
grubbs.test(Animals$brain)

# univariate test for 'body' outliers
grubbs.test(Animals$body)

# what is the most extreme value for brain weight?
outlier(Animals$brain)
# The most extreme value for brain weight is: 5712 (African elephant)

# what is the most extreme value for body weight?
outlier(Animals$body)
# The most extreme value for body weight is 87000 (Brachiosaurus)

# which records identified?
Animals[Animals$brain==outlier(Animals$brain),]
# Identified: African elephant

Animals[Animals$body==outlier(Animals$body),]
# Identified: Brachiosaurus

# let's remove the body weight outlier

# return all records in Animals EXCEPT for the record that has the body weight
# equal to the value of outlying body weigth
Animals<-Animals[Animals$body!=outlier(Animals$body),]

plot(Animals)

# add a trend line based on a linear model between brain and body weight
abline(lm(Animals$brain ~ Animals$body))

# the scatter plot with a simple regression line allows us to visualize 
# bivariate outliers -- that is, data points that are far from the trend line

# pick the four furthest points from the line..
v <- identify(Animals$body,Animals$brain,labels=row.names(Animals))   
Animals[v,]

# and delete if you want...
Animals <- Animals[-v,]

# plot what ever is left, and
plot(Animals)

# the new model looks like this
abline(lm(Animals$brain ~ Animals$body))

# now please identify the most extreme bivariate data point in the resulting plot
v <- identify(Animals$body,Animals$brain,labels=row.names(Animals))
Animals[v,]

# what is the final animal selected in the very last step?
# HUMAN
#-----------------------------------------------------------------------------------

# 3 Generating data and advanced density plots

# 3 (a)
a = rnorm(500, mean = 0, sd = 1)
b = rbinom(500, size = 5, prob = 0.45)
c = rlnorm(500, meanlog = 0, sdlog = 1)
d = rpois(500, lambda = 10)

df = data.frame(a, b, c, d)

df2 <- melt(data = df, id.vars = NULL, variable.name = "groupVar")

# 3 (b)

ggplot(data = df2, aes(x = value, fill = groupVar)) + geom_density(alpha=0.5) + labs(x = "Values", y = "Densities", title = "Densities for variables: A, B, C and D")

# 4 SHARK ATTACK

# 4 (a)

# 4 (b)

sharks = read.csv("ISE 5103 GSAF.csv", header = TRUE)

# Here, I am creating a new data frame, GSAFdata, which contains rows from 4,070
# through the end of the data frame "sharks."
GSAFdata = sharks[4070:5750, ]

# 4 (c)

# We use 'b' because the months are abbreviated
newDate <- as.Date(GSAFdata$Date, "%d-%b-%y")

# Now append newDate to GSAFdata
GSAFdata <- data.frame(newDate, GSAFdata)

# 4 (d): What percentage of the new date field is missing?

missing <- GSAFdata[is.na(GSAFdata$newDate), ]

# 125/1,681 = 0.0743605 which is about 7.44%. This data is not necessarily missing
# but it is formatted differently.

# 4 (e): Delete all of the records in GSAFdata that have missing values for the new
# date field.
GSAFdata <- GSAFdata[!is.na(GSAFdata$newDate), ]

# 4 (f)

# i. Use the diff command to help you create a vector daysBetween with days between
# attacks. Notice that the vector daysBetween will have one less value than the number
# of rows in GSAFdata. Add a missing value as the first element of daysBetween and add
# the revised vector as a new variable in GSAFdata.
GSAFdata <- GSAFdata[order(GSAFdata$newDate),]

daysBetween <- diff(GSAFdata$newDate)

# Adding a 0 as the first element of daysBetween.
daysBetween <- c(0, daysBetween)

GSAFdata <- data.frame(daysBetween, GSAFdata)

# ii. Run and comment on the results from boxplot and adjbox for GSAFdata$daysBetween.

boxplot(GSAFdata$daysBetween, notch=T, col="red", horizontal=T, xlab="Days Between Shark Attacks", main="Box Plot of Days Between Shark Attacks")

adjbox(GSAFdata$daysBetween, notch=T, col="red", horizontal=T, xlab="Days Between Shark Attacks", main="Adjusted Box Plot of Days Between Shark Attacks")

# iii. Is the Grubb's test, the Generalized ESD test, both, or neither appropriate for
# this data?

grubbs.test(GSAFdata$daysBetween, type=10)
# The Grubbs test might not be very helpful for this data set since it is large, and
# as the boxplots show, there are multiple outliers.

# 4 (g)

qqnorm(GSAFdata$daysBetween)

qqline(GSAFdata$daysBetween, distribution = qnorm)

x <- rexp(1556)

qqplot(x, GSAFdata$daysBetween, main = "Exponential Q-Q Plot", xlab = "Theoretical Values", ylab = "Sample Values")

qqline(GSAFdata$daysBetween, distribution = qexp)

# 4 (h)
fitExponential <- fitdist(GSAFdata$daysBetween, "exp")
plot(fitExponential)
gofstat(fitExponential)
#-----------------------------------------------------------------------------------

# 5 Missing Data

# 5 (a): Explore the "missingness" in the freetrade using your choice of methods,
# e.g. from packages VIM, mice, Amelia, and/or others.

data("freetrade")

# missmp (missingness map) plots a map showing where the missingness occurs in the
# dataset passed to Amelia.
missmap(freetrade, by = list(freetrade$country))

# aggr (aggregations for missing/imputed values) plots the a mount of missing/
# imputed values in each variable and the amount of missing/imputed values in
# certain combinations of variables.
aggr(freetrade)

# 5 (b): Implement your own statistical test to determine if the missingness in the
# tariff variable is independent with the country variable. Does your answer change
# if you remove Nepal or if you remove the Philippines?

# Test to be used to determine if the missingness in the variable tariff is
# independent with the country variable: CHI-SQUARE TEST.
tariffVar <- table(freetrade$country, is.na(freetrade$tariff))

chisq.test(tariffVar)

# Chi-square test while excluding Nepal..
tariffVarNoNepal <- table(freetrade$country[freetrade$country!="Nepal"], is.na(freetrade$tariff[freetrade$country!="Nepal"]))

chisq.test(tariffVarNoNepal)

# Chi-square test while excluding the Philippines..
tariffVarNoPhil <- table(freetrade$country[freetrade$country != "Philippines"], is.na(freetrade$tariff[freetrade$country != "Philippines"]))

chisq.test(tariffVarNoPhil)

# Chi-square test while excluding Nepal and the Philippines...
NoNepalNoPhil <- freetrade$country != "Nepal" & freetrade$country != "Philippines"

tariffVarNoNepalNoPhil <- table(freetrade$country[NoNepalNoPhil], is.na(freetrade$tariff[NoNepalNoPhil]))

chisq.test(tariffVarNoNepalNoPhil)

# 6 PRINCIPAL COMPONENT ANALYSIS

# 6 (a): Mathematics of principal components

# 6 (a) (i): Using the data mtcars, create the correlation matrix of all the
# attributes and store the results in a new object corMat.
data("mtcars")

corMat <- cor(mtcars)

# 6 (a) (ii): Compute the eigenvalues and eigenvectors of corMat.
eigenValuesAndVectors <- eigen(corMat)

# 6 (a) (iii): Use "prcomp" to compute the principal components of the "mtcars"
# attributes (make sure to use the scale option).
prcompValues <- prcomp(mtcars, scale = TRUE)

# 6 (a) (iv): Compare the results from (ii) and (iii).

# Both (ii) and (iii) are the same in magnitude because principal components are the
# same as the eigenvector with the highest eigen value.

# 6 (a) (v): Using R demonstrate that principal components 1 and 2 from (iii) are 
# orthogonal.
PCA <- as.data.frame(prcompValues$rotation)

PCA$PC1%*%PCA$PC2

# Principal components 1 and principal components 2 are orthogonal.

# 6 (b): The HSAUR2 package contains the data heptathlon which are the results of the 
# women's olympic heptathlon competition in Seoul, Korea from 1988.
data("heptathlon")

# 6 (b) (i): Look at histograms of each numerical variable using
# apply(heptathlon[,1:8],2,hist) (note: these are not labeled well, but that is okay
# for now since you just want to take a quick look at the distributions).
# From this quick inspection, are the distributions reasonably normal?
par(mfrow=c(2,2))

m <- apply(heptathlon[,1:8], 2, hist)

# The distributions seem to be reasonably normally distributed at a quick glance at
# these histograms.

# 6 (b) (ii): Examine the event results using the Grubb's test.
grubbsTest <- apply(heptathlon[,1:8], 2, grubbs.test)

grubbsTest

# Launa seems to be the outlier in 5 of 8 the competitions.

heptathlon <- heptathlon[!rownames(heptathlon) %in% "Launa (PNG)", ]

# 6 (b) (iii)
heptathlon[,"hurdles"] <- max(heptathlon$hurdles) - heptathlon[,"hurdles"]
heptathlon[,"run200m"] <- max(heptathlon$hurdles) - heptathlon[,"run200m"]
heptathlon[,"run800m"] <- max(heptathlon$hurdles) - heptathlon[,"run800m"]

# 6 (b) (iv): Perform a principal component analysis on the 7 event results and save
# the results of the prcomp function to a new variable Hpca.
prcompHeptathlon <- prcomp(heptathlon, scale = TRUE)

Hpca <- as.data.frame(prcompHeptathlon$rotation)

summary(Hpca)

# 6 (b) (v): Use "ggibiplot" to visualize the first two principal components.
# Provide a concise interpretation of the results.
ggbiplot(prcompHeptathlon, circle = T, obs.scale = 1, varname.size = 5, labels = rownames(heptathlon))

# Hurdles, score, shot and longjump are the biggest contibuting factors for PC1.

# 6 (b) (vi)
plot(prcompHeptathlon$x[,1], heptathlon$score, main = "PCA Projection 1 vs. Heptathlon Score", xlab = "PCA 1 Projections", ylab = "Total Hepthalon Score")

# The plot shoes that there is a strong relationship between PCA comp 1 and the total
# Heptathlon score.

# 6 (c): Load the digit file "ClassDigits.csv" from the course website.
classDigits <- read.csv("ClassDigits.csv", header = TRUE)

# We want to get rid of the first column (the label column 0-9)
classDigitsNoLabelCol <- classDigits[,-1]

# 6 (c) (i): Compute the eigenvectors of the digit data.
classDigits.pca <- prcomp(classDigitsNoLabelCol, scale=F)

classDigits.eigen <- classDigits.pca$rotation

# head(classDigits.eigen, 2)

plot(classDigits.pca)

# 6 (c) (ii): Create a JPG image of the mean digit. Name this file 
# meanDigit.jpg.

classDigits.mean <- colMeans(classDigitsNoLabelCol[sapply(classDigitsNoLabelCol, is.numeric)])

digitMatrix <- matrix(classDigits.mean, 28, 28, byrow=T)
writeJPEG(digitMatrix, target="meanDigit.jpg")

# 6 (c) (iii): iii. Reconstruct two training images (image #15 and #100)
# based on k = 5; 20; and 100 principal components. Name these files
# image15-5.jpg, image15-20.jpg, image15-100, image100-5, ...".

image15 <- unlist(classDigitsNoLabelCol[15, ])
image15matrix <- matrix(image15, 28, 28, byrow=T)
actualImage15 <- image(image15matrix)

image100 <- unlist(classDigitsNoLabelCol[100, ])
image100matrix <- matrix(image100, 28, 28, byrow=T)
actualImage100 <- image(image100matrix)

# image15-5
A = classDigits.pca$x[15,1:5] %*% t(classDigits.pca$rotation[,1:5]) + classDigits.mean
matrixA <- matrix(A, 28, 28, byrow=T)
writeJPEG(A, target="image15-5.jpg")

# image15-20
B = classDigits.pca$x[15,1:20] %*% t(classDigits.pca$rotation[,1:20]) + classDigits.mean
matrixB <- matrix(B, 28, 28, byrow=T)
writeJPEG(B, target="image15-20.jpg")

# image15-100
C = classDigits.pca$x[15,1:100] %*% t(classDigits.pca$rotation[,1:100]) + classDigits.mean
matrixC <- matrix(C, 28, 28, byrow=T)
writeJPEG(C, target="image15-100.jpg")

# image100-5
X = classDigits.pca$x[100,1:5] %*% t(classDigits.pca$rotation[,1:5]) + classDigits.mean
matrixA <- matrix(X, 28, 28, byrow=T)
writeJPEG(A, target="image100-5.jpg")

# image100-20
Y = classDigits.pca$x[100,1:20] %*% t(classDigits.pca$rotation[,1:20]) + classDigits.mean
matrixB <- matrix(Y, 28, 28, byrow=T)
writeJPEG(B, target="image100-20.jpg")

# image100-100
Z = classDigits.pca$x[100,1:100] %*% t(classDigits.pca$rotation[,1:100]) + classDigits.mean
matrixC <- matrix(Z, 28, 28, byrow=T)
writeJPEG(C, target="image100-100.jpg")

# 6 (c) (iv): Choose a value for k << 784 based on the PCA summary
# or a screeplot. Using this value of k, for each of the 7 observations
# in the test data, determine the average mahalanobis distance
# from "digit-space". Describe the results.

classDigitsNoLabel <- classDigits[,-1]
classDigits.pca2 <- prcomp(classDigitsNoLabel)
meanClassDigits <- classDigits.pca2$center
meanClassDigits <- as.matrix(meanClassDigits)

screeplot(classDigits.pca2, npcs = 200, type = "lines", main="Digit Data Screeplot")

meanClassDigits <- t(meanClassDigits)

# Looking at the graph, one can tell most of the variance
# by PC 33.

class7test <- read.csv("class7test.csv", header = TRUE)
class7testDigits <- as.matrix(class7test[,c(-1,-2,-787)])

meanDigitsMatrix <- matrix(rep(meanClassDigits[1,],7), nrow = 7, ncol=784, byrow=TRUE)

weightsData <- classDigits.pca2$x[,1:33]

difference <- class7testDigits - meanDigitsMatrix

eigen33 <- as.matrix(classDigits.eigen[,1:33])


weightsClass7Test <- difference%*%eigen33
eigenClass7Test <- cov(weightsClass7Test)

mahaDistance = rep(0,7)
for (i in 1:7){
  mahaDistance[i] = mean(mahalanobis(weightsData, weightsClass7Test[i,], eigenClass7Test))
}

head(mahaDistance)
plot(weightsClass7Test)
points(mahaDistance)
plot(mahaDistance)

# 6 (c) (v): For the test images, 4, 5, and 6, determine
# the lowest value of k principal components that you
# need to correctly identify the 10 digits. The value of 
# k may be different for each test image.

for (i in class7test[4:6, 1]) {
  
  k = 1
  
  repeat {
    
    weightsData <- classDigits.pca2$x[,1:k]
    dif <- (class7testDigits[i,] - meanClassDigits)
    
    eigenK <- as.matrix(classDigits.eigen[,1:k]) 
    
    weightsClass7Test <- dif%*%eigenK
    
    weightsData <- as.matrix(weightsData)
    eigenClass7Test <- cov(weightsData)
    
    # which.min determines the index of min/max of numeric vector
    whichOne <- which.min(mahalanobis(weightsData, weightsClass7Test, eigenClass7Test))
    label <- classDigits[whichOne, 1]
    class7TestLabel <- class7test[i, 2]
    
    if (label == class7TestLabel || k > 784) break
    k <- k + 1
  }
  
  print(k)
}
