# PACKAGES USED:

# the "mlbench" package contains the dataset "Glass"
library(mlbench)
# "robustbase" allows us to create "adjusted-box-plots"
library(robustbase)
# installing "psych" will let me use the function "describe"
library(psych)
# "car" contains the "symbox" function
library(car)
# "EnvStats" allows us to use "boxcox"
library(EnvStats)
# "mass" allows you to use the linear discriminant analysis "lda" function
library(MASS)

library(Amelia)

library(mice)

library(randomForest)

library(corrplot)


#--------------------------------------------------------------------------#

# PROBLEM 1 (A):


boxplot(Glass)
adjbox(Glass, main="Adjusted Boxplot of 'Glass'", xlab="Variables", ylab="Values")

describe(Glass)

# I am creating histograms for all the attributes of Glass except for Type to
# look at individual distributions.

hist(Glass$RI)
hist(Glass$Na)
hist(Glass$Mg)
hist(Glass$Al)
hist(Glass$Si)
hist(Glass$K)
hist(Glass$Ca)
hist(Glass$Ba)
hist(Glass$Fe)


# PROBLEM 1 (B):


# After looking at individual distributions and the boxplots,
# I determined that K, Ca, and Ba will be transformed. They
# also have the highest values under skew.

symbox(Glass$K + 0.0001, powers = c(-1, -0.5, 0, 0.5, 1, 2, 3), main="Symbox K")
boxcox(Glass$K + 0.0001)
# Choose lambda = 0.5

symbox(Glass$Ca + 0.0001, powers = c(-1, -0.5, 0, 0.5, 1, 2, 3), main="Symbox Ca")
boxcox(Glass$Ca + 0.0001)
# Choose lambda = -1.0

symbox(Glass$Ba + 0.0001, powers = c(-1, -0.5, 0, 0.5, 1, 2, 3), main="Symbox Ba")
boxcox(Glass$Ba + 0.0001)
# Choose lambda = 0.0


# PROBLEM 1 (C): PRINCIPAL COMPONENT ANALYSIS (PCA)


GlassPCA <- prcomp(Glass[,1:9], scale = TRUE)
summary(GlassPCA)

# One can see that about 90% of the variance can be accounted by PC1, PC2,
# PC3, PC4, and PC5.


# PROBLEM 1 (D): LINEAR DISCRIMINANT ANALYSIS (LDA)


GlassLDA <- lda(formula = Type ~ ., data = Glass)

GlassPredict <- predict(GlassLDA, newdata = Glass[,1:9])$class
table(GlassPredict, Glass[,10])

# PCA treats data sets as a whole, while LDA tries to explicitly model
# the difference between classes within a data set. LDA as a technique
# cares about class separability.

#--------------------------------------------------------------------------#

# PROBLEM 2: MISSING DATA

library(VIM)
data("freetrade")

# 2 (a): Listwise Deletion - delete the record if any value is missing.

# create a copy of the freetrade dataset & call it "freetradeListwiseDeletion"
freetradeListwiseDeletion <- freetrade
freetradeListwiseDeletion$pop <- freetradeListwiseDeletion$pop/1000000

# return a logical vector indicating which rows have no missing values
# store this data under "freetradeMissing"
freetradeMissing <- complete.cases(freetradeListwiseDeletion)

# add the complete.cases vector in the "freetradeListWiseDeletion" dataframe
freetradeListwiseDeletion$freetradeMissing <- freetradeMissing

# if false, delete
freetradeListwiseDeletion <- freetradeListwiseDeletion[!freetradeListwiseDeletion$freetradeMissing=="FALSE",]

ListwiseDeletion <- lm(data=freetradeListwiseDeletion,tariff~year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg)

ListwiseDeletionCoefficients <- ListwiseDeletion$coefficients

ListwiseDeletionCoefficients


# 2 (b): perform the regression using mean imputation


# create a copy of the freetrade dataset & call it "freetradeMeanImputation"
freetradeMeanImputation <- freetrade
freetradeMeanImputation$pop <- freetradeMeanImputation$pop/1000000

# IMPUTE <tariff> MISSING VALUES
tariffMissing <- is.na(freetradeMeanImputation$tariff)

# sum all the FALSE = 58
sum(tariffMissing)

freetradeMeanImputation$tariffMissing <- tariffMissing

freetradeMeanImputation[tariffMissing, "tariff"] <- mean(freetradeMeanImputation$tariff, na.rm=TRUE)


# IMPUTE <polity> MISSING VALUES
polityMissing <- is.na(freetradeMeanImputation$polity)

# sum all the FALSE = 2
sum(polityMissing)

freetradeMeanImputation$polityMissing <- polityMissing

freetradeMeanImputation[polityMissing, "polity"] <- mean(freetradeMeanImputation$polity, na.rm=TRUE)


# IMPUTE <gpd.pc> MISSING VALUES
gpd.pcMissing <- is.na(freetradeMeanImputation$gdp.pc)

# sum all the FALSE = 0
sum(gpd.pcMissing)
# the sum was 0 so all the values are there (no need to impute here)!


# IMPUTE <intresmi> MISSING VALUES
intresmiMissing <- is.na(freetradeMeanImputation$intresmi)

# sum all the FALSE = 13
sum(intresmiMissing)

freetradeMeanImputation$intresmiMissing <- intresmiMissing

freetradeMeanImputation[intresmiMissing, "intresmi"] <- mean(freetradeMeanImputation$intresmi, na.rm=TRUE)


# IMPUTE <signed> MISSING VALUES
signedMissing <- is.na(freetradeMeanImputation$signed)

# sum all the FALSE = 3
sum(signedMissing)

freetradeMeanImputation$signedMissing <- signedMissing

freetradeMeanImputation[signedMissing, "signed"] <- mean(freetradeMeanImputation$signed, na.rm=TRUE)


# IMPUTE <fiveop> MISSING VALUES
fiveopMissing <- is.na(freetradeMeanImputation$fiveop)

# sum all the FALSE = 18
sum(fiveopMissing)

freetradeMeanImputation$fiveopMissing <- fiveopMissing

freetradeMeanImputation[fiveopMissing, "fiveop"] <- mean(freetradeMeanImputation$fiveop, na.rm=TRUE)

meanImputationLM <- lm(data = freetradeMeanImputation, tariff ~ year + country + polity + pop + gdp.pc + intresmi + signed + fiveop + usheg)

meanImputationCoefficients <- meanImputationLM$coefficients

meanImputationCoefficients

# 2 (c): perform the regression using multiple imputation


freetradeMultipleImputation <- freetrade
# since I am dividing the pop by 1,000,000 here, I will go back and do it
# for part (a) and (b).
freetradeMultipleImputation$pop <- freetradeMultipleImputation$pop/1000000


averageFunction <- function(x) mean(is.na(x))

apply(freetradeMultipleImputation, 2, averageFunction)

imputationMethod <- c(year = "rf", country = "mean", tariff = "pmm", polity = "rf", pop = "sample", gdp.pc = "rf", intresmi = "mean", signed = "rf", fiveop = "rf", usheg = "pmm")

MultipleImputationChEq <- mice(freetradeMultipleImputation, m = 5, maxit = 50, meth = imputationMethod)

plot(MultipleImputationChEq)

MultipleImputationFit <- with(MultipleImputationChEq, lm(tariff ~ year + country + polity + pop + gdp.pc + intresmi + signed + fiveop + usheg))
str(MultipleImputationFit)

# pool(): pools the results of m repeated complete data analysis
est <- pool(MultipleImputationFit)
str(est)

MultipleImputationCoefficients <- est$qbar


# 2 (d): compare the coefficients for each of the regression models


compare <- data.frame(ListwiseDeletionCoefficients, meanImputationCoefficients, MultipleImputationCoefficients)
compare

#--------------------------------------------------------------------------#


# PROBLEM 3: HOUSE PRICES DATA


housingData <- read.csv("housingData.csv")

# let's go ahead and remove the "Id" column because it is repeated.
housingData <- housingData[,c(-1)]

which(colSums(is.na(housingData))/nrow(housingData) >= 0.25)

# those values are deleted:
housingData <- housingData[,colSums(is.na(housingData))/nrow(housingData) < 0.25]

numericValues <- sapply(housingData, is.numeric)
numericHousing <- housingData[,numericValues]
numericCorrelation <- cor(numericHousing)
corrplot(numericCorrelation)


TotalSquareFootage = (housingData$TotalBsmtSF + housingData$X1stFlrSF + housingData$X2ndFlrSF)

housingData <- data.frame(housingData, TotalSquareFootage)

PricePerSquareFootage = (housingData$TotalBsmtSF + housingData$X1stFlrSF + housingData$X2ndFlrSF) / (housingData$SalePrice)

housingData <- data.frame(housingData, PricePerSquareFootage)

plot(housingData$PricePerSquareFootage)
plot(housingData$TotalSquareFootage, housingData$SalePrice)

#--------------------------------------------------------------------------#


# PROBLEM 4: Kaggle.com - a little more data understanding


Titanic <- read.csv("Titanic.csv")

# PROBLEM 4 (a):


# I picked the Titanic dataset available in Kaggle because I have heard about
# this particular tutorial a couple of years ago. It is supposed to be an
# introductory project to people interested in data science where one has 
# apply tools to predict who survives the wreck due to some variables (such
# as, socioeconimic status, gender, etc.).

# https://www.kaggle.com/c/titanic


# PROBLEM 4 (b):


# count the number of rows:
nrow(Titanic)
# There are 891 rows.

# number of variables:
ncol(Titanic)
# There are 12 variables.

# descriptive statistics:
describe(Titanic)

library(dplyr)

# I want to know the average age of males and females in the Titanic and
# do not use all the NA values.
Titanic %>% group_by(Sex) %>% summarise(mean(Age, na.rm=TRUE))

# a selection of visualizations:
plot(Titanic)


library(ggplot2)

qplot(Age, Fare, data=Titanic, colour=as.factor(Pclass), facets=Sex~Embarked)

ggplot(Titanic, na.rm=TRUE, aes(Age, Fare)) + geom_smooth()

ggplot(Titanic, aes(as.factor(Pclass), fill=as.factor(Survived)))+geom_bar()


library(outliers)
library(Amelia)
missmap(Titanic, by = list(Titanic$PassengerId))

library(VIM)
aggr(Titanic)

adjbox(Titanic)
