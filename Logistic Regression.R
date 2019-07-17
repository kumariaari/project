# Logistic Model
#****************

# clear the environment
rm(list = ls())

# Read the iris file
setwd("C:/Digital Nest/Logistic Regression")
iris=read.csv("iris.csv", header=TRUE)

head(iris)
summary(iris)
table(iris$Species)

# convert Target variable 'Species' into Binary
# set 1 to 'setosa' category, 0 to other categories
iris$setosa = ifelse(iris$Species == 'setosa',1,0)
class(iris$setosa)
iris$setosa = as.factor(iris$setosa)
class(iris$setosa)

iris$Species = NULL
prop.table(table(iris$setosa))

#Building the Model and estimating the Coefficients
logit=glm(setosa ~ . , data=iris, family=binomial)
summary(logit)
?glm()

probt=fitted(logit)
probt
probt2=ifelse(probt>0.33,1,0)
class(probt2)
probt2 = as.factor(probt2)
probt2

install.packages("caret",dependencies = TRUE)
install.packages("e1071",dependencies = TRUE)
library(caret)
library(e1071)
confusionMatrix(iris$setosa,probt2,positive = "1")
?confusionMatrix
# Randomly split the data into train and test with 70:30 ratio 
TrainNum = as.integer(nrow(iris) * 0.7)

Trainindex = sort(sample(seq_len(nrow(iris)),size=TrainNum))
irisTrain = iris[c(Trainindex),]
irisTest = iris[-c(Trainindex),]

prop.table(table(irisTrain$setosa))
prop.table(table(irisTest$setosa))

logit2=glm(setosa ~ . , data=irisTrain, family=binomial)
summary(logit2)
probtTrain=fitted(logit2)
probtTrain2=ifelse(probtTrain>0.25,1,0)
probtTrain2 = as.factor(probtTrain2)
confusionMatrix(probtTrain2,irisTrain$setosa,positive = "1")

probtterms = predict(logit2,irisTest,type="terms")  # returns value of fitted line
probtlink = predict(logit2,irisTest,type="link")  # returns value of fitted line
probtTest = predict(logit2,irisTest,type="response")  # returns probabilites
probtTest2=ifelse(probtTest>0.25,1,0)
confusionMatrix(probtTest2,irisTest$setosa,positive = "1")
?confusionMatrix
# build ROC curve
library(ROCR)
ROCR1 = prediction(probtTest,irisTest$setosa)
ROCR2 = performance(ROCR1,"tpr","fpr")
plot(ROCR2)
auc2 = performance(ROCR1,measure = "auc")
auc2@y.values

# perform multi nominal logistric regression
# clear the environment
rm(list = ls())

# Read the iris file
setwd("C:/Digital Nest/Logistic Regression")
iris=read.csv("iris.csv", header=TRUE)

#Building the Model and estimating the Coefficients
logit=glm(Species ~ . , data=iris, family=binomial)
summary(logit)
probt=fitted(logit)

probTrain = predict(logit,iris,type='response')