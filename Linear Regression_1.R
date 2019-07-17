# Linear regression

rm(list=ls())

# read baby file
Baby = read.csv("C:/Digital Nest/Linear Regression/baby_age_to_weight.csv")

summary(Baby)
head(Baby)
nrow(Baby)
ncol(Baby)
plot(Baby$Age,Baby$Weight)
plot(Baby$Age)
plot(Baby$Weight)
hist(Baby$Weight)
boxplot(Baby$Weight)

sum(is.na(Baby))
sum(is.na(Baby$Age))
sum(is.na(Baby$Weight))

# build the model
glm = glm(Weight ~ Age,data = Baby)
summary(glm)

## split the file into train and test
BabyTrain = Baby[1:104,]
BabyTest = Baby[105:130,]


# build the model
glmbabysub1 = glm(Weight ~ Age,data = BabyTrain)
?glm
summary(glmbabysub1)
glmbabysub2 = glm(Weight~Age,data = BabyTrain, family = 'gaussian')
summary(glmbabysub2)

# predict return the expected values based on best of intercept & co-efficient
Predictbaby = predict(glmbabysub1,BabyTrain)
?predict
# find sqaured error and RMSE
SqError = (BabyTrain$Weight - Predictbaby) ** 2
RMSEbaby = sqrt(sum(SqError) / nrow(BabyTrain))

#apply on model test
Predicttest = predict(glmbabysub1,BabyTest)

SqError2 = (BabyTest$Weight - Predicttest) ** 2
RMSEtest = sqrt(sum(SqError2) / nrow(BabyTest))

# Randomly split the data into train and test with 70:30 ratio 
TrainNum = as.integer(nrow(Baby) * 0.7)
seq2 = seq_len(nrow(Baby))
              
Trainindex = sort(sample(seq2,size=TrainNum))
BabyTrain2 = Baby[c(Trainindex),]
BabyTest2 = Baby[-c(Trainindex),]

glm2 = glm(Weight~Age,data = BabyTrain2)
summary(glm2)
glm2$coefficients

Predict1 = predict(glm2,BabyTrain2)
SqError3 = (BabyTrain2$Weight - Predict1) ** 2
RMSEtrain2 = sqrt(sum(SqError3) / nrow(BabyTrain2))

# observe how error is distributed - is it normally distributed?
Error = BabyTrain2$Weight - Predict1
hist(Error)
glm2$residuals
glm2$coefficients
glm2$effects

# is there any pattern in error w.r.t to predicted target value
plot(BabyTrain2$Weight,Error)
plot(Predict1,Error)
hist(Error,breaks = 20)
# is there any pattern in error w.r.t to predicted target value
plot(Predict1,Error)

# is there any correlation between actual and predicted
cor(BabyTrain2$Weight,Predict1)
plot(BabyTrain2$Weight,Predict1)

# find RMSE for test
Predict2 = predict(glm2,BabyTest2)
SqError4 = (BabyTest2$Weight - Predict2) ** 2
RMSEtest2 = sqrt(sum(SqError4) / nrow(BabyTest2))

# include sequnce number 
Baby$Sequence = seq(1:nrow(Baby))
Babysub1$Sequence = seq(1:nrow(Babysub1))

# build linear model based on 2 dependent variables - age and sequence
# from output, p value for sequence number > 5%, hence it should be ignored
glmbabysub1 = glm(Weight~Age+Sequence,data = Baby)
summary(glmbabysub1)
