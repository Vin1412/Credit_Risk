
train <-read.csv("D:\\data science with R\\Decision tree\\titanic_dataset\\train.csv")

test <-read.csv("D:\\data science with R\\Decision tree\\titanic_dataset\\test.csv")


model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             family=binomial)

summary(model)
library(caret)
library(ggplot2)
predict<- predict(model,newdata= train, type = 'response')
confusionMatrix(data=as.numeric(predict>0.5), reference = trai$Survived)

#confusion matrix-method1

model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             family=binomial(link ="logit"))

predict<- predict(model,newdata= test, type = 'response')
p_class<-ifelse(predict>0.50, "M", "R")
table(p_class)
table(p_class, test$survived)
summary(p_class)
summary(test$survived)

confusionMatrix(data=as.numeric(predict>0.5), reference = test$Survived)

# to get confusion matrix-method 2
pred<- predict(model,newdata= test, type = 'response')

table(factor(pred, levels=min(test):max(test)), factor(test, levels=min(test):max(test)))