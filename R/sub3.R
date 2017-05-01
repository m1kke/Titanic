library(rpart)

setwd("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R")

# load data
test <- read.csv("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R/test.csv")
train <- read.csv("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R/train.csv") #stringsAsFactors=FALSE muuttaisi factorit stringeiksi

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train,
             method = "class")

plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub3.csv", row.names = F)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train,
             method = "class",
             control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(fit)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train,
             method = "class",
             control = rpart.control(minsplit = 4, cp = 0))
new.fit <- prp(fit, snip = T)$obj
fancyRpartPlot(new.fit)
