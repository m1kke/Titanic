setwd("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R")

# load data
test <- read.csv("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R/test.csv")
train <- read.csv("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R/train.csv") #stringsAsFactors=FALSE muuttaisi factorit stringeiksi


test$Survived <- rep(0, 418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = F, quote = F)

prop.table(table(train$Sex, train$Survived), 1)

test$Survived <- 0 # sama tulos kuin rep

test$Survived[test$Sex == 'female'] <- 1 # hakaset luo subsetin dataframesta

train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})


train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN = function(x) {sum(x)/length(x)})
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0
