library(rpart)

setwd("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R")

# load data
test <- read.csv("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R/test.csv")
train <- read.csv("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R/train.csv") #stringsAsFactors=FALSE muuttaisi factorit stringeiksi

train$Name[1]

test$Survived <- NA
combi <- rbind(train, test) #combine datasets

# turn factors into strings
combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split = '[,.]') # split name from specific symbols
strsplit(combi$Name[1], split = '[,.]')[[1]]
strsplit(combi$Name[1], split = '[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#change variable back to factor
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


train <- combi[1:891,]
test <- combi[892:1309,]


Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data = combi[!is.na(combi$Age),],
                method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])


summary(combi)
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62, 830)] = 'S'
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = T)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data = train,
                    importance = TRUE,
                    ntree = 2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub5.csv", row.names = F)

library(party)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train,
               controls = cforest_unbiased(ntree = 200, mtry = 3))
Prediction <- predict(fit, test, OOB = T, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub5.csv", row.names = F)
