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

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data = train, 
             method = "class")

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub4.csv", row.names = F)
