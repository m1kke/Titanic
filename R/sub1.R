setwd("C:/Users/Mikko/Desktop/Kaggle Titanic Challenge/Kaggle-Titanic-Challenge/R")

# load data
test <- read.csv("test.csv", header = T, stringsAsFactors = F)
train <- read.csv("train.csv", header = T, stringsAsFactors = F)

head(train)

plot(density(train$Age, na.rm = T))
plot(density(train$Fare, na.rm = T))

counts <- table(train$Survived, train$Sex)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "Survived and deceased between male and female")
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])

Pclass_survival <- table(train$Survived, train$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People", main = "Survived and deceased between male and female")
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])

# CLEAN TRAIN DATA

train = train[-c(1,9:12)]

train$Sex = gsub("female", 1, train$Sex)
train$Sex = gsub("^male", 0, train$Sex)

master_vector = grep("Master.", train$Name, fixed = T)
miss_vector = grep("Miss.", train$Name, fixed = T)
mrs_vector = grep("Mrs.", train$Name, fixed = T)
mr_vector = grep("Mr.", train$Name, fixed = T)
dr_vector = grep("Dr.", train$Name, fixed = T)

for(i in master_vector) {
  train$Name[i] = "Master"
}

for(i in miss_vector) {
  train$Name[i] = "Miss"
}

for ( i in mrs_vector ) {
  train$Name[i] = "Mrs"
}

for ( i in mr_vector ) {
  train$Name[i] = "Mr"
}

for ( i in dr_vector ) {
  train$Name[i] = "Dr"
}

master_age = round(mean(train$Age[train$Name == "Master"], na.rm = T), digits = 2)
miss_age = round(mean(train$Age[train$Name == "Miss"], na.rm = T), digits = 2)
mrs_age = round(mean(train$Age[train$Name == "Mrs"], na.rm = T), digits = 2)
mr_age = round(mean(train$Age[train$Name == "Mr"], na.rm = T), digits = 2)
dr_age = round(mean(train$Age[train$Name == "Dr"], na.rm = T), digits = 2)


for ( i in 1:nrow(train)) {
  if (is.na(train[i,5])) {
    if (train$Name[i] == "Master") {
      train$Age[i] = master_age
    }
    else if (train$Name[i] == "Miss") {
      train$Age[i] = miss_age
    }
    else if (train$Name[i] == "Mrs") {
      train$Age[i] = mrs_age
    }
    else if (train$Name[i] == "Mr") {
      train$Age[i] = mr_age
    }
    else if (train$Name[i] == "Dr") {
      train$Age[i] = dr_age
    }
    else {
      print("Uncaught Title")
    }
  }
}

train["Child"] <- NA

for ( i in 1:nrow(train)) {
  if (train$Age[i] <= 12) {
    train$Child[i] = 1
  }
  else {
    train$Child[i] = 2
  }
}

train["Family"] <- NA

for ( i in 1:nrow(train)) {
  x = train$SibSp[i]
  y = train$Parch[i]
  train$Family[i] = x + y + 1
}

train["Mother"] <- NA

for ( i in 1:nrow(train)) {
  if (train$Name[i] == "Mrs" & train$Parch[i] > 0) {
    train$Mother[i] = 1
  }
  else {
    train$Mother[i] = 2
  }
}



# CLEAN TEST DATA
PassengerId = test[1]
test = test[-c(1, 8:11)]

test$Sex = gsub("female", 1, test$Sex)
test$Sex = gsub("^male", 0, test$Sex)

test_master_vector = grep("Master.",test$Name)
test_miss_vector = grep("Miss.", test$Name)
test_mrs_vector = grep("Mrs.", test$Name)
test_mr_vector = grep("Mr.", test$Name)
test_dr_vector = grep("Dr.", test$Name)

for(i in test_master_vector) {
  test[i, 2] = "Master"
}
for(i in test_miss_vector) {
  test[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  test[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  test[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  test[i, 2] = "Dr"
}

test_master_age = round(mean(test$Age[test$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(test$Age[test$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(test$Age[test$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(test$Age[test$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(test$Age[test$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(test)) {
  if (is.na(test[i,4])) {
    if (test[i, 2] == "Master") {
      test[i, 4] = test_master_age
    } else if (test[i, 2] == "Miss") {
      test[i, 4] = test_miss_age
    } else if (test[i, 2] == "Mrs") {
      test[i, 4] = test_mrs_age
    } else if (test[i, 2] == "Mr") {
      test[i, 4] = test_mr_age
    } else if (test[i, 2] == "Dr") {
      test[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", test[i,2], sep=""))
    }
  }
}

#We do a manual replacement here, because we weren't able to programmatically figure out the title.
#We figured out it was 89 because the above print statement should have warned us.
test[89, 4] = test_miss_age

test["Child"] = NA

for (i in 1:nrow(test)) {
  if (test[i, 4] <= 12) {
    test[i, 7] = 1
  } else {
    test[i, 7] = 1
  }
}

test["Family"] = NA

for(i in 1:nrow(test)) {
  test[i, 8] = test[i, 5] + test[i, 6] + 1
}

test["Mother"] = NA

for(i in 1:nrow(test)) {
  if(test[i, 2] == "Mrs" & test[i, 6] > 0) {
    test[i, 9] = 1
  } else {
    test[i, 9] = 2
  }
}

train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, family = binomial, data = train)
summary(train.glm)

p.hats <- predict.glm(train.glm, newdata = test, type = "response")

survival <- vector()

for (i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  }
  else {
    survival[i] <- 0
  }
}

kaggle.sub <- cbind(PassengerId, survival)
colnames(kaggle.sub) <- c("PassengerId", "Survived")
write.csv(kaggle.sub, file = "kaggle.csv", row.names = F)
