# Set working directory
setwd("~/Documents/SPD")

# Include librarys 
library("party")
library("randomForest")
library("rattle")
library("rpart")
library("ggplot2")
library("gridExtra")
library("rpart.plot")

### READING DATA ###
train <- read.csv("train.csv")
test  <- read.csv("test.csv")
test$Survived <- 0

# data and summary of data
data <- rbind(train, test)
#str(data)
#print(summary(data))

# Factorise PClass and Survived
data$Pclass <- factor(data$Pclass)
data$Survived <- factor(data$Survived)
#print(summary(data))

### CLEANING DATA ###
data$Name <- as.character(data$Name)
strsplit(data$Name[1], split='[,.]')
strsplit(data$Name[1], split='[,.]')[[1]]
strsplit(data$Name[1], split='[,.]')[[1]][2]
data$Title <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data$Title <- sub(' ', '', data$Title)
data$Title[data$PassengerId == 797] <- 'Mrs' # female doctor
data$Title[data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
data$Title[data$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col')] <- 'Sir'
data$Title[data$Title %in% c('Mlle', 'Mme')] <- 'Mlle'
data$Title <- factor(data$Title)
#print(summary(data))
#print(table(data$Title))

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
data$Embarked[c(62,830)] = "S"
data$Embarked <- factor(data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
data$Fare[1044] <- median(data$Fare, na.rm=TRUE)

# Create new column -> family_size
data$FamilySize <- data$SibSp + data$Parch + 1


# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                       data=data[!is.na(data$Age),], method="anova")
data$Age[is.na(data$Age)] <- predict(predicted_age, data[is.na(data$Age),])
#print(summary(data))


# Adding Child
data$Child<-0
data$Child[data$Age<18 ]<- 1
data$Child <- factor(data$Child)

# Is alone
data$IsAlone <- 0
data$IsAlone[data$FamilySize == 1] <- 'Yes'
data$IsAlone[data$FamilySize > 1] <- 'No'
data$IsAlone <- factor(data$IsAlone)
#print(summary(data))

# Split the data back into train and test set
train <- data[1:891,]
test <- data[892:1309,]
#test$Survived <- NULL

# Some plots
p1 <- ggplot(train, aes(Pclass, fill = Survived)) + geom_bar()+
  labs(title = "Passanger class by Survived", x = "Passanger class", y = "Count of Passanger")
p2 <- ggplot(train, aes(Sex, fill = Survived)) + geom_bar()+
  labs(title = "Passanger class by Survived", x = "Sex", y = "Count of Passanger")
p3 <- ggplot(train, aes(Title, fill = Survived)) + geom_bar()+
  labs(title = "Passanger Title by Survived", x = "Title", y = "Count of Passanger")
p4 <- ggplot(train, aes(IsAlone, fill = Survived)) + geom_bar()+
  labs(title = "Mother vs Alone by Survived", x = "Alone", y = "Count of Passanger")
grid.arrange(p1, p2, p3, p4, nrow=2)

# Train Random Forest 
#print(summary(train))
set.seed(1)
model_plot <- rpart(as.factor(Survived) ~ Pclass+Age+Sex+FamilySize
                      +Fare+Embarked+SibSp+IsAlone+Child+Title
                      +Parch,data = train, method = 'class')

fancyRpartPlot(model_plot)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize+Child 
                   , data=train, importance=TRUE, ntree=1000)
varImpPlot(fit)

model <- cforest(as.factor(Survived) ~ Pclass+Age+Sex+FamilySize
                      +Fare+Embarked+SibSp+Title
                      ,data = train, controls=cforest_unbiased(ntree=2000))
print(importance(fit))
#Prediction <- predict(model, test)
Prediction <- predict(model, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "cforest.csv", row.names = FALSE)
plot(fit)
print(sum(Prediction == test$Survived)/nrow(test))

 


