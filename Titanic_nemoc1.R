# TITANIC

# Load raw data => Session => Set Working Directory => To Source File Location
getwd()
setwd("C:\\Users\\HP\\Documents\\R\\Titanic")

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

# Repeat with factor => comparison
str(data.combined)

# Distribution of survibal rate
table(data.combined$survived)

# Distribution across classes
table(data.combined$pclass)

# Load up ggplot2 package
library(ggplot2)

# Hypothesis - Rich folks survive at a higher rate
train$pclass <- as.factor(train$pclass)

ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examine a few first names
head(as.character(train$name))

# How many unique names are there in train and test, may be a mistake
length(unique(as.character(data.combined$name)))

# Two duplicate names
# Get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look on duplicate names, are they the same people???
data.combined[which(data.combined$name %in% dup.names),]

# What is the "Mr."  and "Miss." thing
library(stringr)

# Family Fortune, Andersson, Sage,
Fortunes <- data.combined[which(str_detect(data.combined$name, "Fortune,")),]
Fortunes[1:5,]

# Miss. and Mrs.
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Male
males <- data.combined[which(train$sex == "male"), ]
males [1:5,]

library(ggplot2)

# Create an utility function
extraTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
 }

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extraTitle(data.combined[i, "name"]))
}
data.combined$title <- as.factor(titles)

# Histogram
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survibed")

#======================================================================================================

# Distribution of males vs. females in train and test
table(data.combined$sex)

# Visualisation the 3-way relationship of sex, pclass, and survived
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Distribution of age
summary(data.combined$age)
summary(data.combined[1:891, "age"])

# Graph sex, age, pclass
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master" is a good proxy for a male kid
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

# Complicated misses
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

#~ ALT + 1up
ggplot(misses[misses$survived !="None",], aes(x=age, fill=survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 4) +
  ggtitle("Age for Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count")
  
# Female children have a different survival rate
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))

# Sibsp variable
summary(data.combined$sibsp)

# Can we treat as factor?
length(unique(data.combined$sibsp))

# Survival rates by sibsp
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
 
# Survival rates by parch
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Family size feature
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.integer(temp.sibsp + temp.parch +1)


# Family size
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Ticket variable
data.combined$ticket
class(data.combined$ticket)
str(data.combined$ticket)

#----------------------------------------------------------------------------------------------------------------
# TITANIC 2021

getwd()
setwd("C:\\Users\\HP\\Documents\\R\\Titanic")
dir()

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
test <- data.frame(survived = rep("Not_known", nrow(test)), test[,])

data <- rbind(train,test)

str(train)
train <- data.frame(lapply(train,as.character), stringsAsFactors = FALSE)
train[train == ""] <- NA

library(Amelia) #missing train
missmap(train, main = 'Missing Map', col = c('red', 'green'), rank.order = F)

train$age <- round(as.numeric(train$age))
train$pclass <- round(as.numeric(train$pclass))

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        out[i] <- mean(data[data$pclass==1,"age"], na.rm=T)
      }else if (class[i] == 2){
        out[i] <- mean(data[data$pclass==2,"age"], na.rm=T)
      }else{
        out[i] <- mean(data[data$pclass==3,"age"], na.rm=T)
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

train$age <- round(impute_age(train$age, train$pclass))

library(naniar)
vis_miss(train)
gg_miss_upset(train, nsets = n_var_miss(train))

extraTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(train)) {
  titles <- c(titles, extraTitle(train[i, "name"]))
}
train$title <- as.factor(titles)

train <- train[-c(3,7,8,10)]
train[!complete.cases(train),]
train <- na.omit(train)

train$survived <- factor(train$survived)
train$pclass <- factor(train$pclass)
train$sex <- factor(train$sex)
train$sibsp <- factor(train$sibsp)
train$fare <- as.numeric(train$fare)
train$embarked <- NULL
train$title <- factor(train$title)

str(train)

log.model <- glm(survived ~ . , family = binomial(link = 'logit'), data = train)
summary(log.model)

# CHECK ACCURACY on train

library(caTools)
split <- sample.split(train$survived, SplitRatio = 0.7)
train.check <- subset(train, split == T)
test.check <- subset(train, split == F)

log.model.check <- glm(survived ~ . , binomial(link='logit'), train.check)
summary(log.model.check)

prob_pred <- predict(log.model.check, type = 'response', newdata = test.check[-1])
prob_pred

y_pred <- ifelse(prob_pred > 0.5, 1, 0)
y_pred

outcome <- cbind(test.check, y_pred)
outcome_check <- subset(outcome, survived == y_pred)

cm <- table(y_pred,test.check[ ,1]) # 83%
cm
cm <- as.data.frame(cm)
wrong <- paste0("Wrong: ", round(sum(subset(cm, y_pred != Var2)$Freq)/(sum(cm$Freq)/100),2),"%")
wrong
correct <- paste0("Correct: ", round(sum(subset(cm, y_pred == Var2)$Freq)/(sum(cm$Freq)/100),2),"%")
correct

# PREDICTION test

mean_input1 <- mean(data[data$pclass==1,"age"], na.rm=T)
mean_input1
test[is.na(test$age) & test$pclass==1, "age"] <- mean_input1
mean_input2 <- mean(data[data$pclass==2,"age"], na.rm=T)
mean_input2
test[is.na(test$age) & test$pclass==2, "age"] <- mean_input2
mean_input3 <- mean(data[data$pclass==3,"age"], na.rm=T)
mean_input3
test[is.na(test$age) & test$pclass==3, "age"] <- mean_input3

extraTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(test)) {
  titles <- c(titles, extraTitle(test[i, "name"]))
}
test$title <- as.factor(titles)

test <- test[-c(1,3,7,8,10)]
test[!complete.cases(test),]
test <- na.omit(test)

test$pclass <- factor(test$pclass)
test$sex <- factor(test$sex)
test$sibsp <- factor(test$sibsp)
test$fare <- as.numeric(test$fare)
test$embarked <- NULL
test$title <- factor(test$title)

str(test)

prob_pred <- predict(log.model, type = 'response', newdata = test)
prob_pred

y_pred <- ifelse(prob_pred > 0.5, 1, 0)
y_pred

test <- cbind(survived=y_pred,test)

# OWN PREDICTION with train

own <- read.table(header = TRUE, text = "
  pclass    sex      age   sibsp  fare   title
  3         male     27    2      10     Mr.
  3         female   24    1      10     Miss.
  3         male     26    3      1000   Mr.
  3         female   26    1      1000   Miss.
")

own <- data.frame(lapply(own,as.character), stringsAsFactors = FALSE)

own$pclass <- as.factor(own$pclass)
own$sex <- as.factor(own$sex)
own$age <- as.numeric(own$age)
own$sibsp <- as.factor(own$sibsp)
own$fare <- as.numeric(own$fare)
own$title <- as.factor(own$title)

str(own)

prob_pred <- predict(log.model, type = 'response', newdata = own)
prob_pred

y_pred <- ifelse(prob_pred > 0.5, 1, 0)
y_pred

own <- cbind(survived=y_pred,own)
own
