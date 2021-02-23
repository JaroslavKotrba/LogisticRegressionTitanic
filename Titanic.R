# TITANIC -------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

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
