getwd()
setwd("/Users/alicewang/Downloads")


library(readxl)
library(dplyr)
library(ggplot2)
library(caTools)

data <- read_excel('data_student_learning_habit_covid19-200408.xlsx', sheet = 1)

data$total_learn <- data$LHInstruction + data$LH_w_Instruction

data = data %>%
  select(eff_env, eff_resource, kno_elearn, school_type, income, Lh_before_Cov, Lh_in_Cov, total_learn, Total_Online, Total_offline, nec_prog)
str(data)

#data$school_type <- as.factor(data$school_type)
levels(data$school_type) <- c("Normal public", "Gifted public", "Private", "International")
#keep school_type as numeric

#data$income <- as.factor(data$income)
levels(data$income) <- c("<430", "430-860", "860-1290", "1290-1720", "1720-2150", ">2150")

data$nec_prog <- as.factor(data$nec_prog)
levels(data$nec_prog) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

#data$Lh_before_Cov <- as.factor(data$Lh_before_Cov)
levels(data$Lh_before_Cov) <- c("Under_4", "4<7", "Above_7")

#data$Lh_in_Cov <- as.factor(data$Lh_in_Cov)
levels(data$Lh_in_Cov) <- c("Under_4", "4<7", "Above_7")

#data$eff_env <- as.factor(data$eff_env)
levels(data$eff_env) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

#data$eff_resource <- as.factor(data$eff_resource)
levels(data$eff_resource) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

#data$kno_elearn <- as.factor(data$kno_elearn)
levels(data$kno_elearn) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

##########ELENA'S COMMENT#######
## KNN from class package might not treat factors appropriately, so you have 2 options
## keep school type as numeric, which is not ideal from interpretation standpoint but fine.
## Another option is called 'one hot encoding' - this is a way to reporesent categorical 
## variable numerically. It creates a binary column for every category.
## We remove one column to escape multicollinearity issue.
#library(fastDummies)
#data <- fastDummies::dummy_cols(data,   select_columns = 'school_type',
                             #remove_first_dummy = TRUE,
                             #remove_selected_columns = TRUE)

#data <- data[,c(1:9, 11:13, 10)]
################################

#standartization 
scale_standardization <- function(x) {
  if (is.numeric(x)) {
    x = (x - mean(x))/sd(x)
  }
  return(x)
}

data_std <- as.data.frame(lapply(data[, c(1:11)], scale_standardization)) 
dim(data_std)

##########ELENA'S COMMENT#######
## Making sure you didn't lose your dependent variable that you have to keep as factor
## To fix this:
data_std <- cbind(data_std, data[,'nec_prog'])
dim(data_std)
################################


#split
set.seed(1443)
split <- sample.split(data_std$nec_prog, 0.7)
train <- data_std[split,]
test <- data_std[!split,]

#knn
library(class)

accuracies = list()
set.seed(1443)
for(k in 1:30){
  ########ELENA'S COMMENT######## this is what most probably caused the coersion:
  knn.pred <- knn(train[1:12], # indicate explicitly the feature vectors with indexes
                  test[1:12], # indicate explicitly the feature vectors with indexes
                  train$nec_prog, k=k)
  
  accuracies[k] <- mean(knn.pred == test$nec_prog)
}

accuracies

results <- as.data.frame(cbind(1:30, unlist(accuracies)))
colnames(results) <- c("k", "accuracy")

library(ggplot2)
ggplot(results, aes(k, accuracy)) + geom_line(col='#5EBD46') +
  scale_x_continuous(breaks = seq(1, 30, by = 1))

knn.pred <- knn(train[1:12], test[1:12], train$nec_prog, k=9)

matrix1 <- table(predictions = knn.pred, truth = test$nec_prog)
matrix1

accuracy <- mean(knn.pred == test$nec_prog)
accuracy
#9 is best k













#dont use

########ELENA'S COMMENT########
#Also you can try knn from caret package
#this implementation can handle factors automatically
data1 <- read_excel('data_student_learning_habit_covid19-200408.xlsx', sheet = 1)
data1$total_learn <- data1$LHInstruction + data1$LH_w_Instruction
data1 = data1 %>%
  select(eff_env, eff_resource, kno_elearn, school_type, income, Lh_before_Cov, Lh_in_Cov, total_learn, Total_Online, Total_offline, nec_prog)
data1$school_type <- as.factor(data1$school_type)
data1$nec_prog <- as.factor(data1$nec_prog)

#split
set.seed(1443)
split <- sample.split(data1$nec_prog, 0.7)
train <- data1[split,]
test <- data1[!split,]

#knn - caret
library(caret)
set.seed(1234)
knn_model <- train(nec_prog ~., data = train, method = "knn",

                 preProcess = c("center", "scale"),
                 tuneLength = 10)


knn_pred <- predict(knn_model, newdata = test)
knn_pred

table(knn_pred, test$nec_prog)

###############################

