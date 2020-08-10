getwd()
setwd("/Users/alicewang/Downloads")

library(readxl)
library(dplyr)
library(ggplot2)
library(caTools)
#keep , , school_type, income, total learning hours, total online learning hours, total offline learning hours, "I can assure my learning progress" (independent)), kno_elearn

data <- read_excel('data_student_learning_habit_covid19-200408.xlsx', sheet = 1)

sum(is.na(data))
summary(data)
str(data)
head(data)

data <- data[!duplicated(data),]
#no missing or duplicated data, no outliers

data$total_learn <- data$LHInstruction + data$LH_w_Instruction

data = data %>%
  select(eff_env, kno_elearn, eff_resource, school_type, income, Lh_before_Cov, Lh_in_Cov, total_learn, Total_Online, Total_offline, nec_prog) #include kno_elearn

#as.factor conversions (only for visualizations and logistic regression)

data$school_type <- as.factor(data$school_type)
levels(data$school_type) <- c("Normal public", "Gifted public", "Private", "International")

data$income <- as.factor(data$income)
levels(data$income) <- c("<430", "430-860", "860-1290", "1290-1720", "1720-2150", ">2150")

data$nec_prog <- as.factor(data$nec_prog)
levels(data$nec_prog) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

data$Lh_before_Cov <- as.factor(data$Lh_before_Cov)
levels(data$Lh_before_Cov) <- c("Under_4", "4<7", "Above_7")

data$Lh_in_Cov <- as.factor(data$Lh_in_Cov)
levels(data$Lh_in_Cov) <- c("Under_4", "4<7", "Above_7")

data$eff_env <- as.factor(data$eff_env)
levels(data$eff_env) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

data$eff_resource <- as.factor(data$eff_resource)
levels(data$eff_resource) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

data$kno_elearn <- as.factor(data$kno_elearn)
levels(data$kno_elearn) <- c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")

#distributions
library(ggthemes)

ggplot(data, aes(nec_prog)) + geom_bar() + labs(title="Answers to 'I can assure my learning progress'", x="Responses", y="") + theme_economist()
ggplot(data, aes(school_type)) + geom_bar()
ggplot(data, aes(income)) + geom_bar()
ggplot(data, aes(Lh_before_Cov)) + geom_bar()
ggplot(data, aes(Lh_in_Cov)) + geom_bar()
ggplot(data, aes(Total_Online)) + geom_histogram()
ggplot(data, aes(Total_offline)) + geom_histogram()
ggplot(data, aes(eff_env)) + geom_bar()
ggplot(data, aes(eff_resource)) + geom_bar()

ggplot(data, aes(total_learn)) + geom_boxplot()
ggplot(data, aes(Total_offline)) + geom_boxplot()
ggplot(data, aes(Total_Online)) + geom_boxplot()

ggplot(data, aes(total_learn, col=income, fill=income)) + geom_histogram()

#used graphs
ggplot(data, aes(total_learn, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_histogram() + 
  labs(title="Assured Learning Progress and Total Learning Hours", x="Total Learning Hours", y="Frequency", fill='Responses', col='Responses') + theme_economist() #+ 
  #theme(axis.title.x=element_text(vjust=-2))
ggplot(data, aes(eff_env, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar() +
  labs(title="Assured Learning Progress and Total Learning Hours", x="I have an effective learning environment", y='Frequency', fill="Responses", col="Responses") + theme_economist()
ggplot(data, aes(nec_prog, col=school_type, fill=school_type)) + geom_bar() +
  labs(title="Assured Learning Progress and School Type", x="I can assure my learning progress", y="Frequency", fill="School Type", col="School Type") + theme_economist()
?theme

ggplot(data, aes(income, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar()
ggplot(data, aes(Lh_in_Cov, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar()
ggplot(data, aes(Lh_before_Cov, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar()
ggplot(data, aes(Total_Online, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_histogram()
ggplot(data, aes(Total_offline, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_histogram()
ggplot(data, aes(eff_resource, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar()
ggplot(data, aes(kno_elearn, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar()

ggplot(data, aes(Lh_in_Cov, col=as.factor(nec_prog), fill=as.factor(nec_prog))) + geom_bar() + facet_wrap(~Lh_before_Cov)


colnames(data)

#logistic regression
data$nec_prog <- as.numeric(data$nec_prog)

data <- data %>%
  filter(data$nec_prog != 3)

unique(data$nec_prog)

data$nec_prog <- ifelse(as.numeric(data$nec_prog) > 3, 1, 0)
data$nec_prog <- as.factor(data$nec_prog)

set.seed(1443)
split <- sample.split(data$nec_prog, 0.7)
train <- data[split,]
test <- data[!split,]

full_model <- glm(nec_prog~., family='binomial', data=train)
summary(full_model)

#most powerful: eff_env, eff_resource, LH_before_Cov, total_learn
#finish visualizations