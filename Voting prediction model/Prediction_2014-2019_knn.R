
library(tidyverse)



lok_2019 <- read.csv("D:/Janta ka mood Intern/Model/loksabha_2019.csv")
lok_2014 <- read.csv("D:/Janta ka mood Intern/Model/loksabha_2014 new.csv")

glimpse(lok_2019)
glimpse(lok_2014)


loksabha_2019 <- lok_2019 %>% select(Constituency,Party,Criminal.Cases,Education,Age,Winner,Total.Assets,Liabilities)
loksabha_2014 <- lok_2014 %>% select(Constituency,Party,Criminal.Cases,Education,Age,Winner,Total.Assets,Liabilities)


education_level <- c( "10th Pass","Literate","Post Graduate","8th Pass", "12th Pass","Graduate","Others","Graduate Professional","Doctorate","5th Pass","Not Given","Illiterate","")
education_label <- c(1:13)

loksabha_2019$Education <- as.integer(factor(loksabha_2019$Education,levels = education_level, labels = education_label))
loksabha_2014$Education <- as.integer(factor(loksabha_2014$Education,levels = education_level,labels = education_label))


constituency_level <- unique(loksabha_2019$Constituency)
loksabha_2019$Constituency <- as.integer(factor(loksabha_2019$Constituency,levels = constituency_level,labels = c(1:40)))
loksabha_2014$Constituency <- as.integer(factor(loksabha_2014$Constituency,levels = constituency_level,labels = c(1:40)))

str(loksabha_2014)

loksabha_2014$Total.Assets[is.na(loksabha_2014$Total.Assets)]  <- "0"
loksabha_2019$Total.Assets[is.na(loksabha_2019$Total.Assets)]  <- "0"
loksabha_2014$Liabilities[is.na(loksabha_2014$Liabilities)]  <- "0"
loksabha_2019$Liabilities[is.na(loksabha_2019$Liabilities)]  <- "0"

na.omit(loksabha_2014)
na.omit(loksabha_2019)


loksabha_2019$Total.Assets <- strtoi(gsub("[RsÂ,]","",loksabha_2019$Total.Assets))
loksabha_2019$Liabilities <- strtoi(gsub("[RsÂ,]","",loksabha_2019$Liabilities))
loksabha_2014$Total.Assets <- strtoi(gsub("[RsÂ,]","",loksabha_2014$Total.Assets))
loksabha_2014$Liabilities <- strtoi(gsub("[RsÂ,]","",loksabha_2014$Liabilities))

str(loksabha_2019)
str(loksabha_2014)

x <- unique(loksabha_2019$Party)
y <- unique(loksabha_2014$Party)
z <- c(x,y)
part_level <- unique(z)
party_label <- c(1:length(part_level))

print(part_level)

loksabha_2019$Party <- strtoi(factor(loksabha_2019$Party,levels = part_level,labels = party_label))
loksabha_2014$Party <- strtoi(factor(loksabha_2014$Party,levels = part_level,labels = party_label))



loksabha_2014$Winner <- as.factor(loksabha_2014$Winner)
loksabha_2019$Winner <- as.factor(lok_2019$Winner)

str(loksabha_2014)
str(loksabha_2019)


dim(loksabha_2014)
dim(loksabha_2019)

loksabha_2014 <- na.omit(loksabha_2014)
loksabha_2019 <- na.omit(loksabha_2019)

train_y <- loksabha_2014$Winner
train_x <- subset(loksabha_2014,select=-Winner)



test_y <- loksabha_2019$Winner
test_x <- subset(loksabha_2019,select=-Winner)

#Normalization

library(caret)
colnames(train_x[3:7])
q <- preProcess(train_x[3:7],method=c("center", "scale"))
train_x[3:7] <- predict(q,train_x[3:7])
train_x

colnames(test_x[3:7])
q <- preProcess(test_x[3:7],method=c("center", "scale"))
test_x[3:7] <- predict(q,test_x[3:7])
test_x

train_x[is.na(train_x)==TRUE]
test_x[is.na(test_x)==TRUE]
train_y[is.na(train_y)==TRUE]
test_y[is.na(test_y)==TRUE]

# Counting Actual sits

Actual_sits <- loksabha_2019 %>% filter(Winner==1) %>% select(Party)
Actual_sits <- Actual_sits %>% group_by(Party) %>% mutate(count=n())
Actual_sits <- unique(Actual_sits)
Actual_sits$Party <- factor(Actual_sits$Party,levels = party_label,labels = part_level)
Actual_sits


# KNN model

library(caTools)
library(class)
classifier_knn <- knn(train = train_x, test = test_x, cl = train_y, k = 2)
#classifier_knn
length(classifier_knn[classifier_knn==1])

knn_pred_sits <- mutate(test_x,classifier_knn)
knn_pred_sits <- knn_pred_sits %>% filter(classifier_knn==1) %>% select(Party)
knn_pred_sits <- knn_pred_sits %>% group_by(Party) %>% mutate(count=n())
knn_pred_sits <- unique(knn_pred_sits)
knn_pred_sits$Party <- factor(knn_pred_sits$Party,levels = party_label,labels = part_level)
knn_pred_sits

# Confusion Matrix

confusionMatrix(table(test_y, classifier_knn))
