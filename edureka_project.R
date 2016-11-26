retaildata = read.csv("Retail_Case_Study_Data.csv", header = TRUE)
retailpath = file.path("~","datasets","Retail_Case_Study_Data.csv")
retaildata = read.csv(retailpath)
summary(retaildata)
str(retaildata)
View(retaildata)
library(caret)

########################Decision Tree#####################################

library(tree)
library(caTools)
library(party)
set.seed(1500)
attach(retaildata)
sample1 = sample.split(retaildata$Sale.Made, SplitRatio = 0.7)
Traindata = subset(retaildata, sample1 == TRUE)
Testdata = subset(retaildata, sample1 == FALSE)
nrow(Traindata)
nrow(Testdata)
tree.retail = tree(Sale.Made ~., data = Traindata, control = tree.control(nobs = nrow(Traindata), mindev = 0, minsize = 15))
rplot = plot(tree.retail)
text(tree.retail)
tree.predict= predict(tree.retail, data = Testdata, type = "tree")
tree.predict
ctree.retail = ctree(Sale.Made ~ ., data = Traindata)
summary(ctree.retail)
plot(ctree.retail)
plot(ctree.retail, type = "simple")
print(ctree.retail)
predict.retail = predict(ctree.retail, data = Traindata)
predict.retail
Testdata$Sale.Made
table(predict.retail, Traindata$Sale.Made)
confusionMatrix(predict.retail, Traindata$Sale.Made)
pred = prediction(tree.predict, labels = Testdata$Sale.Made)
roc = performance(pred, "tpr", "fpr")
library(rpart)
auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc


###############################Logistic Regression#########################################


logretail = glm(Sale.Made ~ ., data = Traindata, family = "binomial")
summary(logretail)
logretail.pred = predict(logretail, data = Traindata, type = "response")
logretail.pred
logretail.pred[logretail.pred >= 0.5] = 1
logretail.pred[logretail.pred < 0.5] = 0
table(logretail.pred)
table(logretail.pred, Traindata$Sale.Made)
library(ROCR)
pred2 = prediction(logretail.pred, labels = Traindata$Sale.Made)
pred2
roc2 = performance(pred2, "tpr", "fpr")
aucretail = performance(pred2, "auc")
aucretail
aucretail = unlist(aucretail@y.values)
aucretail

########################################Random forest##############################################


library(randomForest)
str(retaildata)
Traindata$Sale.Made = na.omit(Traindata$Sale.Made)
rm(retaildata)
View(retaildata)
retailpath = file.path("~","datasets","Retail_Case_Study_Data.csv")
retaildata = read.csv(retailpath)
View(retaildata)
str(retaildata)
retaildata$Spend.Numeric = as.factor(retaildata$Spend.Numeric)
retaildata$Spend.Category = as.numeric(retaildata$Spend.Category)
retaildata$Area = as.numeric(retaildata$Area)
retaildata$Purchase.Channel = as.numeric(retaildata$Purchase.Channel)
str(retaildata)
rm(retaildata)
retailpath = file.path("~","datasets","Retail_Case_Study_Data.csv")
retaildata = read.csv(retailpath)
View(retaildata)
str(retaildata)
retaildata$Spend.Category = as.numeric(retaildata$Spend.Category)
retaildata$Area = as.numeric(retaildata$Area)
retaildata$Purchase.Channel = as.numeric(retaildata$Purchase.Channel)
levels(retaildata$Purchase.Channel)
str(retaildata)
retaildata = na.omit(retaildata)
str(retaildata)
retaildata$Purchase.Channel = gsub("^Phone","Other",retaildata$Purchase.Channel)
retaildata$Purchase.Channel = gsub("^Web","Other",retaildata$Purchase.Channel)
retaildata$Purchase.Channel = gsub("^Multichannel","Multichannel",retaildata$Purchase.Channel)
str(retaildata)
retaildata$Purchase.Channel = as.numeric(retaildata$Purchase.Channel)
str(retaildata)
levels(retaildata$Area)
retaildata$Area = gsub("^Rural","Rural",retaildata$Area)
retaildata$Area = gsub("^Surburban","Urban",retaildata$Area)
retaildata$Area = gsub("^Urban","Urban",retaildata$Area)
str(retaildata)
retaildata$Area = as.numeric(retaildata$Area)
str(retaildata)
levels(retaildata$Spend.Category)
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "1) $0 - $100")] = "1"
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "2) $100 - $200")] = "2"
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "3) $200 - $350")] = "3"
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "4) $350 - $500")] = "4"
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "5) $500 - $750")] = "5"
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "6) $750 - $1,000")] = "6"
levels(retaildata$Spend.Category)[levels(retaildata$Spend.Category == "7) $1,000 +")] = "7"
str(retaildata)
retaildata$Spend.Category = as.numeric(retaildata$Spend.Category)
str(retaildata)
head(retaildata$Spend.Category)
library(randomForest)
test1$Sale.Made <- as.factor(test1$Sale.Made)
rf.retail = randomForest(Sale.Made ~.,data = test1, ntree = 200)
print(rf.retail)
rf.pred1 = predict(rf.retail, type = "response", newdata = Testdata)
rf.pred1
table(rf.pred1, Testdata$Sale.Made)
rf.preds
nrow(rf.preds)
rf.preds[rf.preds >= 0.5] = 1
rf.preds[rf.preds < 0.5] = 0
rf.preds
table(rf.preds, Testdata$Sale.Made)
pred1 = prediction(rf.preds, Testdata$Sale.Made)
pred1
rocnew = performance(pred1,"tpr","fpr") 
aucnew = performance(pred1, "auc")
aucnew = unlist(aucnew@y.values)
aucnew
auc
aucretail


#########################  ROC Plots ##########################################################

plot(roc, type = "l")
plot(roc2, type = "l", add = TRUE, col = "red")
plot(rocnew, type = "l", add = TRUE, col = "blue")
