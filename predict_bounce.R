# Clear All Variables & Clear Screen
rm(list=ls())
cat("\014")

# Read in the Data
Webdata = read.csv("/Users/jadechen/Documents/Customer/final project/Webdata.b.csv")
Webdata.b<-na.omit(Webdata)
Webdata.b$isgoogle<-ifelse(Webdata.b$source == "google",1,0)
Webdata.b$isbing<-ifelse(Webdata.b$source == "bing",1,0)
Webdata.b$isyahoo<-ifelse(Webdata.b$source == "yahoo",1,0)
Webdata.b$isdirect<-ifelse(Webdata.b$source == "(direct)",1,0)
Webdata.b$isrambler<-ifelse(Webdata.b$source == "nova.rambler.ru",1,0)

# Explore the data
str(Webdata.b)
summary(Webdata.b)

# Creating Training and Testing Sets
library(caTools)

set.seed(88)
split = sample.split(Webdata.b$bounce, SplitRatio = 0.70)
WebdataTrain = subset(Webdata.b, split==TRUE)
WebdataTest = subset(Webdata.b, split==FALSE)

# Build a Logistic Regression Model
WebdataModel = glm(bounce ~ pageviews+isgoogle+isbing+isyahoo+isdirect+isrambler+timeOnSite, data = WebdataTrain, family=binomial)
summary(WebdataModel)

cor(Webdata.b$PVs.Per.Visit,Webdata.b$newVisits)
cor(Webdata.b$PVs.Per.Visit,Webdata.b$timeOnSite)
cor(Webdata.b$PVs.Per.Visit,Webdata.b$pageviews)
cor(Webdata.b$visits,Webdata.b$pageviews)
cor(Webdata.b$timeOnSite,Webdata.b$pageviews)


# Evaluating the Model
PredictTrain = predict(WebdataModel, type="response")
table(WebdataTrain$bounce, PredictTrain > 0.5) # Creates confusion matrix for t=0.5

# Testing model on new data
PredictTest = predict(WebdataModel, type="response", newdata=WebdataTest)
table(WebdataTest$bounce, PredictTest > 0.5) # Creates confusion matrix for t=0.5

#logistics regression accuracy is 0.83
(340+3813)/(340+734+98+3813)

#baseline is 0.78
nrow(WebdataTrain)
sum(WebdataTrain$bounce)
sum(WebdataTest$bounce)/nrow(WebdataTest)


# ROC Curve
library(ROCR)

ROCRpred = prediction(PredictTest, WebdataTest$bounce)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)

# Making ROC Curve more informative
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)
