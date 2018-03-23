rm(list=ls())
library(neuralnet)
library(xts)
library(zoo)
library(quantmod)
library(tidyr)
library(PerformanceAnalytics)
library(reshape2)
library(ggplot2)

Bimbo<-na.omit(getSymbols("BIMBOA.MX",src="yahoo",auto.assign = F))
summary(Bimbo)
dim(Bimbo)
sapply(df, class)
head(Bimbo)#2007-01-02
tail(Bimbo)#2018-03-21
length(Bimbo) #16818
set.seed(30)

colnames(Bimbo)<-c("Open","High","Low","Close","Volume","Adjusted")
View(Bimbo)
plot(Op(Bimbo),type = 'l',col="blue")
plot(Ad(Bimbo),type = 'l',col="red")

plot(Bimbo$High,type = 'l',col="green") 
lines(Bimbo$Low,type = 'l',col="deeppink")

str(Bimbo)
hist(Bimbo$Volume)
hist(Bimbo$Close)

head(Bimbo,2,range)
apply(Bimbo,2,range)


minValue<-apply(Bimbo,2,min)
minValue
maxValue<-apply(Bimbo, 2, max)
maxValue

#Training data
t1<-sample(1:nrow(Bimbo),1950)
tr1<-Bimbo[t1,]
dim(tr1)

#Test data
te1<-Bimbo[-t1,]
dim(te1)

Bimbo<-as.data.frame(scale(Bimbo,center = minValue,scale = maxValue-minValue))
View(Bimbo)
ind<-sample(1:nrow(Bimbo),1950)
length(ind)
trainBm<-Bimbo[ind,]
testBm<-Bimbo[-ind,]

dim(trainBm)
dim(testBm)

allVars<-colnames(Bimbo)
allVars

predictorVarss<-allVars[!allVars%in%"Close"]
predictorVarss
predictorVarss<-paste(predictorVarss,collapse = "+")
predictorVarss

form=as.formula(paste("Close~",predictorVarss,collapse = "+"))
form

neuralModel<-neuralnet(formula = form,hidden = c(4,2),linear.output = T,data = trainBm)
plot(neuralModel)


predictions<-compute(neuralModel,testBm[,1:5])
str(predictions)

pred<-predictions$net.result

predictions<-pred*(max(testBm$Close)-min(testBm$Close))+min(testBm$Close)
actualValues<-(testBm$Close)*(max(testBm$Close)-min(testBm$Close))+min(testBm$Close)

#Error minimo cuadrado
MSE<-sum((predictions-actualValues)^2/nrow(testBm))
MSE

plot(testBm$Close,predictions,col="red",main = "Reales vs Predecidos",pch=1,cex=0.9,type="p",xlab ="Actual",ylab="Predecido")
final<-(testBm$Close-predictions)
result<-(te1$Close+final)
