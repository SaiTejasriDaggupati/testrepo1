df <- read.csv("C:\\Users\\user\\Downloads\\Concrete_Data (1).csv",headers=TRUE,sep = ",")

library(dplyr)

library(PerformanceAnalytics)
#install.packages("PerformanceAnalytics")
library(ggplot2)
install.packages(chart.Correlation)
library(corrplot)
install.packages(corrplot)
library(RColorBrewer)
library(ggthemes)

library(caret)

library(caretEnsemble)

library(doParallel)

# EDA
summary(df)
corrplot(cor(df), method = "number",tl.cex = 0.5)
chart.Correlation(df)
anyNA(df)
sapply(df, {function(x) any(is.na(x))})
boxplot(df[-9], col = "orange", main = "Features Boxplot")
boxplot(df$Age, col = "red")
age_outliers <- which(df$Age > 100)
df[age_outliers, "Age"]


#simple linear regression for total data

#simple_lm <- lm(strength ~ ., data = df)
#summary(simple_lm)
#coef(simple_lm)
#predict = predict(simple_lm,df[,])
#mean((df[,]$strength-predict(simple_lm,df[,]))^2)

#train and test data

set.seed(5)  # setting seed to reproduce results of random sampling
testData<- sample(nrow(df),200)  # row indices for training data

lm_fit <- lm(strength ~ Cement+Water+Age+Coarse.Aggregate+Fly.Ash+Fine.Aggregate+Superplasticizer+Blast.Furnace.Slag ,
                data = df, subset = -testData)
summary(lm_fit)
pred <- predict(lm_fit, df[testData,])
pred

summary(pred)
#mean square error
mean((df[testData,]$strength-pred)^2)
#correlation betwwen oringinal and prediction values
cor(df[testData,]$strength,predict(lm_fit,df[testData,]))
ggplot(data=testData,aes(x=1:nrow(testData)))+geom_line(aes(y=strength,color="blue"))+geom_line(aes(y=pred,colour="red"))+
  labs(title="actual strength vs predicted strength",x="",color="")+scale_color_manual(labels=c("actual","predicted"),values = c("red","black"))


set.seed(5)  # setting seed to reproduce results of random sampling
testData<- sample(nrow(df),200)  # row indices for training data
test1=data.frame(testData)
lm_fit <- lm(strength ~ Cement+Water+Age+Coarse.Aggregate+Fly.Ash+Fine.Aggregate+Superplasticizer+Blast.Furnace.Slag ,
             data = df, subset = -testData)
summary(lm_fit)
pred <- predict(lm_fit, df[testData,])
pred
pred1=data.frame(pred)
summary(pred)
#mean square error
mean((df[testData,]$strength-pred)^2)
#correlation betwwen oringinal and prediction values
cor(df[testData,]$strength,predict(lm_fit,df[testData,]))
ggplot(data=test1,aes(x=1:nrow(test1),inherit.aes = FALSE))+geom_line(aes(y=strength,color="blue"),inherit.aes = FALSE)+geom_line(aes(y=pred1,colour="red"),inherit.aes = FALSE)+
  labs(title="actual strength vs predicted strength",x="",color="")+scale_color_manual(labels=c("actual","predicted"),values = c("red","black") )























