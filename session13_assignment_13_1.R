#session13_assignment13_11

#1. Use the given link Data Set 
#   Answer the below questions: 
library(readr)
library(data.table)

getwd()
p<-"C:/Users/Swapna/Documents/R files test"
setwd(p)

COBRA_YTD<-read.csv("C:/Users/Swapna/Documents/R files test/crime-in-atlanta-2017/COBRA-YTD2017.csv")
View(COBRA_YTD) 
str(COBRA_YTD) 
summary(COBRA_YTD)
sum(is.na(COBRA_YTD))
COBRA_YTD[,is.na(COBRA_YTD$loc_type)]<- mean(COBRA_YTD, na.rm=TRUE)
library(car)
#a. Find out top 5 attributes having highest correlation (select only Numeric features).
  
fit<-lm(beat~MinOfucr+MaxOfnum_victims+loc_type+neighborhood+x+y,data =COBRA_YTD, na.action = na.omit)
fit  
summary(fit) 
vif(fit)
fit1<-lm(formula=MinOfucr~beat+MaxOfnum_victims+loc_type+neighborhood+x+y,data =COBRA_YTD)
fit1  
summary(fit1)
vif(fit1)
vif(fit)>5
vif(fit1)>5

#b. Find out top 3 reasons for having more crime in a city.



COBRA_YTD$hour <- sub(":.*", "", COBRA_YTD$occur_time)
COBRA_YTD$hour <- as.numeric(COBRA_YTD$hour)
ggplot(aes(x = hour), data = COBRA_YTD) + geom_histogram(bins = 24, color='white', fill='red') +
  ggtitle('Histogram of Crime Time') 


z<-table(COBRA_YTD$UC2.Literal) 
hist(z)

#c. Which all attributes have correlation with crime rate? 
library(ggplot2)
pairs(COBRA_YTD)

library(corrplot)

rank1<-sample(COBRA_YTD[1:100,22:23], 20, replace=T)
rank2<-sample(COBRA_YTD[1:100,22:23], 20, replace=T)  
cbind(rank1,rank2)
plot(rank1, rank2)
cor(rank1,rank2, method="spearman")
cor(rank1,rank2, method="pearson")
