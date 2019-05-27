setwd("C:/Sourav/Technology/Trainings/Data Analytics with R, Excel & Tableau/R_Directory/Files/")
library(readr)
#FE2010 <- read_csv("FE2010.csv")
FE2010 <- readxl::read_xlsx("FE2010.xlsx", col_names = TRUE)

View(FE2010)
str(FE2010)
summary(FE2010)

#install.packages("party")
#install.packages("randomForest")
#install.packages("ROCR")

library(ggplot2)
library(reshape2)
library(corrplot)
library(e1071)
library(caret)
library(rpart)
library(C50)
library(party)
#library(partykit)
library(randomForest)
library(ROCR)
library(dplyr)
library(car)

mydata<-FE2010
head(mydata)
summary(mydata)
View(mydata)
sapply(mydata, sd)
cormatrix <- round(cor(mydata), digits = 2 )
cormatrix

plot.new()
plot(mydata$FE ~mydata$NumCyl)
title('Basic Scatterplot')

library("ggplot2")
ggplot(mydata, aes(x=mydata$EngDispl)) +
      geom_histogram(binwidth = 1, fill = "white", color = "purple")
plot(mydata$FE,mydata$VarValveLift)
plot(mydata$FE,mydata$VarValveTiming)
plot(mydata$FE,mydata$ExhaustValvesPerCyl)
plot(mydata$FE,mydata$IntakeValvePerCyl)
plot(mydata$FE,mydata$TransCreeperGear)
plot(mydata$FE,mydata$TransLockup)
plot(mydata$FE,mydata$NumGears)
plot(mydata$FE,mydata$NumCyl)
plot(mydata$FE,mydata$EngDispl)
cor(mydata$FE,mydata$EngDispl)
cor(mydata$FE,mydata$VarValveLift)
cor(mydata$FE,mydata$VarValveTiming)
cor(mydata$FE,mydata$ExhaustValvesPerCyl)
cor(mydata$FE,mydata$IntakeValvePerCyl)
cor(mydata$FE,mydata$TransCreeperGear)
cor(mydata$FE,mydata$TransLockup)
cor(mydata$FE,mydata$NumGears)
cor(mydata$FE,mydata$NumCyl)

mod=lm(mydata$FE~mydata$EngDispl)
mod
summary(mod)
predict(mod)
pred=predict(mod)
mydata$predicted=NA
mydata$predicted=pred
mydata$error=mod$residuals

library(car)
dwt(mod)
plot(mydata$FE,mydata$EngDispl,abline(lm(mydata$FE~mydata$EngDispl),col="orange"))
#Assumption1 Linearity
plot(mydata$FE,mydata$error,xlab="FE",ylab="Residuals",main="Linearity")
#Assumption 2 constant error variance
plot(mydata$predicted,mydata$error, xlab="predicted", ylab="Residuals",main="constant error variance")
#Assumption 3 constant error variance
#plot(mydata$observation.no,mydata$error, xlab="observation.no",ylab="Residuals",main="independence of error")
#Assumption 4: Normality 
hist(mydata$error, xlab="Residuals", main="Histogram of Residuals",col="blue")
#Save all newly inserted variables like predicted, error along with original variables in a new file
head(mydata)
FE2010new<-mydata
head(FE2010new)

write.csv(FE2010new,"C:/Sourav/Technology/Trainings/Data Analytics with R, Excel & Tableau/R_Directory/Files/FE2010new.csv")
names(FE2010)
fit<-lm(FE~EngDispl+NumCyl+NumGears+TransLockup+TransCreeperGear+IntakeValvePerCyl+ExhaustValvesPerCyl+VarValveTiming+VarValveLift,data=FE2010)
fit
summary(fit)
vif(fit)
vif(fit)>5

setwd("C:/Sourav/Technology/Trainings/Data Analytics with R, Excel & Tableau/R_Directory/Files/")
library(readr)
FE2010 <- read_csv("FE2010new.csv")
View(FE2010new)


