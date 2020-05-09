############################### PROPENSITY SCORE METHOD CODES #################
#IMPORT DATASET
library(readxl)
heart <- read_excel("~/Desktop/Faith_Lembemo_project_codes/heart.xlsx", 
                    sheet = "Heart")
#View(heart)

#variable names
names(heart)

set.seed(2000000)

#Check the summary of the data set if we have missing values
summary(heart)
#Change all character variables to factor

heart$Status<-as.factor(heart$Status)
heart$DeathCause<-as.factor(heart$DeathCause)
heart$Sex<-as.factor(heart$Sex)
heart$Chol_Status<-as.factor(heart$Chol_Status)
heart$BP_Status<-as.factor(heart$BP_Status)
heart$Weight_Status<-as.factor(heart$Weight_Status)
heart$Smoking_Status<-as.factor(heart$Smoking_Status)
summary(heart)
#noted that smoking, weight, cholesterol, MRW, Height having missing values

##missing data checking the missing data here will 
first_percentage<-function(Cholestral){sum(is.na(Cholestral))/length(Cholestral)*100}
apply(heart,2,first_percentage)

## Exclude entries with missing data
## observed that Cholestrol and Weight_Status have missing values
heart1 <- heart[which(is.na(heart$Cholesterol)==FALSE ),]
heart2 <- heart1[which(is.na(heart1$Weight)==FALSE ),]
heart3 <- heart2[which(is.na(heart2$Height)==FALSE ),]
heart4 <- heart3[which(is.na(heart3$Smoking)==FALSE ),]
#View(heart4)
#check the percentage of missing data after removing the missing values 
first_percentage<-function(Cholestral){sum(is.na(Cholestral))/length(Cholestral)*100}
apply(heart4,2,first_percentage)
summary(heart4)

#Creating new variables
# Coronary heart disease (CHD) 
CHD <- matrix(0, length(heart[,3]))
heart5 <- cbind(heart4[,-2], CHD)
heart5[which(is.na(heart4[,3])==TRUE), which(names(heart5)=="CHD")] <- 0
heart5[which(is.na(heart4[,3])==FALSE), which(names(heart5)=="CHD")] <- 1

## Add column, if smoker (yes or no)
Smoker <- matrix(0, length(heart[,3]))
heart6 <- cbind(heart5, Smoker)
heart6[which(heart6[,which(names(heart6)=="Smoking")]<=5), which(names(heart6)=="Smoker")] <- 0
heart6[which(heart6[,which(names(heart6)=="Smoking")]>5), which(names(heart6)=="Smoker")] <- 1


## Add column, for cholestrol status with only two levels <=230 low cholestrol
Cholestrol_Status <- matrix(0, length(heart[,3]))
heart7 <- cbind(heart6, Cholestrol_Status)
heart7[which(heart7[,which(names(heart7)=="Cholesterol")]<=239), which(names(heart7)=="Cholestrol_Status")] <- 0
heart7[which(heart7[,which(names(heart7)=="Cholesterol")]>239), which(names(heart7)=="Cholestrol_Status")] <- 1

heart7$Smoker<-as.factor(heart7$Smoker)
heart7$Cholestrol_Status<-as.factor(heart7$Cholestrol_Status)
heart7$CHD<-as.factor(heart7$CHD)

## Add Column for the Body Max Index(BMI) 
## assuming that height is measured in inches and weight in (lbs)

BMI<- matrix(0, length(heart[,3]))
Heart<-cbind(heart7, BMI)
Heart$BMI<-round(((703*(heart7$Weight))/((heart7$Height)^2)))#,digits = 2)

View(Heart)
summary(Heart)


################################################################################################
################# SELECTION OF VARIABLES########################


## BACKWARD REGRESSION METHOD  ######

### Model 1 we include all variables that can have an effect on CHD

model1=glm(formula=CHD~Sex+ AgeAtStart+Diastolic+Systolic+Smoker+Cholestrol_Status+BP_Status+BMI  , data = Heart, family="binomial")# na.rm=TRUE)
summary(model1)  

## First step remove the variable with highest p-value ###########
model1<-update(model1,.~.-BP_Status)
summary(model1)


## third step remove the variable with highest p-value ###########
model1<-update(model1,.~.-Diastolic)
summary(model1)

########### LOGISTIC REGRESSION ######
# model 1
mylogit<-glm(CHD~Sex+ AgeAtStart+Systolic+Cholestrol_Status+BMI+Smoker, data = Heart, family="binomial")
summary(mylogit)

# estimated odds ratio
coef<-mylogit$coef
exp(coef)                   
