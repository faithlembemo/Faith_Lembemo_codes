#Import data set from excel
  library(readxl)
  heart <- read_excel("~/Desktop/Propensity /final project/RSTUDIO/Faith_Lembemo_project_codes/heart.xlsx", 
                      sheet = "Heart")
  
View(heart)
 set.seed(2000000000)
  # check variable names
  #names(heart)
  
  #Check the summary of the data set if we have missing values, datatype
  #summary(heart)
  str(heart)
  #change the character variables to factor, so that we should check the frequencies 
  #for categorical variables  
  heart$Status<-as.factor(heart$Status)
  heart$DeathCause<-as.factor(heart$DeathCause)
  heart$Sex<-as.factor(heart$Sex)
  heart$Chol_Status<-as.factor(heart$Chol_Status)
  heart$BP_Status<-as.factor(heart$BP_Status)
  heart$Weight_Status<-as.factor(heart$Weight_Status)
  heart$Smoking_Status<-as.factor(heart$Smoking_Status)
  #summary for the changed variables
  summary(heart)
  
  #removing missing data for variables like : Smoking, Weight, Cholesterol and Height
  heart1 <- heart[which(is.na(heart$Cholesterol)==FALSE ),]
  heart2 <- heart1[which(is.na(heart1$Weight)==FALSE ),]
  heart3 <- heart2[which(is.na(heart2$Height)==FALSE ),]
  heart4 <- heart3[which(is.na(heart3$Smoking)==FALSE ),]
  #summary to check if the missing data has been removed
  summary(heart4)
  str(heart4)
  
  ## Creating new variables
  ### CHD
  CHD <- matrix(0, length(heart[,3])) 
  heart5 <- cbind(heart4[,-2], CHD)
  heart5[which(is.na(heart4[,3])==TRUE), which(names(heart5)=="CHD")] <- 0 # heart4[,3] is the AgeCHDdiag column
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
  summary(heart7)
  #recode the new creted categorical variables as factors
  heart7$Smoker<-as.factor(heart7$Smoker)
  heart7$Cholestrol_Status<-as.factor(heart7$Cholestrol_Status)
  heart7$CHD<-as.factor(heart7$CHD)
  
  ## Add Column for the Body Max Index(BMI) variable 
  ## I assume that height is measured in inches and weight in (lbs)
  
  BMI<- matrix(0, length(heart[,3]))
  Heart<-cbind(heart7, BMI)
  Heart$BMI<-round(((703*(heart7$Weight))/((heart7$Height)^2)))#,digits = 2)
  
  #View(Heart)
  summary(Heart)
  str(Heart)
  
  #########################
  # create a subset of the dataset  with only the variables that we will use for the analysis
  Sub_Heart<- Heart[c("CHD","Smoker", "Sex", "AgeAtStart", "Diastolic","Cholestrol_Status", "BP_Status" , "Systolic", "BMI")]
  
  summary(Sub_Heart)
  ############################################################################
    
  #### Baseline characteristics table
  #load package
  library(tableone)
  
  ###Create a variable list which we want
  listvariables<-c( "Sex", "AgeAtStart", "Diastolic","Cholestrol_Status", "BP_Status" , "Systolic", "BMI")
  
  ### Lets define our categorical variables
  Categorical_variables<-c("Sex","Cholestrol_Status", "BP_Status" )
  
  ### baseline characteristics for smokers and non smokers
  table1<-CreateTableOne(vars = listvariables, data = Sub_Heart, factorVars = Categorical_variables, strata = c("Smoker"),smd = T)
  table1<-print(table1, printToggle = FALSE, noSpaces = TRUE, smd = TRUE, quote = T)
  table1
  
  #################PROPENSITY SCORES######################
  ## Load package 
  library(MatchIt)
  
  # Estimate propensity scores using the regression model
  
  #First step : create a regression model with variable Smoker as a dependent variable
  PS_model <- "Smoker ~ Sex + AgeAtStart + BMI + Diastolic +Systolic + BP_Status + Cholestrol_Status "
  
  ## Function- matchit() calculates the propensity scores
  
  SubHeart_PS <- matchit(as.formula(PS_model), method="nearest", distance="logit", replace=FALSE,
                         ratio=1, caliper=0.1, discard="none", data=Sub_Heart)
  

  ## Create variables containing the propensity scores of all smokers and non-smokers before matching
  pscore.control.all <- SubHeart_PS$distance[Sub_Heart[,"Smoker"]==0]
  pscore.treated.all <- SubHeart_PS$distance[Sub_Heart[,"Smoker"]==1]
  
  #######################check the overlappimg of the propensity scores in both groups################

  p1 <- hist(pscore.control.all,plot=FALSE)
  p2 <- hist(pscore.treated.all,plot=FALSE)
  plot(1,1,type="n",xlim=c(0,1),ylim=c(0,500),xlab="Propensity Score",ylab="Freq",main=" Smoking groups")
  plot(p1,col="green",density=10,angle=135,add=TRUE)
  plot(p2,col="blue",density=10,angle=45,add=TRUE)
  
  
  #################         MATCHING        ####################
  
  ## Now lets create a dataset which only has matched participants 
  # it should also include propensity scores and weights
  
  all_matched <- cbind(Sub_Heart, SubHeart_PS$distance, SubHeart_PS$weights) 
  #View(all_matched)
  
  # Rename the columns of the propensity scores and the weights
  names(all_matched)[c(dim(all_matched)[2]-1, dim(all_matched)[2])] <- c("propensity_scores", "weights") 
  
  # match_heart2 it only has matched observations (weight > 0)
  match_heart2 <- subset(all_matched,weights!=0) 
  View(match_heart2)
  
  ### Display the distributions of the propensity scores before and after the matching
  set.seed(1563)
  
  ## Create weighted samples out of the matched smokers  and non smokers
  pscore.treated.matched <- sample(match_heart2$propensity_scores[match_heart2[,"Smoker"]==1], 10000, replace=TRUE,
                                   prob=match_heart2$weights[match_heart2[,"Smoker"]==1])
  pscore.control.matched <- sample(match_heart2$propensity_scores[match_heart2[,"Smoker"]==0], 10000, replace=TRUE,
                                   prob=match_heart2$weights[match_heart2[,"Smoker"]==0])
  
  ### Create 4 histograms of the propensity scores containing also the density
  
  
  #Histogram for the unmatched  treated group
  par(mfcol = c(2, 2))

  #par(mar=c(1,3,2,3))
  #Histogram for the unmatched  treated group
  hist(pscore.treated.all, xlab="Propensity Score", ylab="Density", main ="Unmatched Treated
  (Smoker)", freq=FALSE, breaks=10, xlim=c(0,1), ylim=c(0,3))
  lines(density(pscore.treated.all))
  
  #Histogram for the unmatched  control group
  hist(pscore.control.all, xlab="Propensity Score", ylab="Density", main ="Unmatched Control (Non
  Smoker)", freq=FALSE, breaks=10, xlim=c(0,1), ylim=c(0,3))
  lines(density(pscore.control.all))
  
  #Histogram for the matched  treated group
  hist(pscore.treated.matched, xlab="Propensity Score", ylab="Density", main ="Matched Treated
  (Smoker)", freq=FALSE, breaks=10, xlim=c(0,1), ylim=c(0,3))
  lines(density(pscore.treated.matched))
  
  #Histogram for the matched  control group
  hist(pscore.control.matched , xlab="Propensity Score", ylab="Density", main ="Matched Control
  (Non Smoker)", freq=FALSE, breaks=10, xlim=c(0,1), ylim=c(0,3))
  lines(density(pscore.control.matched))
 
  
  ### Baseline characteristics for smokers and non smokers
  table2<-CreateTableOne(vars = listvariables, data = match_heart2, factorVars = Categorical_variables, strata = c("Smoker"), smd=T)
  table2<-print(table2, printToggle = FALSE, noSpaces = TRUE, smd = TRUE, quote = T)
  table2
  
  ###LOGISTIC REGRESSION FOR MATCHED DATA SET  
  # model 1
  
  mylogit<-glm(CHD~Sex+ AgeAtStart+BMI+Diastolic+ Systolic+BP_Status+Cholestrol_Status+Smoker, data = match_heart2, family="binomial")
  summary(mylogit)
  
  # estimated odds ratio
  coef<-mylogit$coef
  exp(coef)    