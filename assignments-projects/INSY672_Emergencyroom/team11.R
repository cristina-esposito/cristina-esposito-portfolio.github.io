################################################################
# INSY 672 Group Assignment
# Team 11
# Cristina Esposito and Mohamad Khalili
################################################################

#### TABLE OF CONTENTS
# 1. DATA CLEANING - line 47
# 2. EXPLORATION - line 164 
# 3. FEATURE SELECTION - line 204
# 4. BUILDING DIFFERENT MODELS - line 270
# 5. CHOSEN MODEL - line 543
# 6. APPLY THE TEST DATA - line 580


#### INSTRUCTIONS
# An .Rdata file was provided with the submission. When importing this, if it was not done correctly
# or there are issues with the file, please run the following lines to generate the chosen trained model:
#   - Section 1 - all lines
#   - Section 2 - line 196
#   - Section 5 - all lines
#   - Section 6 - all lines
# 
# If the .Rdata file was correct and you do not need to rerun all the code to generate the trained model, 
# you can go straight to running all lines in section 6 to generate the RMSE with the test data and the chosen model


################################################################


#import the data
library(readxl)
library(dplyr)
library(stringi)
library(lubridate)
library(mice)
library(caret)
library(caTools)

Case3Data <- read_excel("C:\\Users\\crist\\Desktop\\MCGILL\\3 - WINTER I\\INSY 672 HEALTHCARE ANALYTICS\\group assignment\\Case3Data.xlsm", col_types=c("date", "numeric", "text", "text", "text", "text", "numeric", "text", "numeric"))



#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#                     1. CLEANING
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
 
# check for null values
missingpattern=md.pattern(Case3Data)  # no null values

# look at data summary to spot any outliers or weird numbers
summary(Case3Data)

# there are 274 observations with min age less than 0. We will change these values to NA and apply complete() to impute
zero=Case3Data[Case3Data$Age<0,]
Case3Data[Case3Data$Age<0,]$Age=NA

# looking at the arrival model
table(Case3Data$Arrival.Model) #4 types, 8 marked invalid

# looking at the complain system
table(Case3Data$Chief.Complain.System) # can group ENVIRONMENTAL and Environmental

# grouping environmental together
Case3Data$Chief.Complain.System<-replace(Case3Data$Chief.Complain.System,Case3Data$Chief.Complain.System=="ENVIRONMENTAL","Environmental")

table(Case3Data$Chief.Complain.System) # 16 types, 94 marked Invalid, replace with NA and apply complete()

# link about triage acute score: https://www.researchgate.net/figure/Five-level-Triage-Systems_tbl1_51701713
table(Case3Data$Triage.Acute.Score) # 1-5, the higher the score less urgent

# looking at gender
table(Case3Data$Gender) # 3 gender, Female, Male, and "T" --> can assume transgender

# drop discharged home and start to disposition, as these are only assumed after the response variable
Case3Data<-select(Case3Data, -c("Discharged Home","Start.to.Disposition..Min."))



#### Creating new features
# creating time related variables
Case3Data$Hour = as.numeric(stri_sub(Case3Data$Startdatetime,12,13))
Case3Data$Month = as.numeric(stri_sub(Case3Data$Startdatetime,6,7))
Case3Data$Date = ymd(stri_sub(Case3Data$Startdatetime,1,10))


# grouping the hours into 3 hour bins
Case3Data$TOD = ifelse(
  Case3Data$Hour >= 0 & Case3Data$Hour < 3, "12AM-3AM",
  ifelse(
    Case3Data$Hour >= 3 & Case3Data$Hour < 6, "3AM-6AM",
    ifelse(
      Case3Data$Hour >= 6 & Case3Data$Hour <9, "6AM-9AM",
      ifelse(
        Case3Data$Hour >= 9 & Case3Data$Hour <12, "9AM-12PM",
        ifelse(
          Case3Data$Hour >= 12 & Case3Data$Hour < 15, "12PM-3PM",
          ifelse(
            Case3Data$Hour >= 15 & Case3Data$Hour <18, "3PM-6PM",
            ifelse(
              Case3Data$Hour >=18 & Case3Data$Hour < 21, "6PM-9PM", "9PM-12AM"
            )
          )
        )
      )
    )
  )
)
table(Case3Data$TOD)

# creating seasons
Case3Data$Season = ifelse(Case3Data$Month>=3 & Case3Data$Month<6,"Spring",ifelse(Case3Data$Month>=6 & Case3Data$Month<9,"Summer",ifelse(Case3Data$Month>=9 & Case3Data$Month<12,"Fall","Winter")))
table(Case3Data$Season)

# creating day of the week
Case3Data$dayoftheweek = wday(Case3Data$Date, label=TRUE, abbr=FALSE)
# refactor the contents
Case3Data$dayoftheweek= as.factor(ifelse(Case3Data$dayoftheweek=="Monday","Monday",ifelse(Case3Data$dayoftheweek=="Tuesday","Tuesday",ifelse(Case3Data$dayoftheweek=="Wednesday","Wednesday",ifelse(Case3Data$dayoftheweek=="Thursday","Thursday",ifelse(Case3Data$dayoftheweek=="Friday","Friday",ifelse(Case3Data$dayoftheweek=="Saturday","Saturday","Sunday")))))))

### creating waiting census
#function to create waiting census
countnumber = function(time,timestamps1,timestamps2){
  indices = which(time > timestamps1 & time < timestamps2)
  return(length(indices))
} 

Case3Data$SeeMDTime=as.POSIXct(Case3Data$Startdatetime+Case3Data$Time.to.MD..Min.*60) #compute Waiting times (min) for the training data


for(i in 1:nrow(Case3Data)){
  Case3Data$waitingcensus[i] <- countnumber(Case3Data$Startdatetime[i],Case3Data$Startdatetime,Case3Data$SeeMDTime) #compute waiting census when a patient A arrived at the ED. To do that, we count the number of patients whose arrival time is earlier than Patient A's arrival time and whose seeMD time is later than patient A's arrival time.
}


# dropping remaining columns that won't be helpful
Case3Data<-select(Case3Data, -c("Startdatetime","Hour","Month","Date","SeeMDTime"))


# applying complete() to the NA values we created for age
missingpattern=md.pattern(Case3Data)
predictvars=Case3Data[,-c(6)]  
predictfull=complete(mice(predictvars))
missingpattern=md.pattern(predictfull)

# adding the response back in the case3data
Case3Data<-cbind(predictfull,Case3Data$Time.to.MD..Min.)
names(Case3Data)[10]<-"Time.to.MD..Min."


# applying formats to the variables
Case3Data = Case3Data %>% 
  mutate_at(vars(Age,Time.to.MD..Min.,waitingcensus), as.numeric) %>% 
  mutate_at(vars(Arrival.Model,Chief.Complain.System,Gender,TOD,Season,dayoftheweek,Triage.Acute.Score),
            as.factor)


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#                    2.  EXPLORATION
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------

#### Some exploration to understand patterns between predictors and the response variable
hist(Case3Data$Time.to.MD..Min.) # seems to peak around the 40 minute mark

# ground ambulance seems to get priority and has less wait time, air + groun is similar to no ambulance, but no ambulance has many outliers and is more spread out
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$Arrival.Model,xlab="Arrival Mode",ylab="Time to MD in Mins",main="Boxplot")

# all categories seem to be very consistent
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$Chief.Complain.System,xlab="Chief Complain System",ylab="Time to MD in Mins",main="Boxplot")

# As expected, score of 1 sees shorter wait time as it is more urgent
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$Triage.Acute.Score,xlab="Triage Acute Score",ylab="Time to MD in Mins",main="Boxplot")

# No preference towards any gender
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$Gender,xlab="Gender",ylab="Time to MD in Mins",main="Boxplot")

# 12am-9am seem to have shorter wait time
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$TOD,xlab="Time of Day",ylab="Time to MD in Mins",main="Boxplot")

# winter sees the most variation
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$Season,xlab="Season",ylab="Time to MD in Mins",main="Boxplot")

# weekends seem to have less wait time, mondays with the greatest variation
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$dayoftheweek,xlab="Day of the week",ylab="Time to MD in Mins",main="Boxplot")

# very clear that those who arrive by ambulance will receive priority and have shorter wait time 
plot(Case3Data$waitingcensus,Case3Data$Time.to.MD..Min., col=ifelse(Case3Data$Arrival.Model=="Ground Ambulance","red","blue"))
# regroup categories for arrival mode to be either ground ambulance or other
Case3Data$Arrival.Model<-ifelse(Case3Data$Arrival.Model=="Ground Ambulance","Ground Ambulance","Other")
boxplot(Case3Data$Time.to.MD..Min.~Case3Data$Arrival.Model,xlab="Arrival Mode",ylab="Time to MD in Mins",main="Boxplot")
table(Case3Data$Arrival.Model)


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#                3.  FEATURE SELECTION
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------

### running a random forest with 50 trees to see variable importance
set.seed (1)
require(randomForest)
#names(Case3Data)
myforest=randomForest(Time.to.MD..Min.~., ntree=50, data=Case3Data, importance=TRUE, na.action = na.omit)
myforest
importance(myforest)
varImpPlot(myforest)

# variables order by most to least important:
# - Arrival.Model 
# - waitingcensus 
# - Triage.Acute.Score 
# - dayoftheweek 
# - TOD
# - Season
# - Gender
# - Age
# - Chief.Complain.System



### running various regressions to see p-value significance
summary(lm(Time.to.MD..Min.~., data=Case3Data))
# - Arrival.ModelOther  --> increase wait time by 31 minutes
# - Chief.Complain.SystemMental Health --> reduces by 1 minute
# - Chief.Complain.SystemNeurologic --> reduces by 0.29 minutes
# - Chief.Complain.SystemSUBSTANCE MISUSE --> reduces by 0.32 minutes
# - Triage.Acute.Score --> the higher the score the higher the wait time
# - TOD --> almost all tod are significant
# - Season --> spring and summer reduce wait time, winter increases wait time
# - day of the week --> saturdays and sundays reduce wait time, thursdays and wednesdays increase 
# - waiting census --> for each additional person waiting, it increases wait time by 2.54 minutes
summary(lm(Time.to.MD..Min.~Arrival.Model, data=Case3Data))
summary(lm(Time.to.MD..Min.~waitingcensus, data=Case3Data))
summary(lm(Time.to.MD..Min.~Triage.Acute.Score, data=Case3Data))
summary(lm(Time.to.MD..Min.~dayoftheweek, data=Case3Data))

# trying DID
summary(lm(Time.to.MD..Min.~.+TOD*dayoftheweek, data=Case3Data)) # many are significant
summary(lm(Time.to.MD..Min.~.+TOD*Season, data=Case3Data)) # not significant for many pairings
summary(lm(Time.to.MD..Min.~.+dayoftheweek*Season, data=Case3Data)) # many not significant
summary(lm(Time.to.MD..Min.~.+Triage.Acute.Score*Chief.Complain.System, data=Case3Data)) # not significant
summary(lm(Time.to.MD..Min.~.+Triage.Acute.Score*Arrival.Model, data=Case3Data)) # most are significant
summary(lm(Time.to.MD..Min.~.+Chief.Complain.System*Arrival.Model, data=Case3Data)) # non significant

summary(lm(Time.to.MD..Min.~.+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=Case3Data))
summary(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=Case3Data)) 
summary(lm(Time.to.MD..Min.~TOD+Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=Case3Data)) 
summary(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+Triage.Acute.Score*Arrival.Model, data=Case3Data)) 








#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#            4.  BUILDING DIFFERENT MODELS
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------

set.seed(1)
sample=sample.split(Case3Data$Time.to.MD..Min., SplitRatio=0.9)
train=subset(Case3Data, sample==TRUE)
test=subset(Case3Data, sample==FALSE)



# model 1 # Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek
summary((lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek, data=train)))
model=(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.382155


# model 2 # Arrival.Model+waitingcensus+Triage.Acute.Score
summary((lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score, data=train)))
model=(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.404229


# model 3 # Arrival.Model+waitingcensus
summary((lm(Time.to.MD..Min.~Arrival.Model+waitingcensus, data=train)))
model=(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.499513

# model 4 # all variables
summary((lm(Time.to.MD..Min.~., data=train)))
model=(lm(Time.to.MD..Min.~., data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.320321


# model 5 # all variables + interaction between tod and day of the week
summary((lm(Time.to.MD..Min.~.+TOD*dayoftheweek, data=train)))
model=(lm(Time.to.MD..Min.~.+TOD*dayoftheweek, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.288646


# model 6 # all variables + interaction between tas and arrival
summary((lm(Time.to.MD..Min.~.+Triage.Acute.Score*Arrival.Model, data=train)))
model=(lm(Time.to.MD..Min.~.+Triage.Acute.Score*Arrival.Model, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.319082

# model 7 # all variables + both sets of interactions
summary((lm(Time.to.MD..Min.~.+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=train)))
model=(lm(Time.to.MD..Min.~.+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.28714

# model 8 # selected variables + interaction between tod and day of the week
summary((lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+TOD+TOD*dayoftheweek, data=train)))
model=(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+TOD+TOD*dayoftheweek, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.301211


# model 9 # selected variables + interaction between tas and arrival
summary((lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+Triage.Acute.Score*Arrival.Model, data=train)))
model=(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+Triage.Acute.Score*Arrival.Model, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.380973

# model 10 # selected variables + both sets of interactions
summary((lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+TOD+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=train)))
model=(lm(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+TOD+Triage.Acute.Score*Arrival.Model+TOD*dayoftheweek, data=train))
predictedvalues=predict(model,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.299653


# model 11
# bootstrapping with glmnet and all variables
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~.,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) 
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.320096


# model 12
# bootstrapping with glmnet and selected variables
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error #RMSE = 7.384794


# model 13
# bootstrapping with glmnet and selected variables
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~Arrival.Model+waitingcensus+Triage.Acute.Score,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE 7.407385

# model 14
# bootstrapping with glmnet and selected variables
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~Arrival.Model+waitingcensus,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.500395


# model 15
# bootstrapping with glmnet and all variables and interaction
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~.+TOD*dayoftheweek,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.286918

# model 16
# bootstrapping with glmnet and all variables and interaction
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~.+Triage.Acute.Score*Arrival.Model,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.32069

# model 17
# bootstrapping with glmnet and all variables and all interactions
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~.+TOD*dayoftheweek+Triage.Acute.Score*Arrival.Model,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.2856


# model 18
# bootstrapping with glmnet and all variables and interaction
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+TOD+TOD*dayoftheweek,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.299355

# model 19
# bootstrapping with glmnet and all variables and interaction
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+Triage.Acute.Score*Arrival.Model,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.383385

# model 20
# bootstrapping with glmnet and all variables and all interactions
glmgrid=expand.grid(alpha=c(0.2,0.3,0.4,0.5,0.6,0.7, 0.8),lambda=c(0.0005,0.001,0.002,0.005, 0.01))  #specify main parameters to tune 
glmnet_reg=train(as.numeric(Time.to.MD..Min.)~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek+TOD+TOD*dayoftheweek+Triage.Acute.Score*Arrival.Model,data=train, method='glmnet',metric='RMSE',tuneGrid=glmgrid) #specify the parameters to tune using tuneGrid=""
glmnet_reg
predictedvalues=predict(glmnet_reg,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 7.298005

# model 21
# cross validation with RF
Case2Ctr=trainControl(method="cv", number=5) 
# rf with arrival model, waiting census, triage acute score
rf1=train(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score, data=train, method="rf", metric="RMSE", trControl=Case2Ctr, tuneLength=3) 
rf1
predictedvalues=predict(rf1,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 6.655405

# model 22
# cross validation with RF
Case2Ctr=trainControl(method="cv", number=3) 
# rf with arrival model, waiting census, triage acute score, day of the week
rf2=train(Time.to.MD..Min.~Arrival.Model+waitingcensus+Triage.Acute.Score+dayoftheweek, data=train, method="rf", metric="RMSE", trControl=Case2Ctr, tuneLength=3) 
predictedvalues=predict(rf2,test) 

test_error<-sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2)) 

test_error # RMSE = 6.652404


# model 23 - gradient boosted tree with arrival model, waiting census, triage acute score, day of the week, time of day, and season
library(gbm)
gbt = gbm(Time.to.MD..Min.~
                as.factor(Arrival.Model)+
                waitingcensus+ 
                Triage.Acute.Score+
                dayoftheweek+
                TOD+
                Season,
              data = train,
              distribution = 'gaussian',
              n.trees = 1100, 
              interaction.depth = 8) 

predictedvalues <- predict(gbt,test)

test_error <- sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2))

test_error # RMSE = 6.235068 ***lowest rmse from all models, to select this





#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#       5 - CHOSEN MODEL #23 Gradient Boosted Tree
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------

set.seed(1)
sample=sample.split(Case3Data$Time.to.MD..Min., SplitRatio=0.9)
train=subset(Case3Data, sample==TRUE)
test=subset(Case3Data, sample==FALSE)

library(gbm)
gbt = gbm(Time.to.MD..Min.~
            as.factor(Arrival.Model)+
            waitingcensus+ 
            Triage.Acute.Score+
            dayoftheweek+
            TOD+
            Season,
          data = train,
          distribution = 'gaussian',
          n.trees = 1100, 
          interaction.depth = 8) 

predictedvalues <- predict(gbt,test)

test_error <- sqrt(mean((predictedvalues-test$Time.to.MD..Min.)^2))

test_error # RMSE = 6.228194

rm("Case3Data","missingpattern","predictfull","predictvars","test","train","zero","i","predictedvalues","sample", "countnumber")




#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#                 6.   APPLY TEST DATA
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------

# import the test data
Case3Test <- read_excel("C:\\Users\\crist\\Desktop\\MCGILL\\3 - WINTER I\\INSY 672 HEALTHCARE ANALYTICS\\group assignment\\Case3Test.xlsm", col_types = c("date","numeric", "text", "text", "text", "text", "numeric","text","numeric"))

# re-import Case3Data --> this will be needed to calculate the waiting census for the test dataset
Case3Copy <- read_excel("C:\\Users\\crist\\Desktop\\MCGILL\\3 - WINTER I\\INSY 672 HEALTHCARE ANALYTICS\\group assignment\\Case3Data.xlsm", col_types = c("date","numeric", "text", "text", "text", "text", "numeric","text","numeric"))


# create a new column that identifies which records belong to the Case3Data and Case3Test
Case3Copy$traintest<-"train"
Case3Test$traintest<-"test"

# combine records for Case3Data and Case3Test together and order them by the start date 
Case3combined=rbind(Case3Copy,Case3Test)
Case3combined <- Case3combined[order(Case3combined$Startdatetime),]

# function to create waiting census
countnumber = function(time,timestamps1,timestamps2){
  indices = which(time > timestamps1 & time < timestamps2)
  return(length(indices))
} 

Case3combined$SeeMDTime=as.POSIXct(Case3combined$Startdatetime+Case3combined$Time.to.MD..Min.*60) #compute Waiting times (min) 

# calculating the waiting census
for(i in 1:nrow(Case3combined)){
  Case3combined$waitingcensus[i] <- countnumber(Case3combined$Startdatetime[i],Case3combined$Startdatetime,Case3combined$SeeMDTime) #compute waiting census when a patient A arrived at the ED. To do that, we count the number of patients whose arrival time is earlier than Patient A's arrival time and whose seeMD time is later than patient A's arrival time.
}

# filter to only have the Case3Test data
Case3Test<-Case3combined[Case3combined$traintest=="test",]



### Apply data cleaning

Case3Test[Case3Test$Age<0,]$Age=NA

# drop discharged home and start to disposition, as these are only assumed after the response variable
Case3Test<-select(Case3Test, -c("Discharged Home","Start.to.Disposition..Min."))
Case3Test$Arrival.Model<-ifelse(Case3Test$Arrival.Model=="Ground Ambulance","Ground Ambulance","Other")

#### Creating new features
# creating time related variables
Case3Test$Hour = as.numeric(stri_sub(Case3Test$Startdatetime,12,13))
Case3Test$Month = as.numeric(stri_sub(Case3Test$Startdatetime,6,7))
Case3Test$Date = ymd(stri_sub(Case3Test$Startdatetime,1,10))

# creating time of day with the hours into 3 hour bins
Case3Test$TOD = ifelse(
  Case3Test$Hour >= 0 & Case3Test$Hour < 3, "12AM-3AM",
  ifelse(
    Case3Test$Hour >= 3 & Case3Test$Hour < 6, "3AM-6AM",
    ifelse(
      Case3Test$Hour >= 6 & Case3Test$Hour <9, "6AM-9AM",
      ifelse(
        Case3Test$Hour >= 9 & Case3Test$Hour <12, "9AM-12PM",
        ifelse(
          Case3Test$Hour >= 12 & Case3Test$Hour < 15, "12PM-3PM",
          ifelse(
            Case3Test$Hour >= 15 & Case3Test$Hour <18, "3PM-6PM",
            ifelse(
              Case3Test$Hour >=18 & Case3Test$Hour < 21, "6PM-9PM", "9PM-12AM"
            )
          )
        )
      )
    )
  )
)

# creating seasons
Case3Test$Season = ifelse(Case3Test$Month>=3 & Case3Test$Month<6,"Spring",ifelse(Case3Test$Month>=6 & Case3Test$Month<9,"Summer",ifelse(Case3Test$Month>=9 & Case3Test$Month<12,"Fall","Winter")))

# creating day of the week
Case3Test$dayoftheweek = wday(Case3Test$Date, label=TRUE, abbr=FALSE)
Case3Test$dayoftheweek= as.factor(ifelse(Case3Test$dayoftheweek=="Monday","Monday",ifelse(Case3Test$dayoftheweek=="Tuesday","Tuesday",ifelse(Case3Test$dayoftheweek=="Wednesday","Wednesday",ifelse(Case3Test$dayoftheweek=="Thursday","Thursday",ifelse(Case3Test$dayoftheweek=="Friday","Friday",ifelse(Case3Test$dayoftheweek=="Saturday","Saturday","Sunday")))))))




# dropping remaining columns that won't be helpful
Case3Test<-select(Case3Test, -c("Startdatetime","traintest","Hour","Month","Date","SeeMDTime"))


# applying complete() to the NA values we created
predictvars=Case3Test[,-c(6)]  
predictfull=complete(mice(predictvars))


# adding the response back in the case3data
Case3Test<-cbind(predictfull,Case3Test$Time.to.MD..Min.)
names(Case3Test)[10]<-"Time.to.MD..Min."


# applying formats to the variables
Case3Test = Case3Test %>% 
  mutate_at(vars(Age,Time.to.MD..Min.,waitingcensus), as.numeric) %>% 
  mutate_at(vars(Arrival.Model,Chief.Complain.System,Gender,TOD,Season,dayoftheweek,Triage.Acute.Score),
            as.factor)


#####################
# generating the RMSE with the chosen model

predictedvalues=predict(gbt,Case3Test)# using chosen model

test_error<-sqrt(mean((predictedvalues-Case3Test$Time.to.MD..Min.)^2)) 

test_error #display the RMSE. 
