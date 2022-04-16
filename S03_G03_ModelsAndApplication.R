# ==============================================================================================================
#Required Libraries
#install.packages("caret")
#install.packages("rstudioapi")
library(rstudioapi)
library(remotes)
library(data.table)
library(randomForest)
library(neuralnet)
library(dplyr)
library(scales)
library(mltools)
library(caTools)
library(car)
library(caret)
remotes::install_version("DMwR", version="0.4.1") #Run this For the First time Keep this here as it has to be behind remotes
library(DMwR)
library(boot)
library(openxlsx)

# ==============================================================================================================
#Set Working Directory to Source File Location Automatically
setwd(dirname(getActiveDocumentContext()$path))

#Importing datasets
fraud <- fread("fraud_clean.csv", stringsAsFactors = TRUE)
View(fraud)


#Convert categorical variables to factor type
fraud$WeekOfMonth <- factor(fraud$WeekOfMonth)
fraud$WeekOfMonthClaimed <- factor(fraud$WeekOfMonthClaimed)
fraud$FraudFound_P <- factor(fraud$FraudFound_P)
fraud$DriverRating <- factor(fraud$DriverRating, levels = c("1", "2", "3", "4"))

levels(fraud$WeekOfMonth)
levels(fraud$WeekOfMonthClaimed)
levels(fraud$FraudFound_P) # 0 for not found and 1 for found
levels(fraud$DriverRating) # 1 to 4


#Train-Test Split ==========================
set.seed(2022)
train = sample.split(Y = fraud$FraudFound_P, SplitRatio=0.7)
trainset = subset(fraud,train == T)
testset = subset(fraud, train == F)


#Check Data Skew ==========================
table(trainset$FraudFound_P)
prop.table(table(trainset$FraudFound_P))
# Highly Skewed


#=====================================================

# Data Balancing ==========================

#Different Imbalance Data Handling Methods to Ensure Highest Accuracy
#Because there is a trade-off between losing data with undersampling, and
#over emphasising meaningless data with oversampling


#Method 1: Manual Undersampling Data
majority <- trainset[FraudFound_P == 0]
minority <- trainset[FraudFound_P == 1]
set.seed(2022)
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))
majority.chosen <- majority[chosen]
trainset.bal <- rbind(majority.chosen, minority)
prop.table(table(trainset.bal$FraudFound_P))


#Method 2: SMOTE Mix of Oversampling & Minor Undersampling without K-Nearest Neighbour Algorithm
set.seed(2022)
trainset.bal2 <- SMOTE(FraudFound_P ~ ., trainset, perc.over=1490, perc.under=110)
prop.table(table(trainset.bal2$FraudFound_P))

#Method 3: SMOTE Mix of Oversampling & Minor Undersampling with K-Nearest Neighbour Algorithm to determine Oversample data
set.seed(2022)
trainset.bal3 <- SMOTE(FraudFound_P ~ ., trainset,k=103, perc.over=1490, perc.under=110)
table(trainset.bal3$FraudFound_P)
prop.table(table(trainset.bal3$FraudFound_P))

threshold = 0.5 #For prediction

# ============================================================================================================
# ============================================================================================================
# 
# Logistic Regression for Vehicle Fraud Identification
#
# ============================================================================================================
# ============================================================================================================

#================================================================================================
# LOGISTIC REGRESSION USING MANUALLY UNDERSAMPLED TRAINSET
#===================================================================================================



# Model 0: Full Model ==========================
log1.full = glm(FraudFound_P~., family=binomial, data = trainset.bal)
summary(log1.full)

# Model 0 Trainset Prediction
trainprob1.full = predict(log1.full,type = 'response')
y.hat1.full = ifelse(trainprob1.full>threshold, 1, 0)
confmat1.full = confusionMatrix(data = factor(y.hat1.full), trainset.bal$FraudFound_P,positive ="1")
acc1.full = confmat1.full$overall[1]
sens1.full = confmat1.full$byClass[1]
vif(log1.full)

#Model 0 Testset Prediction
testprob1.full = predict(log1.full, newdata = testset,type="response")
#Error New Levels


#Model 1: Backward Elimination ==========================
log1.m1 = step(log1.full)
summary(log1.m1)

#Model 1 Trainset Prediction
trainprob1.m1 = predict(log1.m1,type = 'response')
y.hat1.m1 = ifelse(trainprob1.m1>threshold, 1, 0)
confmat1.m1 = confusionMatrix(data = factor(y.hat1.m1), trainset.bal$FraudFound_P,positive ="1")
acc1.m1 = confmat1.m1$overall[1]
sens1.m1 = confmat1.m1$byClass[1]
vif(log1.m1)


#Model 1 Testset Prediction
testprob1.m1 = predict(log1.m1, newdata = testset,type="response")
y.hat1.m1.test = ifelse(testprob1.m1>threshold, 1, 0)
confmat1.m1.test = confusionMatrix(data = factor(y.hat1.m1.test), testset$FraudFound_P,positive ="1")
acc1.m1.test = confmat1.m1.test$overall[1]
sens1.m1.test = confmat1.m1.test$byClass[1]
spec1.m1.test = confmat1.m1.test$byClass[2]

#Model 2: Remove Additional Insignificant Variables For Testing ==========================
log1.m2 = glm(FraudFound_P~ Fault +
                VehicleCategory +
                Deductible +
                AgeOfVehicle +
                AddressChange_Claim +
                BasePolicy
              , family=binomial, data = trainset.bal)
summary(log1.m2)

#Model 2 Trainset Prediction
trainprob1.m2 = predict(log1.m2,type = 'response')
y.hat1.m2 = ifelse(trainprob1.m2>threshold, 1, 0)
confmat1.m2 = confusionMatrix(data = factor(y.hat1.m2), trainset.bal$FraudFound_P,positive ="1")
acc1.m2 = confmat1.m2$overall[1]
sens1.m2 = confmat1.m2$byClass[1]
vif(log1.m2)


#Model 2 Testset Prediction
testprob1.m2 = predict(log1.m2, newdata = testset,type="response")
y.hat1.m2.test = ifelse(testprob1.m2>threshold, 1, 0)
confmat1.m2.test = confusionMatrix(data = factor(y.hat1.m2.test), testset$FraudFound_P,positive ="1")
acc1.m2.test = confmat1.m2.test$overall[1]
sens1.m2.test = confmat1.m2.test$byClass[1]
spec1.m2.test = confmat1.m2.test$byClass[2]

#Model 3: Remove Multicollinear Variable Base Policy ==========================
log1.m3 = glm(FraudFound_P~ Fault +
                VehicleCategory +
                Deductible +
                AgeOfVehicle +
                AddressChange_Claim 
              , family=binomial, data = trainset.bal)
summary(log1.m3)

#Model 3 Trainset Prediction
trainprob1.m3 = predict(log1.m3,type = 'response')
y.hat1.m3 = ifelse(trainprob1.m3>threshold, 1, 0)
confmat1.m3 = confusionMatrix(data = factor(y.hat1.m3), trainset.bal$FraudFound_P,positive ="1")
acc1.m3 = confmat1.m3$overall[1]
sens1.m3 = confmat1.m3$byClass[1]
vif(log1.m3)


#Model 3 Testset Prediction
testprob1.m3 = predict(log1.m3, newdata = testset,type="response")
y.hat1.m3.test = ifelse(testprob1.m3>threshold, 1, 0)
confmat1.m3.test = confusionMatrix(data = factor(y.hat1.m3.test), testset$FraudFound_P,positive ="1")
acc1.m3.test = confmat1.m3.test$overall[1]
sens1.m3.test = confmat1.m3.test$byClass[1]
spec1.m3.test = confmat1.m3.test$byClass[2]


#================================================================================================
# LOGISTIC REGRESSION SMOTE MIX without K-nearest Algorithm
#===================================================================================================


# Model 0: Full Model ==========================
log2.full = glm(FraudFound_P~., family=binomial, data = trainset.bal2)
summary(log2.full)

#Model 0 Trainset Prediction
trainprob2.full = predict(log2.full,type = 'response')
y.hat2.full = ifelse(trainprob2.full>threshold, 1, 0)
confmat2.full = confusionMatrix(data = factor(y.hat2.full), trainset.bal2$FraudFound_P,positive ="1")
acc2.full = confmat2.full$overall[1]
sens2.full = confmat2.full$byClass[1]
vif(log2.full)

#Model 0 Testset Prediction
testprob2.full = predict(log2.full, newdata = testset,type="response")
y.hat2.full.test = ifelse(testprob2.full>threshold, 1, 0)
confmat2.full.test = confusionMatrix(data = factor(y.hat2.full.test), testset$FraudFound_P,positive ="1")
acc2.full.test = confmat2.full.test$overall[1]
sens2.full.test = confmat2.full.test$byClass[1]
spec2.full.test = confmat2.full.test$byClass[2]


#Model 1: Backwards Elimination Method ==========================
log2.m1 = step(log2.full)
summary(log2.m1)

#Model 1 Trainset Prediction
trainprob2.m1 = predict(log2.m1,type = 'response')
y.hat2.m1 = ifelse(trainprob2.m1>threshold, 1, 0)
confmat2.m1 = confusionMatrix(data = factor(y.hat2.m1), trainset.bal2$FraudFound_P,positive ="1")
acc2.m1 = confmat2.m1$overall[1]
sens2.m1 = confmat2.m1$byClass[1]
vif(log2.m1)

#Model 1 Testset Prediction
testprob2.m1 = predict(log2.m1, newdata = testset,type="response")
y.hat2.m1.test = ifelse(testprob2.m1>threshold, 1, 0)
confmat2.m1.test = confusionMatrix(data = factor(y.hat2.m1.test), testset$FraudFound_P,positive ="1")
acc2.m1.test = confmat2.m1.test$overall[1]
sens2.m1.test = confmat2.m1.test$byClass[1]
spec2.m1.test = confmat2.m1.test$byClass[2]

# Model 2: Removed Additional Insignificant Variables ==========================
log2.m2 = glm(FraudFound_P~
                Month +
                WeekOfMonth +
                DayOfWeek +
                Make +
                AccidentArea +
                WeekOfMonthClaimed +
                Sex +
                MaritalStatus +
                Age +
                Fault +
                VehicleCategory +
                VehiclePrice +
                RepNumber +
                Deductible +
                DriverRating +
                Days_Policy_Accident +
                Days_Policy_Claim +
                PastNumberOfClaims +
                AgeOfVehicle +
                AgeOfPolicyHolder +
                PoliceReportFiled +
                WitnessPresent +
                AgentType +
                NumberOfSuppliments +
                AddressChange_Claim +
                NumberOfCars +
                Year +
                BasePolicy, 
                family=binomial, data = trainset.bal2)
summary(log2.m2)

# Model 2 Trainset Prediction
trainprob2.m2 = predict(log2.m2,type = 'response')
y.hat2.m2 = ifelse(trainprob2.m2>threshold, 1, 0)
confmat2.m2 = confusionMatrix(data = factor(y.hat2.m2), trainset.bal2$FraudFound_P,positive ="1")
acc2.m2 = confmat2.m2$overall[1]
sens2.m2 = confmat2.m2$byClass[1]
vif(log2.m2)

# Model 2 Testset Prediction
testprob2.m2 = predict(log2.m2, newdata = testset,type="response")
y.hat2.m2.test = ifelse(testprob2.m2>threshold, 1, 0)
confmat2.m2.test = confusionMatrix(data = factor(y.hat2.m2.test), testset$FraudFound_P,positive ="1")
acc2.m2.test = confmat2.m2.test$overall[1]
sens2.m2.test = confmat2.m2.test$byClass[1]
spec2.m2.test = confmat2.m2.test$byClass[2]

# Model 3: Backwards Elimination Method remove age multicollinear ==========================
log2.m3 = glm(FraudFound_P~
                Month +
                WeekOfMonth +
                DayOfWeek +
                Make +
                AccidentArea +
                WeekOfMonthClaimed +
                Sex +
                MaritalStatus +
                Fault +
                VehicleCategory +
                VehiclePrice +
                RepNumber +
                Deductible +
                DriverRating +
                Days_Policy_Accident +
                Days_Policy_Claim +
                PastNumberOfClaims +
                AgeOfVehicle +
                AgeOfPolicyHolder +
                PoliceReportFiled +
                WitnessPresent +
                AgentType +
                NumberOfSuppliments +
                AddressChange_Claim +
                NumberOfCars +
                Year +
                BasePolicy, 
              family=binomial, data = trainset.bal2)
summary(log2.m3)

# Model 3 Trainset Prediction
trainprob2.m3 = predict(log2.m3,type = 'response')
y.hat2.m3 = ifelse(trainprob2.m3>threshold, 1, 0)
confmat2.m3 = confusionMatrix(data = factor(y.hat2.m3), trainset.bal2$FraudFound_P,positive ="1")
acc2.m3 = confmat2.m3$overall[1]
sens2.m3 = confmat2.m3$byClass[1]
vif(log2.m3)

# Model 3 Testset Prediction
testprob2.m3 = predict(log2.m3, newdata = testset,type="response")
y.hat2.m3.test = ifelse(testprob2.m3>threshold, 1, 0)
confmat2.m3.test = confusionMatrix(data = factor(y.hat2.m3.test), testset$FraudFound_P,positive ="1")
acc2.m3.test = confmat2.m3.test$overall[1]
sens2.m3.test = confmat2.m3.test$byClass[1]
spec2.m3.test = confmat2.m3.test$byClass[2]

# Model 4: Backwards Elimination Method remove Vehicle Category multicollinear ==========================
log2.m4 = glm(FraudFound_P~
                Month +
                WeekOfMonth +
                DayOfWeek +
                Make +
                AccidentArea +
                WeekOfMonthClaimed +
                Sex +
                MaritalStatus +
                Fault +
                VehiclePrice +
                RepNumber +
                Deductible +
                DriverRating +
                Days_Policy_Accident +
                Days_Policy_Claim +
                PastNumberOfClaims +
                AgeOfVehicle +
                AgeOfPolicyHolder +
                PoliceReportFiled +
                WitnessPresent +
                AgentType +
                NumberOfSuppliments +
                AddressChange_Claim +
                NumberOfCars +
                Year +
                BasePolicy, 
              family=binomial, data = trainset.bal2)
summary(log2.m4)

# Model 4 Trainset Prediction
trainprob2.m4 = predict(log2.m4,type = 'response')
y.hat2.m4 = ifelse(trainprob2.m4>threshold, 1, 0)
confmat2.m4 = confusionMatrix(data = factor(y.hat2.m4), trainset.bal2$FraudFound_P,positive ="1")
acc2.m4 = confmat2.m4$overall[1]
sens2.m4 = confmat2.m4$byClass[1]
vif(log2.m4)

# Model 4 Testset Prediction
testprob2.m4 = predict(log2.m4, newdata = testset,type="response")
y.hat2.m4.test = ifelse(testprob2.m4>threshold, 1, 0)
confmat2.m4.test = confusionMatrix(data = factor(y.hat2.m4.test), testset$FraudFound_P,positive ="1")
acc2.m4.test = confmat2.m4.test$overall[1]
sens2.m4.test = confmat2.m4.test$byClass[1]
spec2.m4.test = confmat2.m4.test$byClass[2]
#Overall testset accuracy higher



#================================================================================================
# LOGISTIC REGRESSION SMOTE MIX with K-nearest Algorithm = 103
#===================================================================================================


# Model 0: Full Model ==========================
log3.full = glm(FraudFound_P~., family=binomial, data = trainset.bal3)
summary(log3.full)


# Model 0 Trainset Prediction
trainprob3.full = predict(log3.full,type = 'response')
y.hat3.full = ifelse(trainprob3.full>threshold, 1, 0)
confmat3.full = confusionMatrix(data = factor(y.hat3.full), trainset.bal3$FraudFound_P,positive ="1")
acc3.full = confmat3.full$overall[1]
sens3.full = confmat3.full$byClass[1]
vif(log3.full)

#Model 0 Testset Prediction
testprob3.full = predict(log3.full, newdata = testset,type="response")
y.hat3.full.test = ifelse(testprob3.full>threshold, 1, 0)
confmat3.full.test = confusionMatrix(data = factor(y.hat3.full.test), testset$FraudFound_P,positive ="1")
acc3.full.test = confmat3.full.test$overall[1]
sens3.full.test = confmat3.full.test$byClass[1]
spec3.full.test = confmat3.full.test$byClass[2]


# Model 1: Backwards Elimination ==========================
log3.m1 = step(log3.full)
summary(log3.m1)

#Model 1 Trainset Prediction
trainprob3.m1 = predict(log3.m1,type = 'response')
y.hat3.m1 = ifelse(trainprob3.m1>threshold, 1, 0)
confmat3.m1 = confusionMatrix(data = factor(y.hat3.m1), trainset.bal3$FraudFound_P,positive ="1")
acc3.m1 = confmat3.m1$overall[1]
sens3.m1 = confmat3.m1$byClass[1]
vif(log3.m1)

#Model 1 Testset Prediction
testprob3.m1 = predict(log3.m1, newdata = testset,type="response")
y.hat3.m1.test = ifelse(testprob3.m1>threshold, 1, 0)
confmat3.m1.test = confusionMatrix(data = factor(y.hat3.m1.test), testset$FraudFound_P,positive ="1")
acc3.m1.test = confmat3.m1.test$overall[1]
sens3.m1.test = confmat3.m1.test$byClass[1]
spec3.m1.test = confmat3.m1.test$byClass[2]


# Model 2: Remove Additional Insignificant Var - DayOfWeekClaimed ==========================
log3.m2 = glm(formula = FraudFound_P ~ Month + WeekOfMonth + DayOfWeek + 
                Make + AccidentArea + MonthClaimed + WeekOfMonthClaimed + 
                Sex + MaritalStatus + Age + Fault + VehicleCategory + VehiclePrice + 
                RepNumber + Deductible + DriverRating + Days_Policy_Claim + 
                PastNumberOfClaims + AgeOfVehicle + AgeOfPolicyHolder + WitnessPresent + 
                AgentType + NumberOfSuppliments + AddressChange_Claim + NumberOfCars + 
                Year + BasePolicy, family = binomial, data = trainset.bal3)
summary(log3.m2)

#Model 2 Trainset Prediction
trainprob3.m2 = predict(log3.m2,type = 'response')
y.hat3.m2 = ifelse(trainprob3.m2>threshold, 1, 0)
confmat3.m2 = confusionMatrix(data = factor(y.hat3.m2), trainset.bal3$FraudFound_P,positive ="1")
acc3.m2 = confmat3.m2$overall[1]
sens3.m2 = confmat3.m2$byClass[1]
vif(log3.m2)

#Model 2 Testset Prediction
testprob3.m2 = predict(log3.m2, newdata = testset,type="response")
y.hat3.m2.test = ifelse(testprob3.m2>threshold, 1, 0)
confmat3.m2.test = confusionMatrix(data = factor(y.hat3.m2.test), testset$FraudFound_P,positive ="1")
acc3.m2.test = confmat3.m2.test$overall[1]
sens3.m2.test = confmat3.m2.test$byClass[1]
spec3.m2.test = confmat3.m2.test$byClass[2]

# Model 3: Remove Additional Insignificant Variable - MonthClaimed ==========================
log3.m3 = glm(formula = FraudFound_P ~ Month + WeekOfMonth + DayOfWeek + 
                Make + AccidentArea +  WeekOfMonthClaimed + 
                Sex + MaritalStatus + Age + Fault + VehicleCategory + VehiclePrice + 
                RepNumber + Deductible + DriverRating + Days_Policy_Claim + 
                PastNumberOfClaims + AgeOfVehicle + AgeOfPolicyHolder + WitnessPresent + 
                AgentType + NumberOfSuppliments + AddressChange_Claim + NumberOfCars + 
                Year + BasePolicy, family = binomial, data = trainset.bal3)
summary(log3.m3)

#Model 3 Trainset Prediction
trainprob3.m3 = predict(log3.m3,type = 'response')
y.hat3.m3 = ifelse(trainprob3.m3>threshold, 1, 0)
confmat3.m3 = confusionMatrix(data = factor(y.hat3.m3), trainset.bal3$FraudFound_P,positive ="1")
acc3.m3 = confmat3.m3$overall[1]
sens3.m3 = confmat3.m3$byClass[1]
vif(log3.m3)

#Model 3 Testset Prediction
testprob3.m3 = predict(log3.m3, newdata = testset,type="response")
y.hat3.m3.test = ifelse(testprob3.m3>threshold, 1, 0)
confmat3.m3.test = confusionMatrix(data = factor(y.hat3.m3.test), testset$FraudFound_P,positive ="1")
acc3.m3.test = confmat3.m3.test$overall[1]
sens3.m3.test = confmat3.m3.test$byClass[1]
spec3.m3.test = confmat3.m3.test$byClass[2]


# Model 4: Further Remove Insignificant Variable - PastNumberOfClaims ==========================
log3.m4 = glm(formula = FraudFound_P ~ Month + WeekOfMonth + DayOfWeek + 
                Make + AccidentArea +  WeekOfMonthClaimed + 
                Sex + MaritalStatus + Age + Fault + VehicleCategory + VehiclePrice + 
                RepNumber + Deductible + DriverRating + Days_Policy_Claim + 
                AgeOfVehicle + AgeOfPolicyHolder + WitnessPresent + 
                AgentType + NumberOfSuppliments + AddressChange_Claim + NumberOfCars + 
                Year + BasePolicy, family = binomial, data = trainset.bal3)
summary(log3.m4)

#Model 4 Trainset Prediction
trainprob3.m4 = predict(log3.m4,type = 'response')
y.hat3.m4 = ifelse(trainprob3.m4>threshold, 1, 0)
confmat3.m4 = confusionMatrix(data = factor(y.hat3.m4), trainset.bal3$FraudFound_P,positive ="1")
acc3.m4 = confmat3.m4$overall[1]
sens3.m4 = confmat3.m4$byClass[1]
vif(log3.m4)

#Model 4 Testset Prediction
testprob3.m4= predict(log3.m4, newdata = testset,type="response")
y.hat3.m4.test = ifelse(testprob3.m4>threshold, 1, 0)
confmat3.m4.test = confusionMatrix(data = factor(y.hat3.m4.test), testset$FraudFound_P,positive ="1")
acc3.m4.test = confmat3.m4.test$overall[1]
sens3.m4.test = confmat3.m4.test$byClass[1]
spec3.m4.test = confmat3.m4.test$byClass[2]

# Model 5: Remove Multicollinear - Age==========================
log3.m5 = glm(formula = FraudFound_P ~ Month + WeekOfMonth + DayOfWeek + 
                Make + AccidentArea +  WeekOfMonthClaimed + 
                Sex + MaritalStatus + Fault + VehicleCategory + VehiclePrice + 
                RepNumber + Deductible + DriverRating + Days_Policy_Claim + 
                AgeOfVehicle + AgeOfPolicyHolder + WitnessPresent + 
                AgentType + NumberOfSuppliments + AddressChange_Claim + NumberOfCars + 
                Year + BasePolicy, family = binomial, data = trainset.bal3)
summary(log3.m5)

#Model 5 Trainset Prediction
trainprob3.m5 = predict(log3.m5,type = 'response')
y.hat3.m5 = ifelse(trainprob3.m5>threshold, 1, 0)
confmat3.m5 = confusionMatrix(data = factor(y.hat3.m5), trainset.bal3$FraudFound_P,positive ="1")
acc3.m5 = confmat3.m5$overall[1]
sens3.m5 = confmat3.m5$byClass[1]
vif(log3.m5)

#Model 5 Testset Prediction
testprob3.m5= predict(log3.m5, newdata = testset,type="response")
y.hat3.m5.test = ifelse(testprob3.m5>threshold, 1, 0)
confmat3.m5.test = confusionMatrix(data = factor(y.hat3.m5.test), testset$FraudFound_P,positive ="1")
acc3.m5.test = confmat3.m5.test$overall[1]
sens3.m5.test = confmat3.m5.test$byClass[1]
spec3.m5.test = confmat3.m5.test$byClass[2]

# Model 6: Remove Multicollinear - Vehicle Category==========================
log3.m6 = glm(formula = FraudFound_P ~ Month + WeekOfMonth + DayOfWeek + 
                Make + AccidentArea +  WeekOfMonthClaimed + 
                Sex + MaritalStatus + Fault + VehiclePrice + 
                RepNumber + Deductible + DriverRating + Days_Policy_Claim + 
                AgeOfVehicle + AgeOfPolicyHolder + WitnessPresent + 
                AgentType + NumberOfSuppliments + AddressChange_Claim + NumberOfCars + 
                Year + BasePolicy, family = binomial, data = trainset.bal3)
summary(log3.m6)

#Model 6 Trainset Prediction
trainprob3.m6 = predict(log3.m6,type = 'response')
y.hat3.m6 = ifelse(trainprob3.m6>threshold, 1, 0)
confmat3.m6 = confusionMatrix(data = factor(y.hat3.m6), trainset.bal3$FraudFound_P,positive ="1")
acc3.m6 = confmat3.m6$overall[1]
sens3.m6 = confmat3.m6$byClass[1]
vif(log3.m6)

#Model 6 Testset Prediction
testprob3.m6= predict(log3.m6, newdata = testset,type="response")
y.hat3.m6.test = ifelse(testprob3.m6>threshold, 1, 0)
confmat3.m6.test = confusionMatrix(data = factor(y.hat3.m6.test), testset$FraudFound_P,positive ="1")
acc3.m6.test = confmat3.m6.test$overall[1]
sens3.m6.test = confmat3.m6.test$byClass[1]
spec3.m6.test = confmat3.m6.test$byClass[2]


#================================================================================================
# Prediction Accuracy and Sensitivity Testing
#===================================================================================================
#DataFrame to Compare Logistic Regression Models
table1 <- data.frame('Trainset Accuracy' = 1:16,'Trainset Sensitivity' = 1:16, 'Testset Accuracy' = 1:16, 'Testset Sensitivity' = 1:16, 'Testset Specificity'=1:16)

table1$Trainset.Accuracy <- NA
table1$Trainset.Sensitivity <- NA
table1$Testset.Accuracy <- NA
table1$Testset.Sensitivity <- NA
table1$Testset.Specificity <- NA

#Insert Values
table1[1,1]= acc1.full
table1[1,2]= sens1.full
table1[1,3]= 0
table1[1,4]= 0
table1[1,5]= 0

table1[2,1:5]= c(acc1.m1,sens1.m1,acc1.m1.test,sens1.m1.test,spec1.m1.test)

table1[3,1:5]= c(acc1.m2,sens1.m2,acc1.m2.test,sens1.m2.test, spec1.m2.test)

table1[4,1:5]= c(acc1.m3,sens1.m3,acc1.m3.test,sens1.m3.test, spec1.m3.test)


table1[5,1:5]= c(acc2.full,sens2.full,acc2.full.test,sens2.full.test, spec2.full.test)

table1[6,1:5]= c(acc2.m1,sens2.m1,acc2.m1.test,sens2.m1.test, spec2.m1.test)

table1[7,1:5]= c(acc2.m2,sens2.m2,acc2.m2.test,sens2.m2.test, spec2.m2.test)

table1[8,1:5]= c(acc2.m3,sens2.m3,acc2.m3.test,sens2.m3.test, spec2.m3.test)

table1[9,1:5]= c(acc2.m4,sens2.m4,acc2.m4.test,sens2.m4.test, spec2.m4.test)




table1[10,1:5]= c(acc3.full,sens3.full,acc3.full.test,sens3.full.test, spec3.full.test)

table1[11,1:5]= c(acc3.m1,sens3.m1,acc3.m1.test,sens3.m1.test, spec3.m1.test)

table1[12,1:5]= c(acc3.m2,sens3.m2,acc3.m2.test,sens3.m2.test, spec3.m2.test)

table1[13,1:5]= c(acc3.m3,sens3.m3,acc3.m3.test,sens3.m3.test, spec3.m3.test)

table1[14,1:5]= c(acc3.m4,sens3.m4,acc3.m4.test,sens3.m4.test, spec3.m4.test)

table1[15,1:5]= c(acc3.m5,sens3.m5,acc3.m5.test,sens3.m5.test, spec3.m5.test)

table1[16,1:5]= c(acc3.m6,sens3.m6,acc3.m6.test,sens3.m6.test, spec3.m6.test)

rownames(table1) <- c('Undersampling-Full Model',
                      'Undersampling-Backwards Elimination',
                      'Undersampling-Additional Insignificant Variables Removed',
                      'Undersampling-Multicollinear Removed',
                      'SMOTE-Full Model',
                      'SMOTE-Backwards Elimination',
                      'SMOTE-Add. Insig. Var. Removed',
                      'SMOTE-Remove Multicollinear Variable - Age',
                      'SMOTE-Remove Multicollinear Variable - VehicleCategory',
                      'SMOTE with K-Full Model',
                      'SMOTE with K-Backwards Elimination',
                      'SMOTE with K-Add. Insig. Removed - DaysOfWeekClaimed',
                      'SMOTE with K-Add. Insig. Removed - MonthClaimed',
                      'SMOTE with K-Add. Insig. Removed - PastNoOfClaims',
                      'SMOTE with K-Remove Multicollinear - Age',
                      'SMOTE with K-Remove Multicollinear - VehicleCategory')


# ============================================================================================================
# ============================================================================================================
# 
# Random Forest for Vehicle Fraud Identification
#
# ============================================================================================================
# ============================================================================================================



#### Random Forest on Undersampled Trainset ####
set.seed(2022)
m1.RF <- randomForest(FraudFound_P ~ . , data=trainset.bal, importance=T)

m1.RF

m1.RF$confusion

#Accuracy
m1.train.acc = 1-m1.RF$err.rate[500]
m1.train.acc


#Sensitivity 
m1.train.sens = 1-m1.RF$confusion[2,3]
m1.train.sens

#RF Error Plot
plot(m1.RF)
## Confirms error stablised before 500 trees.

m1.RF.yhat <- predict(m1.RF, newdata = testset)

#create confuson matrix for test set 
m1.test.confusion = confusionMatrix(data=factor(m1.RF.yhat), testset$FraudFound_P, positive ="1")

#testset accuracy 
m1.test.acc = m1.test.confusion$overall[1]
m1.test.acc

#testset sensitivity 
m1.test.sens = m1.test.confusion$byClass[1]
m1.test.acc

#testset specificity
m1.test.spec = m1.test.confusion$byClass[2]
m1.test.acc


var.impt.m1.RF <- importance(m1.RF, type = 1)
varImpPlot(m1.RF, type = 1)


#### Random Forest on Model 2 ####
set.seed(2022)
m2.RF <- randomForest(FraudFound_P ~ . , data=trainset.bal2, importance=T)

m2.RF

m2.RF$confusion

#accuracy 
m2.train.acc = 1-m2.RF$err.rate[500]
m2.train.acc

s#sensitivity by taking (TP/P)
m2.train.sens = 1-m2.RF$confusion[2,3]
m2.train.sens

plot(m2.RF)
## Confirms error stablised before 500 trees.

#Testset Prediction
m2.RF.yhat <- predict(m2.RF, newdata = testset)


#create confuson matrix for test set 
m2.test.confusion = confusionMatrix(data=factor(m2.RF.yhat),testset$FraudFound_P,positive="1")

#testset accuracy 
m2.test.acc = m2.test.confusion$overall[1]
m2.test.acc

#testset sensitivity 
m2.test.sens = m2.test.confusion$byClass[1]
m2.test.sens

#testset specificity
m2.test.spec = m2.test.confusion$byClass[2]
m2.test.spec

var.impt.m2.RF <- importance(m2.RF, type = 1)

varImpPlot(m2.RF, type = 1)



#### Random Forest on Model 3 ####
set.seed(2022)
m3.RF <- randomForest(FraudFound_P ~ . , data=trainset.bal3, importance=T)
m3.RF

var.impt.m3.RF <- importance(m3.RF, type = 1)
varImpPlot(m3.RF, type = 1)



#accuracy by taking (TP+TN/P+N)

m3.train.acc = 1-m3.RF$err.rate[500]
m3.train.acc

#sensitivity by taking (TP/P)
m3.train.sens = 1-m3.RF$confusion[2,3]
m3.train.sens

#testset specificity
m3.test.spec = m3.test.confusion$byClass[2]
m3.test.spec

plot(m3.RF)
## Confirms error stablised before 500 trees.


m3.RF.yhat <- predict(m3.RF, newdata = testset)

#create confuson matrix for test set 
m3.test.confusion = confusionMatrix(data = factor(m3.RF.yhat),testset$FraudFound_P, positive = "1")


#testset accuracy 
m3.test.acc = m3.test.confusion$overall[1]
m3.test.acc

#testset sensitivity 
m3.test.sens = m3.test.confusion$byClass[1]

#testset specificity
m3.test.spec = m3.test.confusion$byClass[2]
m3.test.spec


#### Hybrid Model: Logistic Regression + Random Forest ####

set.seed(2022)
m1.RF.logreg <- randomForest(FraudFound_P ~ Fault + VehicleCategory + Deductible + 
                               AgeOfVehicle + AddressChange_Claim + BasePolicy,
                             data=trainset.bal, importance=T)

var.impt.m1.RF.logreg <- importance(m1.RF.logreg, type = 1)

varImpPlot(m1.RF.logreg, type = 1)

m1.RF.logreg

m1.RF.logreg$confusion

#accuracy by taking (TP+TN/P+N)
m1.logreg.train.acc = 1-m1.RF.logreg$err.rate[500]
m1.logreg.train.acc

#sensitivity by taking (TP/P)
m1.logreg.train.sens = 1-m1.RF.logreg$confusion[2,3]
m1.logreg.train.sens

plot(m1.RF.logreg)
## Confirms error stablised before 500 trees.

set.seed(2022)
m1.RF.logreg.yhat <- predict(m1.RF.logreg, newdata = testset)

#create confuson matrix for test set 
m1.logreg.test.confusion = confusionMatrix(data=factor(m1.RF.logreg.yhat), testset$FraudFound_P, positive = "1")


#testset accuracy 
m1.logreg.test.acc = m1.logreg.test.confusion$overall[1]
m1.logreg.test.acc

#testset sensitivity 
m1.logreg.test.sens = m1.logreg.test.confusion$byClass[1]
m1.logreg.test.sens

#testset specificity
m1.logreg.test.spec = m1.logreg.test.confusion$byClass[2]
m1.logreg.test.spec


#### determining effectiveness ####

#### Displaying Accuracies Across 4 Models ####
Balancing.Method <- c('Undersampling', 'SMOTE', 'SMOTE WITH K-NEAREST ALGORITHM','Undersampling')
Model <- c('M1','M2', 'M3','RF Using Log Reg Variables')
Trainset.Accuracy <- c(m1.train.acc, m2.train.acc, m3.train.acc,m1.logreg.train.acc)
Trainset.Sensitivity <- c(m1.train.sens, m2.train.sens, m3.train.sens,m1.logreg.train.sens)
Testset.Accuracy <- c(m1.test.acc, m2.test.acc, m3.test.acc,m1.logreg.test.acc)
Testset.Sensitivity <- c(m1.test.sens, m2.test.sens, m3.test.sens,m1.logreg.test.sens)
Testset.Specificity <- c(m1.test.spec, m2.test.spec, m3.test.sens,m1.logreg.test.spec)
df <- data.frame(Balancing.Method, Model, Trainset.Accuracy,Trainset.Sensitivity,Testset.Accuracy,Testset.Sensitivity, Testset.Specificity)
View(df)




# ============================================================================================================
# ============================================================================================================
# 
# Neural Network for Vehicle Fraud Identification
#
# ============================================================================================================
# ============================================================================================================

###Select Important Variables From Logistic Regression
nntrainset.bal<-as.data.table(subset(trainset.bal, select = -FraudFound_P))
nntrainset.bal2<-as.data.table(subset(trainset.bal2, select = -FraudFound_P))
nntrainset.bal3<-as.data.table(subset(trainset.bal3, select = -FraudFound_P))
nntestset <- as.data.table(subset(testset, select = -FraudFound_P))

#One_Hot Encoding To Create Dummy Variable For Categorical Variables
nn2trainset.bal = one_hot(nntrainset.bal)
onehotdata = as.data.table(cbind(FraudFound_P = trainset.bal$FraudFound_P,nn2trainset.bal))
nn2trainset.bal2 = one_hot(nntrainset.bal2)
onehotdata2 = data.table(cbind(FraudFound_P = trainset.bal2$FraudFound_P,nn2trainset.bal2))
nn2trainset.bal3 = one_hot(data.table(nntrainset.bal3))
onehotdata3 = data.table(cbind(FraudFound_P = trainset.bal2$FraudFound_P,nn2trainset.bal3))
nntestset = one_hot(nntestset)
onehottest = data.table(cbind(FraudFound_P = testset$FraudFound_P,nntestset))


#normalise using max-min(as variables are no 0-1 yet
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}


#Normalise all rows except columns only has 0 because normalising it will return NA
summary(newdata)
newdata = subset(onehotdata, select = -c(Make_Jaguar,Make_Porche,DayOfWeekClaimed_0,MonthClaimed_0))
newdata[,c(2:105)] <- as.data.frame(lapply(newdata[,c(2:105)], normalize))
newdata <- data.table(cbind(newdata,onehotdata[,.(Make_Jaguar,Make_Porche,DayOfWeekClaimed_0,MonthClaimed_0)]))
newdata[,FraudFound_P:=as.character(FraudFound_P)][,FraudFound_P:=as.numeric(FraudFound_P)]

#Normalise all rows except columns only has 0 because normalising it will return NA
summary(newdata2)
newdata2 = subset(onehotdata2, select = -c(Make_Ferrari)) #remove NA Columns
newdata2[,c(2:108)] <- as.data.frame(lapply(newdata2[,c(2:108)], normalize))
newdata2 <- data.table(cbind(newdata2,onehotdata2[,.(Make_Ferrari)]))
newdata2[,FraudFound_P:=as.character(FraudFound_P)][,FraudFound_P:=as.numeric(FraudFound_P)]

#Normalise all rows except columns only has 0 because normalising it will return NA
summary(newdata3)
newdata3 = subset(onehotdata3, select = -c(Make_Ferrari)) #remove NA Columns
newdata3[,c(2:108)] <- as.data.frame(lapply(newdata3[,c(2:108)], normalize))
newdata3 <- data.table(cbind(newdata3,onehotdata3[,.(Make_Ferrari)]))
newdata3[,FraudFound_P:=as.character(FraudFound_P)][,FraudFound_P:=as.numeric(FraudFound_P)]

#Normalise all rows except columns only has 0 because normalising it will return NA
summary(newtest)
newtest = subset(onehottest, select = -c(Make_Ferrari,Make_Lexus,Make_Mecedes,Make_Porche,DayOfWeekClaimed_0,MonthClaimed_0))
newtest[,c(2:103)] = as.data.frame(lapply(newtest[,c(2:103)], normalize))
newtest <-data.table(cbind(newtest,onehottest[,.(Make_Ferrari,Make_Lexus,Make_Mecedes,Make_Porche,DayOfWeekClaimed_0,MonthClaimed_0)]))
newtest[,FraudFound_P:=as.character(FraudFound_P)][,FraudFound_P:=as.numeric(FraudFound_P)]

View(newdata)
View(newdata2)
View(newdata3)

names(newdata) <- make.names(names(newdata))
names(newdata2) <- make.names(names(newdata2))
names(newdata3) <- make.names(names(newdata3))
names(newtest) <- make.names(names(newtest))

#=============================================================================================================
# Neural Network comprising 2 hidden layer with 2/3 of the input size as hidden nodes for binary categorical target
set.seed(2022)
m1 <- neuralnet(FraudFound_P ~ . , data=newdata, hidden=10, err.fct="ce", linear.output=FALSE)
par(mfrow=c(1,1))
#plot(m1)


m1$net.result  # predicted outputs. 
m1$result.matrix  # summary. Error = 0.017634157
m1$startweights
m1$weights
# The generalized weight is defined as the contribution of the ith input variable to the log-odds:
m1$generalized.weights
## Easier to view GW as plots instead

#Trainset
y.hat.train1 <- ifelse(m1$net.result[[1]] > threshold, 1, 0)
y.hat.train1
nn.m1.trainconf <- confusionMatrix(data=factor(y.hat.train1), reference = as.factor(newdata$FraudFound_P))
nn.m1.trainconf

#Trainset
pr.m1 <- neuralnet::compute(m1,newtest)
yhat.m1 <- ifelse(pr.m1$net.result > threshold,1,0)
nn.m1.conf = confusionMatrix(data=factor(yhat.m1), factor(newtest$FraudFound_P),positive ="1")
nn.m1.conf
nn.m1.conf


#Note: This takes few hours to run which may not be viable for DAI
#SMOTE Without K
# set.seed(2022)
# m2 <- neuralnet(FraudFound_P ~ . , data=newdata2, hidden=10, stepmax=10000000,threshold = 0.5, err.fct="ce", linear.output=FALSE)
# 
# #Trainset
# y.hat.train2 <- ifelse(m2$net.result[[1]] > threshold, 1, 0)
# y.hat.train2
# nn.m2.trainconf <- confusionMatrix(data=factor(y.hat.train2), reference = factor(newdata2$FraudFound_P))
# nn.m2.trainconf
# 
# pr.m2 <- neuralnet::com2pute(m2,newtest)
# yhat.m2 <- ifelse(pr.m2$net.result > threshold,1,0)
# nn.m2.conf = confusionm2atrix(data=factor(yhat.m2), testset$FraudFound_P,positive ="1")
# View(nn.m2.conf)
# nn.m2.conf

#Note: This takes few hours to run which may not be viable for DAI
#SMOTE With K=103 
# set.seed(2022)
# m3 <- neuralnet(FraudFound_P ~ . , data=newdata3,hidden =10, err.fct="ce", linear.output=FALSE)
# 
# 
# y.hat.train3 <- ifelse(m3$net.result[[1]] > threshold, 1, 0)
# y.hat.train3
# nn.m3.trainconf <- confusionMatrix(data=factor(y.hat.train3), reference = as.factor(newdata3$FraudFound_P))
# nn.m3.trainconf
# 
# pr.m3 <- neuralnet::com3pute(m3,newtest)
# yhat.m3 <- ifelse(pr.m3$net.result > threshold,1,0)
# nn.m3.conf = confusionm3atrix(data=factor(yhat.m3), testset$FraudFound_P,positive ="1")
# View(nn.m3.conf)
# nn.m3.conf


#Hybrid Model: Using Log Reg Variables in Neural Network Only Increased Threshold as Neural was unable to converge
set.seed(2022)
m4 <- neuralnet(FraudFound_P ~ Fault_Policy.Holder + 
                  Fault_Third.Party +
                  VehicleCategory_Sedan + 
                  VehicleCategory_Sport +
                  VehicleCategory_Utility +
                  Deductible +
                  AgeOfVehicle +
                  AddressChange_Claim +
                  BasePolicy_All.Perils +
                  BasePolicy_Collision +
                  BasePolicy_Liability, data=newdata,hidden =10,threshold = 0.03, err.fct="ce", linear.output=FALSE)
plot(m4)


#Trainset
y.hat.train4 <- ifelse(m4$net.result[[1]] > threshold, 1, 0)
y.hat.train4
nn.m4.trainconf <- confusionMatrix(data=factor(y.hat.train4), reference = factor(newdata$FraudFound_P), positive = "1")
nn.m4.trainconf

pr.m4 <- neuralnet::compute(m4,newtest)
yhat.m4 <- ifelse(pr.m4$net.result > threshold,1,0)
nn.m4.conf = confusionMatrix(data=factor(yhat.m4), factor(newtest$FraudFound_P),positive ="1")
nn.m4.conf


#print  table to show performance, predictions made by the neural network 
nntable <- data.frame('Method' = c('Undersampling','Undersampling with LogReg Variables'),
                    'Trainset Accuracy' = NA,
                    'Trainset Sensitivity' = NA,
                    'Testset Accuracy' = NA,
                    'Testset Sensitivity' = NA,
                    'Testset Specificity' = NA)

nntable[1,2:6]=c(nn.m1.trainconf$overall[1],nn.m1.trainconf$byClass[1],nn.m1.conf$overall[1],nn.m1.conf$byClass[1],nn.m1.conf$byClass[2])
#nntable[2,3:4]=c(nn.m2.trainconf$overall[1],nn.m2.trainconf$byClass[1],nn.m2.conf$overall[1],nn.m2.conf$byClass[1])
#nntable[3,3:4]=c(nn.m3.trainconf$overall[1],nn.m3.trainconf$byClass[1],nn.m3.conf$overall[1],nn.m3.conf$byClass[1])
nntable[2,2:6]=c(nn.m4.trainconf$overall[1],nn.m4.trainconf$byClass[1],nn.m4.conf$overall[1],nn.m4.conf$byClass[1],nn.m4.conf$byClass[2] )




# ============================================================================================================
# ============================================================================================================
# 
# Extracting Data For Dashboarding 
#
# ============================================================================================================
# ============================================================================================================

#Simulate Current Fraud Detection Rate For Dashboarding based on Statistics on Testset========================


fraudyes = testset[FraudFound_P == 1]
fraudno = testset[FraudFound_P == 0]
fraudfound = sample(seq(1:nrow(fraudyes)), size = round(0.02*nrow(fraudyes),0))
frauddetected <- fraudyes[fraudfound]
fraudnotdetected <- fraudyes[!fraudfound]
fraudnotdetected[, Fraud_Detected := 0]
frauddetected[, Fraud_Detected := 1]
fraudno[,Fraud_Detected := 0]
currtestset = data.table(rbind(frauddetected,fraudnotdetected,fraudno))
exacc = data.frame(cbind(as.character(currtestset$FraudFound_P), currtestset$Fraud_Detected))
colnames(exacc) = c("Actual", "Detected")

currconfmat = confusionMatrix(data = factor(currtestset$Fraud_Detected), currtestset$FraudFound_P, positive ="1")
currtable = data.frame('Est.Accuracy'=currconfmat$overall[1],'Est.Sensitivity'=currconfmat$byClass[1], 'Est.Specificity'=currconfmat$byClass[2],'CostImplication'=((1-currconfmat$byClass[1])*nrow(testset)*2559))

write.xlsx(exacc,"existingpredictions.xlsx")
write.xlsx(currtable,"existingcost.xlsx")



#Logistic Regression==========================================================================================

table2 <- data.frame('Balancing Type'=1:16,'Model'=1:16, 'Testset Accuracy' = 1:16, 'Testset Sensitivity' = 1:16,'Testset Specificity'= 1:16, 'CostOfMissedDetection'=1:16)

table2$Balancing.Type <- NA
table2$Model <- NA
table2$Testset.Accuracy <- NA
table2$Testset.Sensitivity <- NA
table2$Testset.Specificity <- NA
table2$CostOfMissedDetection <- NA

table2[c(1,2,3,4),1] = "Undersampling"
table2[c(5,6,7,8,9),1] = "SMOTE"
table2[c(10,11,12,13,14,15,16),1] ="SMOTE WITH K-NEAREST ALGORITHM"

table2[c(1,2,3,4),2] = c(0,1,2,3)
table2[c(5,6,7,8,9),2] = c(0,1,2,3,4)
table2[c(10,11,12,13,14,15,16),2] =c(0,1,2,3,4,5,6)


table2[1,3:5]= 0

table2[2,3:5]= c(acc1.m1.test,sens1.m1.test,spec1.m1.test)

table2[3,3:5]= c(acc1.m2.test,sens1.m2.test, spec1.m2.test)

table2[4,3:5]= c(acc1.m3.test,sens1.m3.test, spec1.m3.test)


table2[5,3:5]= c(acc2.full.test,sens2.full.test, spec2.full.test)

table2[6,3:5]= c(acc2.m1.test,sens2.m1.test, spec2.m1.test)

table2[7,3:5]= c(acc2.m2.test,sens2.m2.test, spec2.m2.test)

table2[8,3:5]= c(acc2.m3.test,sens2.m3.test, spec2.m3.test)

table2[9,3:5]= c(acc2.m4.test,sens2.m4.test, spec2.m4.test)




table2[10,3:5]= c(acc3.full.test,sens3.full.test, spec3.full.test)

table2[11,3:5]= c(acc3.m1.test,sens3.m1.test, spec3.m1.test)

table2[12,3:5]= c(acc3.m2.test,sens3.m2.test, spec3.m2.test)

table2[13,3:5]= c(acc3.m3.test,sens3.m3.test, spec3.m3.test)

table2[14,3:5]= c(acc3.m4.test,sens3.m4.test, spec3.m4.test)

table2[15,3:5]= c(acc3.m5.test,sens3.m5.test, spec3.m5.test)

table2[16,3:5]= c(acc3.m6.test,sens3.m6.test, spec3.m6.test)

#To Calculate Cost Implication = FNR * Nrows of testset * cost per missed detection
table2$CostOfMissedDetection = (1-table2$Testset.Sensitivity)*nrow(testset)*2559




write.xlsx(table2,"ModelBuilding.xlsx")




#Final Model Coefficients

table3 = data.frame(coef(log1.m2))
varimp = rbind("(Intercept)" = NA,data.frame((varImp(log1.m2)/sum(varImp(log1.m2)))*100))
table3 <- cbind(rownames(table3), data.frame(table3, row.names=NULL), data.frame(varimp,row.names=NULL))
write.xlsx(table3,"coef.xlsx")


#Log Reg Accuracies
logacc = data.table(cbind(as.character(testset$FraudFound_P), y.hat1.m2.test))
colnames(logacc)=c("Actual", "FinalPredictions")
logacc[,FinalAccuracy := ifelse(FinalPredictions==Actual, "Detected","Missed")]
write.xlsx(logacc, "LogRegHitMiss.xlsx")





#Random Forest =========================================================================================================
#Cost Implication Calculation + Export Accuracy Table for RF
df$TestCost <- (1-df$Testset.Sensitivity)*nrow(testset)*2559

write.xlsx(df,"RFModels.xlsx")


#Final Random Forest Important Variables
finrfimpt = cbind(Variables = rownames(var.impt.m1.RF),data.frame(var.impt.m1.RF, row.names = NULL))


write.xlsx(finrfimpt, "RFVariableImportance.xlsx")


#Random Forest Accuracies
rfacc = data.table(cbind(as.character(testset$FraudFound_P), as.character(m1.RF.yhat),as.character(m2.RF.yhat),as.character(m3.RF.yhat), as.character(m1.RF.logreg.yhat)))
colnames(rfacc)=c("Actual", "UndersamplingPredictions","SMOTEPredictions", "SMOTEwKPredictions","HybridPredictions")
rfacc[,UndersamplingAccuracy := ifelse(UndersamplingPredictions==Actual, "Detected","Missed")][,SMOTEAccuracy := ifelse(SMOTEPredictions==Actual, "Detected","Missed")][,SMOTEwKAccuracy := ifelse(SMOTEwKPredictions==Actual, "Detected","Missed")][,HybridAccuracy := ifelse(HybridPredictions==Actual, "Detected","Missed")]
write.xlsx(rfacc, "RFHitMiss.xlsx")


#Neural Network =========================================================================================================

#Cost Implication Calculation
nntable$TestCost <- (1-nntable$Testset.Sensitivity)*nrow(testset)*2559

write.xlsx(nntable, "NNAccuracy.xlsx")


#Neural Network Accuracies

neuralnetpred = data.table(cbind(as.character(newtest[,FraudFound_P]),yhat.m1,yhat.m4))
colnames(neuralnetpred) = c("Actual","Model.1.Predictions", "Model.2.Predictions")
View(neuralnetpred)

neuralnetpred[,Model.1.Accuracy := ifelse(Model.1.Predictions==Actual, "Detected","Missed")][,Model.2.Accuracy := ifelse(Model.2.Predictions==Actual, "Detected","Missed")]
write.xlsx(neuralnetpred, "NNHitMiss.xlsx")


#install.packages("shiny")
library(shiny)
library(shinythemes)


ui <- fluidPage(headerPanel("Fraud Detection System"),
                theme = shinytheme("superhero"),
                mainPanel(
                  selectInput(inputId = "month", 
                              label = strong("Month of Claim"),
                              choices = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                             ),
                  selectInput(inputId = "dayofweek",
                              label = strong("Day Of Week"),
                              choices = c("Monday","Tuesday","Wednesday","Thurday","Friday","Saturday","Sunday")),
                  selectInput(inputId = "make",
                              label = strong("Make of Car"),
                              choices = unique(trainset.bal$Make)),
                  selectInput(inputId = "area",
                              label = strong("Accident Area"),
                              choices = unique(trainset.bal$AccidentArea)),
                  selectInput(inputId = "dayofweekclaimed",
                              label = strong("Day of Week Claimed"),
                              choices =  c("Monday","Tuesday","Wednesday","Thurday","Friday","Saturday","Sunday")),
                  selectInput(inputId = "monthclaimed",
                              label = strong("Month Claimed"),
                              choices = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
                  selectInput(inputId = "rating",
                              label = strong("Driver Rating"),
                              choices = sort(unique(trainset.bal$DriverRating))),
                  selectInput(inputId = "pastclaims",
                              label = strong("Past Number of Claims"),
                              choices = sort(unique(trainset.bal$PastNumberOfClaims))),
                  selectInput(inputId = "police",
                              label = strong("Police Report Filed"),
                              choices = unique(trainset.bal$PoliceReportFiled)),
                  selectInput(inputId = "witness",
                              label = strong("Witness Present"),
                              choices = unique(trainset.bal$WitnessPresent)),
                  selectInput(inputId = "agent",
                              label = strong("Agent Type"),
                              choices = unique(trainset.bal$AgentType)),
                  actionButton("submit",label = "Predict", style="color: #343a40; background-color: #ffc107; border-color: #2e6da4"),
                  textOutput("text")),
                absolutePanel(                  selectInput(inputId = "sex",
                                                            label = strong("Sex"),
                                                            choices = unique(trainset.bal$Sex))
                              , top = 75, left = 400, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "marital",
                                                            label = strong("Marital Status"),
                                                            choices = unique(trainset.bal$MaritalStatus))
                              , top = 155, left = 400, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "fault",
                                                            label = strong("Fault in Accident"),
                                                            choices = unique(trainset.bal$Fault))
                              , top = 235, left = 400, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "weekclaimed",
                                                            label = strong("Week of Month Claimed:"),
                                                            choices = c(seq(1:5)))
                                                , top = 315, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "vehcat",
                                                            label = strong("Vehicle Category"),
                                                            choices = unique(trainset.bal$VehicleCategory))
                                                , top = 395, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "week",
                                                            label = "Week of Month:",
                                                            choices = c(seq(1:5)))
                                                , top = 475, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                 
                  selectInput(inputId = "repnum",
                              label = strong("Rep Number"),
                              choices = sort(unique(trainset.bal$RepNumber)))
                                                , top = 555, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "deductible",
                                                            label = strong("Deductible"),
                                                            choices = unique(trainset.bal$Deductible))
                                                , top = 635, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                             selectInput(inputId = "year",
                                                                       label = strong("Year"),
                                                                       choices = sort(unique(trainset.bal$Year)))
                                                , top = 715, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "basepol",
                                                            label = strong("Base Policy"),
                                                            choices = unique(trainset.bal$BasePolicy))
                                                , top = 795, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  selectInput(inputId = "address",
                                                            label = strong("Address Changed After Claim"),
                                                            choices = sort(unique(trainset.bal$AddressChange_Claim)))
                                                , top = 875, left = 400, right = NULL,
                                                bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                                                fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                  absolutePanel(sliderInput(inputId = "vehprice",
                                          "Vehicle Price:",
                                          min = 0, max = max(trainset.bal$VehiclePrice),
                                          value = mean(trainset.bal$VehiclePrice))
                              , top = 180, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(sliderInput(inputId = "Age",
                                          "Age:",
                                          min = 16, max = 120,
                                          value = 30)
                              , top = 80, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(sliderInput(inputId = "daysacc",
                                                            "Number of Days From Policy Purchased to Accident:",
                                                            min = min(trainset.bal$Days_Policy_Accident), max = max(trainset.bal$Days_Policy_Accident),
                                                            value = mean(trainset.bal$Days_Policy_Accident))

                              , top = 280, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(sliderInput(inputId = "daysclaim",
                                                                                          "Number of Days From Policy Purchased to Clai:",
                                                                                          min = min(trainset.bal$Days_Policy_Claim), max = max(trainset.bal$Days_Policy_Claim),
                                                                                          value = mean(trainset.bal$Days_Policy_Claim))
                              , top = 380, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(sliderInput(inputId = "vehage",
                                                            "AgeOfVehicle:",
                                                            min = 0, max = 100,
                                                            value = 50)
                              , top = 480, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  sliderInput(inputId = "policyage",
                                                            "Age of Policy Holder:",
                                                            min = 16, max = 120,
                                                            value = 50)
                              , top = 580, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  sliderInput(inputId = "supp",
                                                            "Number of Supplements:",
                                                            min = min(trainset.bal$NumberOfSuppliments), max=max(trainset.bal$NumberOfSuppliments),
                                                            value = mean(trainset.bal$NumberOfSuppliments))
                              , top = 680, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                absolutePanel(                  sliderInput(inputId = "cars",
                                                            label = strong("Number of Cars"),
                                                            min = 1, max = 8,
                                                            value = 4)
                              , top = 780, left = 800, right = NULL,
                              bottom = NULL, width = NULL, height = NULL, draggable = FALSE,
                              fixed = FALSE, cursor = c("auto", "move", "default", "inherit")),
                  tags$head(tags$style("#text{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                
)


server <- function(input, output) {
  datainput = reactive({
    newdf = data.table(
      "Month" = as.factor(input$month),
      "WeekOfMonth" =as.factor(input$week),
      "DayOfWeek" = as.factor(input$dayofweek),
      "Make" = as.factor(input$make),
      "AccidentArea" = as.factor(input$area),
      "DayOfWeekClaimed" = as.factor(input$dayofweekclaimed),
      "MonthClaimed" = as.factor(input$monthclaimed),
      "WeekOfMonthClaimed" = as.factor(input$weekclaimed),
      "Sex" = as.factor(input$sex),
      "MaritalStatus" = as.factor(input$marital),
      "Age" = as.integer(input$Age),
      "Fault" = as.factor(input$fault),
      "VehicleCategory" = as.factor(input$vehcat),
      "VehiclePrice" = as.integer(input$vehprice),
      "FraudFound_P" = as.factor("0"),
      "RepNumber" = as.integer(input$repnum),
      "Deductible" = as.integer(input$deductible),
      "DriverRating" = as.factor(input$rating),
      "Days_Policy_Accident" = as.integer(input$daysacc),
      "Days_Policy_Claim" = as.integer(input$daysclaim),
      "PastNumberOfClaims" = as.integer(input$pastclaims),
      "AgeOfVehicle" = as.integer(input$vehage),
      "AgeOfPolicyHolder" = as.integer(input$policyage),
      "PoliceReportFiled" = as.factor(input$police),
      "WitnessPresent" = as.factor(input$witness),
      "AgentType" = as.factor(input$agent),
      "NumberOfSuppliments" = as.integer(input$supp),
      "AddressChange_Claim" = as.integer(input$address),
      "NumberOfCars" = as.integer(input$cars),
      "Year" = as.integer(input$year),
      "BasePolicy" = as.factor(input$basepol))
    newdf2 = as.data.table(rbind(newdf,trainset.bal[1]))
    newdf2
  })
  
  
  
  pred = eventReactive(input$submit,{
    newdf2 = datainput()
    pr = predict(m1.RF,newdata = newdf2[1])
    prt = ifelse(as.character(pr) == "1", "FRAUD DETECTED! EXTRA ATTENTION REQUIRED!","NO SUSPECTED FRAUD")
    prt
  })
  
  output$text = renderText({
    pred()  
  })
}    

shinyApp(ui = ui, server = server)

