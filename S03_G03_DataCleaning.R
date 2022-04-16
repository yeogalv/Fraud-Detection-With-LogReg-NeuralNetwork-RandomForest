# ==============================================================================================================
#Required Libraries
library(data.table)
library(ggplot2)


# ==============================================================================================================
#Importing datasets
fraud<- fread("fraud_oracle.csv", stringsAsFactors = TRUE)
View(fraud)

# ==============================================================================================================
#Data Exploration
summary(fraud)
dim(fraud) #number of rows and columns
sapply(fraud, class) #data type of all columns

#Total number of NA values in the dataset 
navalues <- sum(is.na(fraud))
navalues #There are no missing values in the dataset

#Convert categorical variables to factor type
fraud$WeekOfMonth <- factor(fraud$WeekOfMonth)
fraud$WeekOfMonthClaimed <- factor(fraud$WeekOfMonthClaimed)
fraud$FraudFound_P <- factor(fraud$FraudFound_P)
fraud$DriverRating <- factor(fraud$DriverRating, levels = c("1", "2", "3", "4"))

levels(fraud$WeekOfMonth)
levels(fraud$WeekOfMonthClaimed)
levels(fraud$FraudFound_P) # 0 for not fraud and 1 for fraud
levels(fraud$DriverRating) # 1 to 4

summary(fraud)

#Drop PolicyNumber since it is just uniquely identifying each instance
fraud[, PolicyNumber:=NULL]

#Drop PolicyType
fraud[, PolicyType:=NULL]

#Check year 
table(fraud$Year) #Data collected from 1994 to 1996

##Convert Categorical numeric range variables to continuous
#Days_Policy_Accident
levels(fraud$Days_Policy_Accident)
table(fraud$Days_Policy_Accident)
fraud$Days_Policy_Accident <- as.character(fraud$Days_Policy_Accident)
table(fraud$Days_Policy_Accident)

for(i in 1:15420){
  if(fraud$Days_Policy_Accident[i] == "1 to 7"){
    fraud$Days_Policy_Accident[i] <- as.numeric(fraud$Days_Policy_Accident[i])
    fraud$Days_Policy_Accident[i] = sample(1:7,1)
  }
  if(fraud$Days_Policy_Accident[i] == "15 to 30"){
    fraud$Days_Policy_Accident[i] <- as.numeric(fraud$Days_Policy_Accident[i])
    fraud$Days_Policy_Accident[i] = sample(16:30,1) 
  }
  if(fraud$Days_Policy_Accident[i] == "8 to 15"){
    fraud$Days_Policy_Accident[i] <- as.numeric(fraud$Days_Policy_Accident[i])
    fraud$Days_Policy_Accident[i] = sample(8:15,1)
  }
  if(fraud$Days_Policy_Accident[i] ==  "more than 30"){ #For "more than 30 replace with numbers between 30 and 60
    fraud$Days_Policy_Accident[i] <- as.numeric(fraud$Days_Policy_Accident[i])
    fraud$Days_Policy_Accident[i] = sample(31:60,1)
  }
  if(fraud$Days_Policy_Accident[i] == "none"){
    fraud$Days_Policy_Accident[i] <- as.numeric(fraud$Days_Policy_Accident[i])
    fraud$Days_Policy_Accident[i] = 0 #Put 0 for none
  }
}


#Days_Policy_Claim
levels(fraud$Days_Policy_Claim)
table(fraud$Days_Policy_Claim)
fraud$Days_Policy_Claim <- as.character(fraud$Days_Policy_Claim)
table(fraud$Days_Policy_Claim)
for(i in 1:15420){
  if(fraud$Days_Policy_Claim[i] == "15 to 30"){
    fraud$Days_Policy_Claim[i] <- as.numeric(fraud$Days_Policy_Claim[i])
    fraud$Days_Policy_Claim[i] = sample(16:30,1)
  }
  if(fraud$Days_Policy_Claim[i] == "8 to 15"){
    fraud$Days_Policy_Claim[i] <- as.numeric(fraud$Days_Policy_Claim[i])
    fraud$Days_Policy_Claim[i] = sample(8:15,1) 
  }
  if(fraud$Days_Policy_Claim[i] == "more than 30"){
    fraud$Days_Policy_Claim[i] <- as.numeric(fraud$Days_Policy_Claim[i])
    fraud$Days_Policy_Claim[i] = sample(31:60,1)
  }
  if(fraud$Days_Policy_Claim[i] == "none"){
    fraud$Days_Policy_Claim[i] <- as.numeric(fraud$Days_Policy_Claim[i])
    fraud$Days_Policy_Claim[i] = 0 #Put 0 for none
  }
}

#PastNumberofClaims
levels(fraud$PastNumberOfClaims)
table(fraud$PastNumberOfClaims)
fraud$PastNumberOfClaims <- as.character(fraud$PastNumberOfClaims) #convert to character
table(fraud$PastNumberOfClaims)
for(i in 1:15420){
  if(fraud$PastNumberOfClaims[i] == "1"){
    fraud$PastNumberOfClaims[i] <- as.numeric(fraud$PastNumberOfClaims[i])
    fraud$PastNumberOfClaims[i] = 1
  }
  if(fraud$PastNumberOfClaims[i] == "2 to 4"){
    fraud$PastNumberOfClaims[i] <- as.numeric(fraud$PastNumberOfClaims[i])
    fraud$PastNumberOfClaims[i] = sample(2:4,1)
  }
  if(fraud$PastNumberOfClaims[i] == "more than 4"){ #For "more than 4 replace with numbers between 4 and 10
    fraud$PastNumberOfClaims[i] <- as.numeric(fraud$PastNumberOfClaims[i])
    fraud$PastNumberOfClaims[i] = sample(5:10,1)
  }
  if(fraud$PastNumberOfClaims[i] == "none"){
    fraud$PastNumberOfClaims[i] <- as.numeric(fraud$PastNumberOfClaims[i])
    fraud$PastNumberOfClaims[i] = 0 #Put 0 for none
  }
}



#AgeofVehicle
levels(fraud$AgeOfVehicle)
table(fraud$AgeOfVehicle)
fraud$AgeOfVehicle <- as.character(fraud$AgeOfVehicle) #convert to character
table(fraud$AgeOfVehicle)
for(i in 1:15420){
  if(fraud$AgeOfVehicle[i] == "2 years"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = 2
  }
  if(fraud$AgeOfVehicle[i] == "3 years"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = 3
  }
  if(fraud$AgeOfVehicle[i] == "4 years"){ 
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = 4
  }
  if(fraud$AgeOfVehicle[i] == "5 years"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = 5
  }
  if(fraud$AgeOfVehicle[i] == "6 years"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = 6
  }
  if(fraud$AgeOfVehicle[i] == "7 years"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = 7
  }
  if(fraud$AgeOfVehicle[i] == "more than 7"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = sample(8:15,1) 
  }
  if(fraud$AgeOfVehicle[i] == "new"){
    fraud$AgeOfVehicle[i] <- as.numeric(fraud$AgeOfVehicle[i])
    fraud$AgeOfVehicle[i] = sample(0:1,1) #either 0 or 1 years old
  }
}

#AgeofPolicyHolder
levels(fraud$AgeOfPolicyHolder)
table(fraud$AgeOfPolicyHolder)
fraud$AgeOfPolicyHolder <- as.character(fraud$AgeOfPolicyHolder) #convert to character
table(fraud$AgeOfPolicyHolder)
for(i in 1:15420){
  if(fraud$AgeOfPolicyHolder[i] == "16 to 17"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(16:17,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "18 to 20"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(18:20,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "21 to 25"){ 
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(21:25,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "26 to 30"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(26:30,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "31 to 35"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(31:35,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "36 to 40"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(36:40,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "41 to 50"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(41:50,1)
  }
  if(fraud$AgeOfPolicyHolder[i] == "51 to 65"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(51:65,1) 
  }
  if(fraud$AgeOfPolicyHolder[i] == "over 65"){
    fraud$AgeOfPolicyHolder[i] <- as.numeric(fraud$AgeOfPolicyHolder[i])
    fraud$AgeOfPolicyHolder[i] = sample(66:80,1) 
  }
}

#NumberOfSuppliments
levels(fraud$NumberOfSuppliments)
table(fraud$NumberOfSuppliments)
fraud$NumberOfSuppliments <- as.character(fraud$NumberOfSuppliments) #convert to character
table(fraud$NumberOfSuppliments)
for(i in 1:15420){
  if(fraud$NumberOfSuppliments[i] == "1 to 2"){
    fraud$NumberOfSuppliments[i] <- as.numeric(fraud$NumberOfSuppliments[i])
    fraud$NumberOfSuppliments[i] = sample(1:2,1)
  }
  if(fraud$NumberOfSuppliments[i] == "3 to 5"){
    fraud$NumberOfSuppliments[i] <- as.numeric(fraud$NumberOfSuppliments[i])
    fraud$NumberOfSuppliments[i] = sample(3:5,1)
  }
  if(fraud$NumberOfSuppliments[i] == "more than 5"){ 
    fraud$NumberOfSuppliments[i] <- as.numeric(fraud$NumberOfSuppliments[i])
    fraud$NumberOfSuppliments[i] = sample(6:10,1) #from 6 to 10
  }
  if(fraud$NumberOfSuppliments[i] == "none"){
    fraud$NumberOfSuppliments[i] <- as.numeric(fraud$NumberOfSuppliments[i])
    fraud$NumberOfSuppliments[i] = 0 #Put 0 for none
  }
}

#AddressChange_Claim
levels(fraud$AddressChange_Claim)
table(fraud$AddressChange_Claim)
fraud$AddressChange_Claim <- as.character(fraud$AddressChange_Claim) #convert to character
table(fraud$AddressChange_Claim)
for(i in 1:15420){
  if(fraud$AddressChange_Claim[i] == "1 year"){
    fraud$AddressChange_Claim[i] <- as.numeric(fraud$AddressChange_Claim[i])
    fraud$AddressChange_Claim[i] = 1
  }
  if(fraud$AddressChange_Claim[i] == "2 to 3 years"){
    fraud$AddressChange_Claim[i] <- as.numeric(fraud$AddressChange_Claim[i])
    fraud$AddressChange_Claim[i] = sample(2:3,1)
  }
  if(fraud$AddressChange_Claim[i] == "4 to 8 years"){ 
    fraud$AddressChange_Claim[i] <- as.numeric(fraud$AddressChange_Claim[i])
    fraud$AddressChange_Claim[i] = sample(4:8,1) #from 6 to 10
  }
  if(fraud$AddressChange_Claim[i] == "no change"){
    fraud$AddressChange_Claim[i] <- as.numeric(fraud$AddressChange_Claim[i])
    fraud$AddressChange_Claim[i] = 0 #Put 0 for no change
  }
  if(fraud$AddressChange_Claim[i] == "under 6 months"){
    fraud$AddressChange_Claim[i] <- as.numeric(fraud$AddressChange_Claim[i])
    fraud$AddressChange_Claim[i] = 0.5 #Put 0.5 for under 6 months
  }
}

#NumberofCars
levels(fraud$NumberOfCars)
table(fraud$NumberOfCars)
fraud$NumberOfCars <- as.character(fraud$NumberOfCars) #convert to character
table(fraud$NumberOfCars)
for(i in 1:15420){
  if(fraud$NumberOfCars[i] == "1 vehicle"){
    fraud$NumberOfCars[i] <- as.numeric(fraud$NumberOfCars[i])
    fraud$NumberOfCars[i] = 1
  }
  if(fraud$NumberOfCars[i] == "2 vehicle"){
    fraud$NumberOfCars[i] <- as.numeric(fraud$NumberOfCars[i])
    fraud$NumberOfCars[i] = 2
  }
  if(fraud$NumberOfCars[i] == "3 to 4"){ 
    fraud$NumberOfCars[i] <- as.numeric(fraud$NumberOfCars[i])
    fraud$NumberOfCars[i] = sample(3:4,1)
  }
  if(fraud$NumberOfCars[i] == "5 to 8"){
    fraud$NumberOfCars[i] <- as.numeric(fraud$NumberOfCars[i])
    fraud$NumberOfCars[i] = sample(5:8,1) #Put 0 for none
  }
  if(fraud$NumberOfCars[i] == "more than 8"){
    fraud$NumberOfCars[i] <- as.numeric(fraud$NumberOfCars[i])
    fraud$NumberOfCars[i] = sample(9:15,1) 
  }
}

#VehiclePrice
levels(fraud$VehiclePrice)
table(fraud$VehiclePrice)
fraud$VehiclePrice <- as.character(fraud$VehiclePrice) #convert to character
table(fraud$VehiclePrice)
for(i in 1:15420){
  if(fraud$VehiclePrice[i] == "20000 to 29000"){
    fraud$VehiclePrice[i] <- as.numeric(fraud$VehiclePrice[i])
    fraud$VehiclePrice[i] = sample(20000:29000,1)
  }
  if(fraud$VehiclePrice[i] == "30000 to 39000"){
    fraud$VehiclePrice[i] <- as.numeric(fraud$VehiclePrice[i])
    fraud$VehiclePrice[i] = sample(30000:39000,1)
  }
  if(fraud$VehiclePrice[i] == "40000 to 59000"){ 
    fraud$VehiclePrice[i] <- as.numeric(fraud$VehiclePrice[i])
    fraud$VehiclePrice[i] = sample(40000:59000,1)
  }
  if(fraud$VehiclePrice[i] == "60000 to 69000"){
    fraud$VehiclePrice[i] <- as.numeric(fraud$VehiclePrice[i])
    fraud$VehiclePrice[i] = sample(60000:69000,1)
  }
  if(fraud$VehiclePrice[i] == "less than 20000"){
    fraud$VehiclePrice[i] <- as.numeric(fraud$VehiclePrice[i])
    fraud$VehiclePrice[i] = sample(5000:19000,1)
  }
  if(fraud$VehiclePrice[i] == "more than 69000"){
    fraud$VehiclePrice[i] <- as.numeric(fraud$VehiclePrice[i])
    fraud$VehiclePrice[i] = sample(69001:100000,1)
  }
}

# Write CSV
fwrite(fraud, "fraud_clean.csv")


