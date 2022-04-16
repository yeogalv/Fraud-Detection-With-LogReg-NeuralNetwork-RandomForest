# ==============================================================================================================
#Required Libraries
library(data.table)
library(ggplot2)
library(corrplot)

# ==============================================================================================================
#Importing datasets
fraud<- fread("fraud_clean.csv")
View(fraud)
summary(fraud)
str(fraud)

#Convert categorical variables to factor type
fraud$Month <- factor(fraud$Month, ordered = TRUE, 
                      levels = c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
fraud$WeekOfMonth <- factor(fraud$WeekOfMonth)
fraud$DayOfWeek <- factor(fraud$DayOfWeek, ordered = TRUE, levels = c('Monday', "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
fraud$Make <- factor(fraud$Make)
fraud$AccidentArea <- factor(fraud$AccidentArea)
fraud$DayOfWeekClaimed <- factor(fraud$DayOfWeekClaimed, ordered = TRUE, levels = c('Monday', "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday","0"))
fraud$MonthClaimed <- factor(fraud$MonthClaimed, ordered = TRUE, 
                             levels = c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec", "0"))
fraud$WeekOfMonthClaimed <- factor(fraud$WeekOfMonthClaimed)
fraud$Sex <- factor(fraud$Sex)
fraud$MaritalStatus <- factor(fraud$MaritalStatus)
fraud$Fault <- factor(fraud$Fault)
fraud$VehicleCategory <- factor(fraud$VehicleCategory)
fraud$FraudFound_P <- factor(fraud$FraudFound_P)
fraud$DriverRating <- factor(fraud$DriverRating, levels = c("1", "2", "3", "4"))
fraud$PoliceReportFiled <- factor(fraud$PoliceReportFiled)
fraud$WitnessPresent <- factor(fraud$WitnessPresent)
fraud$AgentType <- factor(fraud$AgentType)
fraud$BasePolicy <- factor(fraud$BasePolicy)

# ==============================================================================================================
#DATA VISUALIZATION
# ==============================================================================================================

#### Check for Data Imbalance in Categorical Variables ####
#Month
levels(fraud$Month)
table(fraud$Month)
barplot(prop.table(table(fraud$Month)),
        cex.names=0.6,
        col = rainbow(4),
        ylim = c(0, 0.2),
        main = "Month",
        ylab = "Percentage",
        xlab = "Months")

#WeekOfMonth
levels(fraud$WeekOfMonth)
table(fraud$WeekOfMonth)
barplot(prop.table(table(fraud$WeekOfMonth)),
        col = rainbow(5),
        ylim = c(0, 0.5),
        main = "WeekOfMonth",
        ylab = "Percentage",
        xlab = "The week of the month")

#DayOfWeek
levels(fraud$DayOfWeek)
barplot(prop.table(table(fraud$DayOfWeek)),
        col = rainbow(7),
        cex.names=0.6,
        ylim = c(0, 0.5),
        main = "DayOfWeek",
        ylab = "Percentage",
        xlab = "Day of the Week")

#Make 
levels(fraud$Make)
barplot(prop.table(table(fraud$Make)),
        horiz = T,
        cex.names=0.3,
        col = rainbow(7),
        xlim = c(0, 0.3),
        main = "Make",
        ylab = "Car Companies",
        xlab = "Percentage")

#AccidentArea
barplot(prop.table(table(fraud$AccidentArea)),
        col = c('lightblue','lavender'),
        ylim = c(0, 1),
        main = "AccidentArea",
        ylab = "Percentage",
        xlab = "Area") 

Location <- table(fraud$AccidentArea)
prop.table(Location)
round(prop.table(Location), digits = 2) #proportion of rural and urban is 0.1 to 0.9

#DayOfWeekClaimed
barplot(prop.table(table(fraud$DayOfWeekClaimed)),
        cex.names=0.6,
        col = rainbow(8),
        ylim = c(0, 0.3),
        main = "DayOfWeekClaimed",
        ylab = "Percentage",
        xlab = "Day of the Week") 

table(fraud$DayOfWeekClaimed) 

#MonthClaimed
levels(fraud$MonthClaimed)
barplot(prop.table(table(fraud$MonthClaimed)),
        col = rainbow(4),
        cex.names = 0.6,
        ylim = c(0, 0.2),
        main = "MonthClaimed",
        ylab = "Percentage",
        xlab = "months") 

table(fraud$MonthClaimed) 

#WeekOfMonthClaimed
barplot(prop.table(table(fraud$WeekOfMonthClaimed)),
        col = rainbow(5),
        ylim = c(0, 0.5),
        main = "WeekOfMonthClaimed",
        ylab = "Percentage",
        xlab = "Week of the month") #5th week lesser than the rest

#Sex
barplot(prop.table(table(fraud$Sex)),
        col = c('lightblue','lavender'),
        ylim = c(0, 1),
        main = "Gender Distribution",
        ylab = "Percentage",
        xlab = "Sex") 

gender <- table(fraud$Sex)
round(prop.table(gender), digits = 2) #proportion of female and male is 0.16 to 0.84

d2 <- fraud
d2 <- d2[!(Age == 0),]
min(d2$Age)
max(fraud$Age)
table(fraud$Age) #320 0 values

#MaritalStatus
barplot(prop.table(table(fraud$MaritalStatus)),
        col = c("lightblue","lavender","skyblue","powderblue"),
        ylim = c(0, 0.8),
        main = "MaritalStatus",
        ylab = "Percentage",
        xlab = "Marital Status") 

table(fraud$MaritalStatus)
marital <- table(fraud$MaritalStatus)
prop.table(marital) 

#Fault
barplot(prop.table(table(fraud$Fault)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Fault",
        ylab = "Percentage",
        xlab = "Fault") #High percentage of Policy Holders compared to Third Party


#VehicleCategory
barplot(prop.table(table(fraud$VehicleCategory)),
        col = rainbow(9),
        ylim = c(0, 0.8),
        main = "VehicleCategory",
        ylab = "Percentage",
        xlab = "Vehicle Category") #high proportion for Sedan vehicles

vehicle <- table(fraud$VehicleCategory)
round(prop.table(vehicle),2)

#FraudFound_P
barplot(prop.table(table(fraud$FraudFound_P)),
        col = c('lightblue','lavender'),
        ylim = c(0, 1),
        main = "FraudFound_P",
        ylab = "Percentage",
        xlab = "Fraud Found") #high percentage for not found

prop.table(table(fraud$FraudFound_P))

#DriverRating
barplot(prop.table(table(fraud$DriverRating)),
        col = rainbow(4),
        ylim = c(0, 0.4),
        main = "DriverRating",
        ylab = "Percentage",
        xlab = "Ratings") #evenly distributed

#PoliceReportFiled
barplot(prop.table(table(fraud$PoliceReportFiled)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "PoliceReportFiled",
        ylab = "Percentage",
        xlab = "PoliceReportFiled")

table(fraud$PoliceReportFiled) #most is no

#WitnessPresent
barplot(prop.table(table(fraud$WitnessPresent)),
        col = c("lightblue",'lavender'),
        ylim = c(0, 1),
        main = "WitnessPresent",
        ylab = "Percentage",
        xlab = "WitnessPresent")

table(fraud$WitnessPresent) 

#AgentType
barplot(prop.table(table(fraud$AgentType)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "AgentType",
        ylab = "Percentage",
        xlab = "Type")

agent <- table(fraud$AgentType) #most is external
prop.table(agent)


#BasePolicy
barplot(prop.table(table(fraud$BasePolicy)),
        col = rainbow(3),
        ylim = c(0, 0.5),
        main = "BasePolicy",
        ylab = "Percentage",
        xlab = "BasePolicy")

#There are 18 categorical variables

#Analysis of Fraud and Categorical Variables
#FraudFound_P vs Sex
# stacked bar chart
ggplot(fraud, 
       aes(x = Sex, 
           fill = FraudFound_P)) + 
  geom_bar(position = "fill") 


#FraudFound_P vs AccidentArea
# stacked bar chart
ggplot(fraud, 
       aes(x = AccidentArea, 
           fill = FraudFound_P)) + 
  geom_bar(position = "fill")  

#FraudFound_P vs MaritalStatus
# stacked bar chart
ggplot(fraud, 
       aes(x = MaritalStatus, 
           fill = FraudFound_P)) + 
  geom_bar(position = "fill")

#FraudFound_P vs VehicleCategory
# stacked bar chart
ggplot(fraud, 
       aes(x = VehicleCategory, 
           fill = FraudFound_P)) + 
  geom_bar(position = "fill")  

#FraudFound_P vs BasePolicy
# stacked bar chart
ggplot(fraud, 
       aes(x =BasePolicy, 
           fill = FraudFound_P)) + 
  geom_bar(position = "stack")  



#### Explore Continuous Variables ####

#Correlation Matrix Plot of all continuous variables
cont <- fraud[,c("Age","RepNumber",'Deductible','VehiclePrice','Days_Policy_Accident','Days_Policy_Claim','PastNumberOfClaims','AgeOfVehicle','AgeOfPolicyHolder','NumberOfSuppliments','AddressChange_Claim','NumberOfCars',"Year")]
str(cont)
#Correlation Plot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(cont), type = "upper", method = "color",
         diag= FALSE,
         col = col(200),
         na.label = "square",
         title = "Correlation Plot of fraud Dataset variables",
         addCoef.col = "black", number.cex=0.55,
         mar=c(0,0,1,0))

#Histogram of key variables
#Age Distribution
ggplot(cont, aes(x=Age))+
  geom_histogram(color="darkblue", fill="lightblue")+
  ggtitle("Age Distribution") + theme(
    plot.title = element_text(color="black", size=14, face="bold.italic", hjust=0.5),
  )


##Boxplot of individual variables
#Age
summary(fraud$Age)
boxplot(fraud$Age,
        ylab="Age",
        main = "Boxplot of Age",
        col="lightblue")

#RepNumber
summary(fraud$RepNumber)
boxplot(fraud$RepNumber,
        ylab="RepNumber",
        main = "Boxplot of RepNumber",
        col="lightblue")

#Deductible
summary(fraud$Deductible)
boxplot(fraud$Deductible,
        main = "Boxplot of Deductible",
        ylab="Deductible",
        col="lightblue") 


#VehiclePrice
summary(fraud$VehiclePrice)
boxplot(fraud$VehiclePrice,
        main = "Boxplot of VehiclePrice",
        ylab="VehiclePrice",
        col="lightblue")

#Days_Policy_Accident
summary(fraud$Days_Policy_Accident)
boxplot(fraud$Days_Policy_Accident,
        main = "Boxplot of Days_Policy_Accident",
        ylab="Days_Policy_Accident",
        col="lightblue") 

#Days_Policy_Claim
summary(fraud$Days_Policy_Claim)
boxplot(fraud$Days_Policy_Claim,
        main = "Boxplot of Days_Policy_Claim",
        ylab="Days_Policy_Claim",
        col="lightblue") 

#PastNumberofClaims
summary(fraud$PastNumberOfClaims)
boxplot(fraud$PastNumberOfClaims,
        main = "Boxplot of PastNumberOfClaims",
        ylab="PastNumberOfClaims",
        col="lightblue") 

#AgeofVehicle
summary(fraud$AgeOfVehicle)
boxplot(fraud$AgeOfVehicle,
        main = "Boxplot of AgeOfVehicle",
        ylab="AgeOfVehicle",
        col="lightblue") 

#AgeofPolicyHolder
summary(fraud$AgeOfPolicyHolder)
boxplot(fraud$AgeOfPolicyHolder,
        main = "Boxplot of AgeofPolicyHolder",
        ylab="AgeofPolicyHolder",
        col="lightblue") 

#NumberOfSuppliments
summary(fraud$NumberOfSuppliments)
boxplot(fraud$NumberOfSuppliments,
        main = "Boxplot of NumberOfSuppliments",
        ylab="NumberOfSuppliments",
        col="lightblue") 

#AddressChange_Claim
summary(fraud$AddressChange_Claim)
boxplot(fraud$AddressChange_Claim,
        main = "Boxplot of AddressChange_Claim",
        ylab="AddressChange_Claim",
        col="lightblue") 

#NumberofCars
fraud$NumberOfCars <- as.numeric(fraud$NumberOfCars)
summary(fraud$NumberOfCars)
boxplot(fraud$NumberOfCars,
        main = "Boxplot of NumberOfCars",
        ylab="NumberOfCars",
        col="lightblue") 



#Compare with FraudFound_P
ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=Age)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="Age")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=RepNumber)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="RepNumber")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=Deductible)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="Deductible")


ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=VehiclePrice)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="VehiclePrice")

out <- boxplot.stats(fraud$VehiclePrice[fraud$FraudFound_P ==1])$out #Identify the outliers
length(out) #There are 192 outliers
out <- boxplot.stats(fraud$VehiclePrice[fraud$FraudFound_P ==0])$out #Identify the outliers
length(out) #There are 2143 outliers

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=Days_Policy_Accident)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="Days_Policy_Accident")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=Days_Policy_Claim)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="Days_Policy_Claim")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=PastNumberOfClaims)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="PastNumberOfClaims")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=AgeOfVehicle)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="AgeOfVehicle")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=AgeOfPolicyHolder)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="AgeOfPolicyHolder")

out <- boxplot.stats(fraud$AgeOfPolicyHolder[fraud$FraudFound_P ==1])$out #Identify the outliers
length(out) #There are 105 outliers
out <- boxplot.stats(fraud$AgeOfPolicyHolder[fraud$FraudFound_P ==0])$out #Identify the outliers
length(out) #There are 1084 outliers

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=NumberOfSuppliments)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="NumberOfSuppliments")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=AddressChange_Claim)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="AddressChange_Claim")

ggplot(data = fraud, aes(x=as.character(FraudFound_P), y=NumberOfCars)) +
  geom_boxplot(fill="steelblue") +
  labs(title="",
       x="FraudFound_P", y="NumberOfCars")




