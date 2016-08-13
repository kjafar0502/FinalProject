rm(list = ls())
# Set working directory
setwd("C:/Users/Khaled_Jafar/Desktop/DataScienceClass")

#creaste ODBC connection to SQL Server CRMART
library(RODBC)
connection<-odbcDriverConnect('driver={SQL Server};server=crmart;trusted_connection=true')
# Bring in a few fields from Deven's order table

LT_datalite <- sqlQuery(connection,
                 "
                 select
                 Order_Num
                 ,Order_Process_Start_Date
                 ,Order_DT_Day_of_Week
                 ,Order_Entry_Hour_of_Day
                 ,order_date
                 ,[POI_Delivery_Date_1st_Attempt_SD_DD]
                 ,Delivery_Attempt_1
                 ,Delivery_Date
                 ,Estimated_Ship_Date
                 ,Estimated_Delivery_Date
                 ,Range_Delivery_Outcome
                 ,Src_Channel
                 ,SDS_NBD_Flag
                 ,Sys_Qty
                 ,order_revenue_amt
                 ,Delivery_Promise_Flag
                 ,Wkly_Scorecard_LOB_SSC_Grouping
                 ,Product_Desc
                 ,Product_Line_Desc
                 ,Sku_Class
                 ,SSC_Code
                 ,FGA_ID
                 ,Purchase_Channel
                 ,Pay_Desc
                 ,local_ship_code_desc
                 ,Direct_Ship_Flag
                 ,ShipTo_Country
                 ,ShipTo_State_Province
                 ,Merge_Facility
                 ,Build_Facility
                 ,CS_Flag
                 ,Warehousing_Flag
                 ,second_touch_flag
                 ,Large_Order_Flag
                 ,Pay_Code_Lead_Time
                 ,Payment_Clear_Days
                 ,Mfg_Lead_Time
                 ,Mfg_Lead_Time_ATS
                 ,Longest_Sku_Lead_Time
                 ,Freight_Lead_Time
                 
                 from 
                 DEV_Deven.dbo.OrderExperienceSample_Output
                 
                 where
                 Order_date between '2016-04-01' and '2016-04-30' 
                 and ([Wkly_Scorecard_LOB_SSC_Grouping]='Client BTS' 
                 or [Wkly_Scorecard_LOB_SSC_Grouping]='Client BTO') 
                 and Order_Country='United States' 
                 and ShipTo_Country='USA'
                 and Delivery_Date is not null 
                 and Order_Status='IN'
                 and Range_Delivery_Outcome != 'No condition met'
                 ")

#change Order_Num to character so I can join it to add information from a few other tables

LT_datalite$Order_Num <- as.character(LT_datalite$Order_Num)

LT_data_add <- sqlQuery(connection,
                     "
                     select Distinct
                     ORD_NBR
                     ,QTE_SRC_APP_NM
                     ,CFI_FLG
                     ,PAY_1_CD
                     
                     from 
                     BTPO_Analytics.dbo.orders
                     
                     where
                     ORD_DT between '2016-04-01' and '2016-04-30' 
                     and CTRY_DESC ='United States' 
                     ")

LT_data_add2 <- sqlQuery(connection,
                      "
                      select Distinct
                      ORD_NBR
                      ,LCL_ORD_SRC_CD
                      
                      
                      from 
                      CORE_FACT.dbo.FactHeader
                      
                      where
                      ORD_DT between '2016-03-20' and '2016-05-14' 
                      and SRC_BU_ID ='11' 
                      ")


# Rename ORD_NBR column to Order_Num to match LT_datalite table to join it later and add a few extra columns
colnames(LT_data_add)[1] <- "Order_Num"
colnames(LT_data_add2)[1] <- "Order_Num"
# trim whitespace
LT_data_add$Order_Num <- trimws(LT_data_add$Order_Num)
LT_data_add2$Order_Num <- trimws(LT_data_add2$Order_Num)

library(dplyr)
LT_datalite2 <-left_join(LT_datalite, LT_data_add, by = "Order_Num")
LT_datalite3 <-left_join(LT_datalite2, LT_data_add2, by = "Order_Num")


# Reset LT_datalite from LT_datalite2
LT_datalite <- LT_datalite3 

# Clean up workspace
rm(data,data_add, data_add2, LT_data_add, LT_data_add2, LT_datalite2, LT_datalite3 )
rm(data_add2)
rm(LT_datalite2)
rm(LT_data_add2)


# convert some dates from factor to date
LT_datalite$order_date <- as.Date(LT_datalite$order_date)
LT_datalite$Estimated_Delivery_Date <- as.Date(LT_datalite$Estimated_Delivery_Date)
LT_datalite$Delivery_Date <- as.Date(LT_datalite$Delivery_Date)
LT_datalite$POI_Delivery_Date_1st_Attempt_SD_DD <- as.Date(LT_datalite$POI_Delivery_Date_1st_Attempt_SD_D)
LT_datalite$Delivery_Attempt_1 <- as.Date(LT_datalite$Delivery_Attempt_1)
LT_datalite$Estimated_Ship_Date<- as.Date(LT_datalite$Estimated_Ship_Date)
LT_datalite$Estimated_Delivery_Date<- as.Date(LT_datalite$Estimated_Delivery_Date)

# Create a new field called Delivery1 to store the date of the first delivery attempt. Start with POI_Delivery_Date_1st_Attempt_SD_DD as the date and then replace NA's with the value from Delivery_Attempt_1 column

LT_datalite$Delivery1 <- LT_datalite$POI_Delivery_Date_1st_Attempt_SD_DD

library(lubridate)

LT_datalite$Delivery1 <- with(LT_datalite, as.Date(ifelse(is.na(Delivery1), Delivery_Attempt_1, Delivery1), origin = "1970-01-01"))

# Let's now create  a payment_LT that combines Pay_Code_Lead_Time &  Payment_Clear_Days
LT_datalite$Pay_Lead_Time <- LT_datalite$Pay_Code_Lead_Time
LT_datalite$Pay_Lead_Time <- with(LT_datalite, ifelse(is.na(Pay_Lead_Time), Payment_Clear_Days, Pay_Lead_Time))
LT_datalite$Pay_Lead_Time <- as.numeric(LT_datalite$Pay_Lead_Time)
LT_datalite$Pay_Lead_Time[is.na(LT_datalite$Pay_Lead_Time)] <- round(mean(LT_datalite$Pay_Lead_Time, na.rm = TRUE))                          

#Let's now group the paycodes into something more understandable; we'll first create a new field then change the values of the field.
LT_datalite$Payment_Type <- LT_datalite$PAY_1_CD
str(LT_datalite)

LT_datalite$Payment_Type <- as.factor(LT_datalite$Payment_Type)
str(LT_datalite)


levels(LT_datalite$Payment_Type)
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "8"] <- "Gift Card"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "C"] <- "PayPal"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "PayPal Payment"] <- "PayPal"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "A"] <- "Credit Card"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "D"] <- "Credit Card"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "M"] <- "Credit Card"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "V"] <- "Credit Card"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "Credit or Debit Card Payment in advance."] <- "Credit Card"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "5"] <- "DFS Lease"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "R"] <- "DFS-DPA"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "Dell Preferred Account"] <- "DFS-DPA"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "#"] <- "Terms"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "O"] <- "Terms"

levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "B"] <- "Other"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "F"] <- "Other"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "N"] <- "Other"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "P"] <- "Other"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "W"] <- "Other"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "Prepaid with Cash"] <- "Other"
levels(LT_datalite$Payment_Type)[levels(LT_datalite$Payment_Type)== "No Charge to be used with free of charge orders"] <- "Other"

table(LT_datalite$Payment_Type)


# load dplyr and filter out negative days
library(dplyr)
# Calculating calendar days difference between order date and EDD
LT_datalite$deltacaldaysEDDtoOrder <- LT_datalite$Estimated_Delivery_Date -  LT_datalite$order_date


LT_datalite <- LT_datalite %>% filter(LT_datalite$Estimated_Delivery_Date >= LT_datalite$order_date)
LT_datalite <- LT_datalite %>% filter(LT_datalite$Delivery1 >= LT_datalite$order_date)




# Lets calculate the difference in calendar days between Delivery 1 and EDD to set the most strict delivery window using exact date
LT_datalite$caldaysEDDtoDelivery1 <- LT_datalite$Delivery1 - LT_datalite$Estimated_Delivery_Date 


LT_datalite$DeliveryOutcomeExactDay <- 
  ifelse(LT_datalite$caldaysEDDtoDelivery1 >0, "LATE",
         ifelse(LT_datalite$caldaysEDDtoDelivery1 <0, "EARLY",
                ifelse(LT_datalite$caldaysEDDtoDelivery1 == 0, "ONTIME",
                       NA  )))


# Calculating business days difference for (order_date to EDD) & (order_date to Delivery1)

library(bizdays)
cal <- Calendar(weekdays=c("saturday", "sunday"))
LT_datalite$bizdaystoEDD <- bizdays(LT_datalite$order_date, LT_datalite$Estimated_Delivery_Date, cal)
LT_datalite$bizdaysdeliv <- bizdays(LT_datalite$order_date, LT_datalite$Delivery1, cal)

# Let's now create a variable which is the delta between business days for EDD and Delivery; Negative means ealry and positive means late
LT_datalite$deltabizdays <- LT_datalite$bizdaysdeliv -  LT_datalite$bizdaystoEDD  


# Lets calculate the Delivery Windown Outcome now based on defined window range

LT_datalite$DeliveryOutcomeWindowRange <- 
  ifelse(LT_datalite$bizdaystoEDD <= 2 & LT_datalite$deltabizdays <= -1, "EARLY",
  ifelse(LT_datalite$bizdaystoEDD <= 2 & LT_datalite$deltabizdays == 0, "ONTIME",
  ifelse(LT_datalite$bizdaystoEDD <= 2 & LT_datalite$deltabizdays >= 1, "LATE",
ifelse(LT_datalite$bizdaystoEDD >= 3 &  LT_datalite$bizdaystoEDD <= 4 & LT_datalite$deltabizdays <= -2, "EARLY",
ifelse(LT_datalite$bizdaystoEDD >= 3 &  LT_datalite$bizdaystoEDD <= 4 & LT_datalite$deltabizdays <= 0 & LT_datalite$deltabizdays >= -1, "ONTIME",
ifelse(LT_datalite$bizdaystoEDD >= 3 &  LT_datalite$bizdaystoEDD <= 4 & LT_datalite$deltabizdays >= 1, "LATE",  
  ifelse(LT_datalite$bizdaystoEDD >= 5 &  LT_datalite$bizdaystoEDD <= 6 & LT_datalite$deltabizdays <= -3, "EARLY",
  ifelse(LT_datalite$bizdaystoEDD >= 5 &  LT_datalite$bizdaystoEDD <= 6 & LT_datalite$deltabizdays <= 0 & LT_datalite$deltabizdays >= -2, "ONTIME",
  ifelse(LT_datalite$bizdaystoEDD >= 5 &  LT_datalite$bizdaystoEDD <= 6 & LT_datalite$deltabizdays >= 1, "LATE",  
ifelse(LT_datalite$bizdaystoEDD >= 7 &  LT_datalite$bizdaystoEDD <= 8 & LT_datalite$deltabizdays <= -4, "EARLY",
ifelse(LT_datalite$bizdaystoEDD >= 7 &  LT_datalite$bizdaystoEDD <= 8 & LT_datalite$deltabizdays <= 0 & LT_datalite$deltabizdays >= -3, "ONTIME",
ifelse(LT_datalite$bizdaystoEDD >= 7 &  LT_datalite$bizdaystoEDD <= 8 & LT_datalite$deltabizdays >= 1, "LATE",  
  ifelse(LT_datalite$bizdaystoEDD >= 9 &  LT_datalite$bizdaystoEDD <= 10 & LT_datalite$deltabizdays <= -5, "EARLY",
  ifelse(LT_datalite$bizdaystoEDD >= 9 &  LT_datalite$bizdaystoEDD <= 10 & LT_datalite$deltabizdays <= 0 & LT_datalite$deltabizdays >= -4, "ONTIME",
  ifelse(LT_datalite$bizdaystoEDD >= 9 &  LT_datalite$bizdaystoEDD <= 10 & LT_datalite$deltabizdays >= 1, "LATE",  
ifelse(LT_datalite$bizdaystoEDD >= 11 &  LT_datalite$bizdaystoEDD <= 12 & LT_datalite$deltabizdays <= -6, "EARLY",
ifelse(LT_datalite$bizdaystoEDD >= 11 &  LT_datalite$bizdaystoEDD <= 12 & LT_datalite$deltabizdays <= 0 & LT_datalite$deltabizdays >= -5, "ONTIME",
ifelse(LT_datalite$bizdaystoEDD >= 11 &  LT_datalite$bizdaystoEDD <= 12 & LT_datalite$deltabizdays >= 1, "LATE",                                 
  ifelse(LT_datalite$bizdaystoEDD >= 13 & LT_datalite$deltabizdays <= -7, "EARLY",
  ifelse(LT_datalite$bizdaystoEDD >= 13 & LT_datalite$deltabizdays <= 0 & LT_datalite$deltabizdays >= -6, "ONTIME",
  ifelse(LT_datalite$bizdaystoEDD >= 13 & LT_datalite$deltabizdays >= 1, "LATE",                      
NA  )))))))))))))))))))))


# add a field to determine the order day of the week
LT_datalite$dayofweek <- weekdays(as.Date(LT_datalite$order_date))


# clean up more of the data
LTdata$Pay_Lead_Time[is.na(LTdata$Pay_Lead_Time)] <- round(mean(LTdata$Pay_Lead_Time, na.rm = TRUE))

levels(LTdata$Purchase_Channel) <- c(levels(LTdata$Purchase_Channel),"Offline") # introduce a new factor
LTdata[is.na(LTdata$Purchase_Channel),"Purchase_Channel"]<-"Offline" #replace NA values in Purchase Channel field with "Offline"
levels(LTdata$QTE_SRC_APP_NM) <- c(levels(LTdata$QTE_SRC_APP_NM),"Other") # introduce a new factor
LTdata[is.na(LTdata$QTE_SRC_APP_NM),"QTE_SRC_APP_NM"]<-"Other" #replace NA values 
levels(LTdata$Payment_Type) <- c(levels(LTdata$Payment_Type),"Other") # introduce a new factor
LTdata[is.na(LTdata$Payment_Type),"Payment_Type"]<-"Other" #replace NA values 


levels(LTdata$Merge_Facility) <- c(levels(LTdata$Merge_Facility),"Other") # introduce a new factor
LTdata[is.na(LTdata$Merge_Facility),"Merge_Facility"]<-"Other" #replace NA values 
levels(LTdata$Build_Facility) <- c(levels(LTdata$Build_Facility),"Other") # introduce a new factor
LTdata[is.na(LTdata$Build_Facility),"Build_Facility"]<-"Other" #replace NA values i with "Offline"

LTdata$Direct_Ship_Flag[is.na(LTdata$Direct_Ship_Flag)] <- "Y"
LTdata$CFI_FLG[is.na(LTdata$CFI_FLG)] <- "N"

# save final cleaned up dataset to csv
write.csv(LTdata, "LTdata.csv")

  
# Load key packages
library(dplyr) 
library(ggplot2)
library(caTools) # for subsetting the data
library(Amelia) # creates a map of the missing data
library(corrplot) # for plotting correlations
library(hydroGOF) # calcualting MSE, RMSE, R squared ...
library(caret) # great package for modeling
library(rpart) # decision tree modeling
library(randomForest) # modeling random forest
library(cluster) # for clustering


# Load & inspect dataset
setwd("C:/Users/Khaled_Jafar/Desktop/DataScienceClass")
LTdata <- read.csv('LTdata.csv')
dim(LTdata)
str(LTdata)
head(LTdata)
summary(LTdata)

#identify key missing data
missmap(LTdata, col=c("black", "grey"), legend=FALSE)  # create a missing map

# Split-out validation dataset

set.seed(7)
validationIndex <- createDataPartition(LTdata$bizdaysdeliv, p=0.2, list=FALSE)
# select 20% of the data for validation
validation <- LTdata[-validationIndex,]
# use the remaining 80% of data to training and testing the models
dataset <- LTdata[validationIndex,]

# code for using random forest for variable importance

fit.rfvariable <- randomForest(bizdaysdeliv ~ LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, ntree = 25, data = dataset)


# Variable Importance Plot
varImpPlot(fit.rfvariable,
           sort = T,
           main="Variable Importance",
           n.var=25)


# calculate correlation matrix

correlationMatrix <- cor(LTdata[c("Src_Channel", "Sys_Qty", "order_revenue_amt", "CS_Flag","Warehousing_Flag", "second_touch_flag", "Large_Order_Flag","Pay_Lead_Time", "Mfg_Lead_Time", "Freight_Lead_Time","bizdaystoEDD", "bizdaysdeliv")]) # If just want to do correlations on certain fields:  

# summarize the correlation matrix
print(correlationMatrix)
# create correlation plot
corrplot(correlationMatrix, method="number", type="upper")

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)



# Correlation part 2: When one of the feature is categorical  and the other is continues, like "State" vs. the dependent variable(delivery day- order day) . You can test the significance using ANOVA , you can use ?aov. 

# ANOVA will determine if there are signficant difference between groups
# Tukey HSD tests will determine WHERE those diffences are allowing you to pinpoint what exact groups are actually significantly different from each other.


anova.day <- aov(bizdaysdeliv ~ dayofweek, data = LT_datalite)
summary(anova.day)


model.tables(anova.day, "means")
TukeyHSD(anova.day)


anova.hour <- aov(bizdaysdeliv ~ Order_Entry_Hour_of_Day, data = LT_datalite)
summary(anova.hour)
model.tables(anova.hour, "means")
TukeyHSD(anova.hour)

anova.sscgroup <- aov(bizdaysdeliv ~ Wkly_Scorecard_LOB_SSC_Grouping, data = LT_datalite)
summary(anova.sscgroup)
model.tables(anova.sscgroup, "means")
TukeyHSD(anova.sscgroup)


anova.product <- aov(bizdaysdeliv ~ Product_Desc, data = LT_datalite)
summary(anova.product)


anova.ssccode <- aov(bizdaysdeliv ~ SSC_Code, data = LT_datalite)
summary(anova.ssccode)

anova.deliverypromise <- aov(bizdaysdeliv ~ Delivery_Promise_Flag, data = LT_datalite)
summary(anova.deliverypromise)

anova.directship <- aov(bizdaysdeliv ~ Direct_Ship_Flag, data = LT_datalite)
summary(anova.directship )

anova.shipcode <- aov(bizdaysdeliv ~ local_ship_code_desc, data = LT_datalite)
summary(anova.shipcode)

anova.state <- aov(bizdaysdeliv ~ ShipTo_State_Province, data = LT_datalite)
summary(anova.state)

anova.quote <- aov(bizdaysdeliv ~ QTE_SRC_APP_NM, data = LT_datalite)
summary(anova.quote)

anova.paycode <- aov(bizdaysdeliv ~ PAY_1_CD, data = LT_datalite)
summary(anova.paycode)

anova.purchasechannel <- aov(bizdaysdeliv ~ Purchase_Channel, data = LT_datalite)
summary(anova.purchasechannel)


anova.touch <- aov(bizdaysdeliv ~ second_touch_flag, data = LT_datalite)
summary(anova.touch)

anova.cfi <- aov(bizdaysdeliv ~ CFI_FLG, data = LT_datalite)
summary(anova.cfi)
TukeyHSD(anova.cfi)



# Evaluate Algorithms

# a) Test options and evaluation metric
# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method="repeatedcv", number=5, repeats=3)
metric <- "RMSE"

# b) Spot-Check Algorithms

# RF
set.seed(7)
fit.rf <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="rf", metric=metric, trControl=trainControl)


# LM
set.seed(7)
fit.lm <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="lm", metric=metric, trControl=trainControl)

summary(fit.glm)
# GLM
set.seed(7)
fit.glm <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="glm", metric=metric, trControl=trainControl)

# GLMNET
set.seed(7)
fit.glmnet <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="glmnet", metric=metric, trControl=trainControl)

# SVM
set.seed(7)
fit.svm <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="svmRadial", metric=metric, trControl=trainControl)

# CART
set.seed(7)
grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="rpart", metric=metric, tuneGrid=grid, trControl=trainControl)

# KNN
set.seed(7)
fit.knn <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="knn", metric=metric, trControl=trainControl)


# GBM Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="gbm", metric=metric,trControl=trainControl, verbose=FALSE)


# NB Stochastic Gradient Boosting
set.seed(7)
fit.nb <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset, method="nb", metric=metric,trControl=trainControl, verbose=FALSE)


# c) Compare Algorithms

results <- resamples(list(LM=fit.glm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm, CART=fit.cart, GBM=fit.gbm, KNN=fit.knn))
summary(results)
dotplot(results)
summary(fit.gbm)



# make predictions on the test dataset
predictions.lm <- predict(fit.lm, newdata = validation)
predictions.glm <- predict(fit.glm, newdata = validation)
predictions.glmnet <- predict(fit.glmnet, newdata = validation)
predictions.gbm <- predict(fit.gbm, newdata = validation)
predictions.cart <- predict(fit.cart, newdata = validation)
predictions.cart <- predict(fit.cart, newdata = validation)
predictions.svm <- predict(fit.svm, newdata = validation)
predictions.knn <- predict(fit.knn, newdata = validation)


# You can then add this column to the validationtable
validation$predict.lm <- round(predictions.lm) 
validation$predict.glm <- round(predictions.glm) 
validation$predict.glmnet <- round(predictions.glmnet) 
validation$predict.gbm <- round(predictions.gbm) 
validation$predict.cart <- round(predictions.cart) 
validation$predict.svm <- round(predictions.svm) 
validation$predict.knn <- round(predictions.knn)
validation$predict.gbmplus1 <- round(predictions.gbm) + 1
validation$predict.gbmplus2 <- round(predictions.gbm) + 2

# update window outcome for each model
validation$lm_deltadays <- validation$bizdaysdeliv - validation$predict.lm
validation$glm_deltadays <- validation$bizdaysdeliv - validation$predict.glm 
validation$glmnet_deltadays <- validation$bizdaysdeliv - validation$predict.glmnet 
validation$gbm_deltadays <-  validation$bizdaysdeliv - validation$predict.gbm 
validation$cart_deltadays <- validation$bizdaysdeliv - validation$predict.cart 
validation$svm_deltadays <- validation$bizdaysdeliv - validation$predict.svm 
validation$knn_deltadays <- validation$bizdaysdeliv - validation$predict.knn 
validation$gbmplus1_deltadays <-  validation$bizdaysdeliv - validation$predict.gbmplus1
validation$gbmplus2_deltadays <-  validation$bizdaysdeliv - validation$predict.gbmplus2

# Let's check window results
prop.table(table(validation$DeliveryOutcomeWindowRange)) # actual performance to window range
prop.table(table(validation$lm.windowrange))  # lm model performance to window range
prop.table(table(validation$glm.windowrange))  # glm model performance to window range
prop.table(table(validation$glmnet.windowrange))  # glmnet model performance to window range
prop.table(table(validation$gbm.windowrange))  # gbm model performance to window range
prop.table(table(validation$cart.windowrange))  # cart model performance to window range
prop.table(table(validation$svm.windowrange))  # svm model performance to window range
prop.table(table(validation$knn.windowrange))  # knn model performance to window range
prop.table(table(validation$gbmplus1.windowrange))  # gbm model + 1 day performance to window range
prop.table(table(validation$gbmplus2.windowrange))  # gbm model + 2 days performance to window range

# Improving Accuracy: GBM Algorithm Tuning

# GBM on training set to determine sample size

gbmGrid <-  expand.grid(n.trees = 100, interaction.depth = 10,shrinkage = 0.05, n.minobsinnode = 10)
                       


set.seed(255)
fit.gbm<- train(bizdaysdeliv ~ Mfg_Lead_Time + LCL_ORD_SRC_CD + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM, data = datasetsample, method="gbm",tuneGrid = gbmGrid,trControl=trainControl, verbose=FALSE) 

predictions.gbm <- predict(fit.gbm, newdata = validation)
validation$predict.gbm_plus1 <- round(predictions.gbm)+1 
# validation$predict.gbmsample <- round(predictions.gbmsample)
validation$gbm_plus1_deltadays <-  validation$bizdaysdeliv - validation$predict.gbm_plus1


validation$gbm_plus1.windowrange <- 
ifelse(validation$predict.gbm_plus1 <= 2 & validation$gbm_plus1_deltadays <= -1, "EARLY",
ifelse(validation$predict.gbm_plus1 <= 2 & validation$gbm_plus1_deltadays == 0, "ONTIME",
ifelse(validation$predict.gbm_plus1 <= 2 & validation$gbm_plus1_deltadays >= 1, "LATE",
ifelse(validation$predict.gbm_plus1 >= 3 &  validation$predict.gbm_plus1 <= 4 & validation$gbm_plus1_deltadays <= -2, "EARLY",
ifelse(validation$predict.gbm_plus1 >= 3 &  validation$predict.gbm_plus1 <= 4 & validation$gbm_plus1_deltadays <= 0 & validation$gbm_plus1_deltadays >= -1, "ONTIME",
ifelse(validation$predict.gbm_plus1 >= 3 &  validation$predict.gbm_plus1 <= 4 & validation$gbm_plus1_deltadays >= 1, "LATE",  
ifelse(validation$predict.gbm_plus1 >= 5 &  validation$predict.gbm_plus1 <= 6 & validation$gbm_plus1_deltadays <= -3, "EARLY",
ifelse(validation$predict.gbm_plus1 >= 5 &  validation$predict.gbm_plus1 <= 6 & validation$gbm_plus1_deltadays <= 0 & validation$gbm_plus1_deltadays >= -2, "ONTIME",
ifelse(validation$predict.gbm_plus1 >= 5 &  validation$predict.gbm_plus1 <= 6 & validation$gbm_plus1_deltadays >= 1, "LATE",  
ifelse(validation$predict.gbm_plus1 >= 7 &  validation$predict.gbm_plus1 <= 8 & validation$gbm_plus1_deltadays <= -4, "EARLY",
ifelse(validation$predict.gbm_plus1 >= 7 &  validation$predict.gbm_plus1 <= 8 & validation$gbm_plus1_deltadays <= 0 & validation$gbm_plus1_deltadays >= -3, "ONTIME",
ifelse(validation$predict.gbm_plus1 >= 7 &  validation$predict.gbm_plus1 <= 8 & validation$gbm_plus1_deltadays >= 1, "LATE",  
ifelse(validation$predict.gbm_plus1 >= 9 &  validation$predict.gbm_plus1 <= 10 & validation$gbm_plus1_deltadays <= -5, "EARLY",
ifelse(validation$predict.gbm_plus1 >= 9 &  validation$predict.gbm_plus1 <= 10 & validation$gbm_plus1_deltadays <= 0 & validation$gbm_plus1_deltadays >= -4, "ONTIME",
ifelse(validation$predict.gbm_plus1 >= 9 &  validation$predict.gbm_plus1 <= 10 & validation$gbm_plus1_deltadays >= 1, "LATE",  
ifelse(validation$predict.gbm_plus1 >= 11 &  validation$predict.gbm_plus1 <= 12 & validation$gbm_plus1_deltadays <= -6, "EARLY",
ifelse(validation$predict.gbm_plus1 >= 11 &  validation$predict.gbm_plus1 <= 12 & validation$gbm_plus1_deltadays <= 0 & validation$gbm_plus1_deltadays >= -5, "ONTIME",
ifelse(validation$predict.gbm_plus1 >= 11 &  validation$predict.gbm_plus1 <= 12 & validation$gbm_plus1_deltadays >= 1, "LATE",
ifelse(validation$predict.gbm_plus1 >= 13 & validation$gbm_plus1_deltadays <= -7, "EARLY",
ifelse(validation$predict.gbm_plus1 >= 13 & validation$gbm_plus1_deltadays <= 0 & validation$gbm_plus1_deltadays >= -6, "ONTIME",
ifelse(validation$predict.gbm_plus1 >= 13 & validation$gbm_plus1_deltadays >= 1, "LATE",
NA  )))))))))))))))))))))

prop.table(table(validation$gbm_plus1.windowrange))  # gbm model performance to window range


prop.table(table(validation$DeliveryOutcomeWindowRange)) # actual performance to window range
prop.table(table(validation$gbm.windowrange))  # gbm model performance to window range


write.csv(validation, "validation.csv")




# Let's do the same now for BTS & BTO
bts <- subset(LTdata, Wkly_Scorecard_LOB_SSC_Grouping == "Client BTS")
bto <- subset(LTdata, Wkly_Scorecard_LOB_SSC_Grouping == "Client BTO")

set.seed(7)
validationIndex_bts <- createDataPartition(bts$bizdaysdeliv, p=0.8, list=FALSE)
# select 20% of the data for validation
validation_bts <- bts[-validationIndex_bts,]
# use the remaining 80% of data to training and testing the models
dataset_bts <- bts[validationIndex_bts,]
# Let's now subset the training data
validationIndexTraining_bts <- createDataPartition(dataset_bts$bizdaysdeliv, p=.2, list=FALSE)
dataset_bts <- dataset_bts[validationIndexTraining_bts,]


# BTS Spot-Check Algorithms

# RF
set.seed(7)
fit.rf_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="rf", metric=metric, trControl=trainControl)


# LM
set.seed(7)
fit.lm_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="lm", metric=metric, trControl=trainControl)

# GLM
set.seed(7)
fit.glm_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="glm", metric=metric, trControl=trainControl)

# GLMNET
set.seed(7)
fit.glmnet_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="glmnet", metric=metric, trControl=trainControl)

# SVM
set.seed(7)
fit.svm_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="svmRadial", metric=metric, trControl=trainControl)

# CART
set.seed(7)
grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="rpart", metric=metric, tuneGrid=grid, trControl=trainControl)

# KNN
set.seed(7)
fit.knn_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="knn", metric=metric, trControl=trainControl)


# GBM Stochastic Gradient Boosting
set.seed(7)
fit.gbm_bts <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bts, method="gbm", metric=metric,trControl=trainControl, verbose=FALSE)



# Compare BTS Algorithms

results_bts <- resamples(list(LM=fit.lm_bts, GLM=fit.glm_bts, GLMNET=fit.glmnet_bts, CART=fit.cart_bts, GBM=fit.gbm_bts))
summary(results_bts)
dotplot(results_bts)


# BTO check and algorithm compares
  
set.seed(7)
validationIndex_bto <- createDataPartition(bto$bizdaysdeliv, p=0.8, list=FALSE)
# select 20% of the data for validation
validation_bto <- bto[-validationIndex_bto,]
# use the remaining 80% of data to training and testing the models
dataset_bto <- bto[validationIndex_bto,]
# Let's now subset the training data
validationIndexTraining_bto <- createDataPartition(dataset_bto$bizdaysdeliv, p=.1, list=FALSE)
dataset_bto <- dataset_bto[validationIndexTraining_bto,]

# Spot-Check Algorithms
  
  # RF
  set.seed(7)
fit.rf_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="rf", metric=metric, trControl=trainControl)


# LM
set.seed(7)
fit.lm_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="lm", metric=metric, trControl=trainControl)

# GLM
set.seed(7)
fit.glm_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="glm", metric=metric, trControl=trainControl)

# GLMNET
set.seed(7)
fit.glmnet_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="glmnet", metric=metric, trControl=trainControl)

# SVM
set.seed(7)
fit.svm_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="svmRadial", metric=metric, trControl=trainControl)

# CART
set.seed(7)
grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="rpart", metric=metric, tuneGrid=grid, trControl=trainControl)

# KNN
set.seed(7)
fit.knn_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="knn", metric=metric, trControl=trainControl)


# GBM Stochastic Gradient Boosting
set.seed(7)
fit.gbm_bto <- train(bizdaysdeliv ~ bizdaystoEDD + LCL_ORD_SRC_CD +  Mfg_Lead_Time + Pay_Lead_Time + Freight_Lead_Time + dayofweek +  SDS_NBD_Flag + Sys_Qty + order_revenue_amt + Delivery_Promise_Flag + Wkly_Scorecard_LOB_SSC_Grouping + Product_Desc + SSC_Code + Src_Channel + local_ship_code_desc + CS_Flag +  Warehousing_Flag + second_touch_flag + Large_Order_Flag + CFI_FLG + Merge_Facility +  Build_Facility + Direct_Ship_Flag + Purchase_Channel + QTE_SRC_APP_NM + Payment_Type, data=dataset_bto, method="gbm", metric=metric,trControl=trainControl, verbose=FALSE)



# Compare Algorithms

results_bto <- resamples(list(LM=fit.lm_bto, GLM=fit.glm_bto, GLMNET=fit.glmnet_bto, CART=fit.cart_bto, GBM=fit.gbm_bto))
summary(results_bto)
dotplot(results_bto)

------------------------------------------------------------------------------
# Appendix items
# clustering data; # create dataset of only wanted columns
library(cluster)
library(ggplot2)
library(ggplot)
library(ggdendro) 

clusterdata <- datasetsample[c(8,9,10,13,14,15,17,19,20,21,22, 25, 26, 27,36,41, 48)]
clustersbig <- sample_n(clusterdata,10000 ) # using dplyr

distance <- daisy(clustersbig,metric="gower",stand=FALSE) 
cluster_big <- hclust(distance, method="ward")
plot(cluster_big)


hc <- hclust(distance, "ave")
ggdendrogram(hc, rotate = FALSE, size = 2)

model <- hclust(distance, "ave")
dhc <- as.dendrogram(model)
# Rectangular lines
ddata <- dendro_data(dhc, type = "rectangle")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))
p



cluster_cut <- cutree(cluster_big, k = 5)




require(graphics)
hc <- hclust(distance^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(distance[memb == k, , drop = FALSE]))
}
hc1 <- hclust(distance^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)



fit <- cluster_big
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# using dendrogram objects
hcd = as.dendrogram(cluster_cut)
# alternative way to get a dendrogram
plot(cluster_cut)


require(graphics)
hc <- hclust(distance^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(distance[memb == , , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)





write.csv(clustersbig, "clusterbig.csv")


library(cluster)
k.max <- 15
data <- distance
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

# The function fviz_nbclust() [in factoextra package] can be also used. It just requires the cluster package to be installed:
  
require(cluster)
fviz_nbclust(iris.scaled, kmeans, method = "silhouette")


# determine number of clusters
silhouette(x, ...)
## Default S3 method:
silhouette(cluster_big) #, dmatrix, ...)
## S3 method for class 'partition'
silhouette(x, ...)
## S3 method for class 'clara'
silhouette(cluster_big, full = FALSE)
