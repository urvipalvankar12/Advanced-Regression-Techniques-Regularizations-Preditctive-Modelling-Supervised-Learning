library(readxl)
library(caret)
library(dplyr)
library(writexl)
library(hexbin)
library(glmnet)
library(corrplot)
library(plyr)
library(scales)
library(moments)
library(Metrics)

###
#Reading files and checking data summary
###

#Importing data files - dropping factor levels
train<-read.csv("C:\\Users\\urvipalvankar\\Urvi\\Master of Management Analytics\\867 - Predictive Modelling\\Assignment 1\\train.csv",sep = ',',stringsAsFactors = FALSE)
test<-read.csv("C:\\Users\\urvipalvankar\\Urvi\\Master of Management Analytics\\867 - Predictive Modelling\\Assignment 1\\test.csv",sep = ',',stringsAsFactors = FALSE)

#Checking summary of the data
dim(train)
dim(test)

str(train[,c(1:10,81)])
str(test[,c(1:10,80)])

#Combining the train and test dataset
train_final<-select(train,-SalePrice)
combined_data<-rbind(train_final,test)

###
#Determining correlation between all variables and sales
###

#converting variables into numeric form (train dataset only)
numeric_variables <- which(sapply(train, is.numeric))
numeric_variables <- train[,numeric_variables]

#Correlations of all numeric variables (train dataset only)
cor_numVar <- cor(numeric_variables, use="pairwise.complete.obs") 

#Sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#Selecting only correlations above 0.5
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

#Plotting correlation
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt",number.cex=0.7)

###
#Dealing with missing values
###

missing_data_columnwise<-sort(colSums(is.na(combined_data)),decreasing = TRUE)

#Pool QC - data description says NA means none and label encoding
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined_data$PoolQC[is.na(combined_data$PoolQC)]<-'None'
combined_data$PoolQC<-as.integer(revalue(combined_data$PoolQC, Qualities))

#MiscFeature - data description says NA means none
combined_data$MiscFeature[is.na(combined_data$MiscFeature)]<-'None'

#Alley - data description says NA means none
combined_data$Alley[is.na(combined_data$Alley)]<-'None'

#Fence - data description says NA means none
combined_data$Fence[is.na(combined_data$Fence)]<-'None'

#Fireplace - data description says NA means none and label encoding
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
combined_data$FireplaceQu[is.na(combined_data$FireplaceQu)]<-'None'
combined_data$FireplaceQu<-as.integer(revalue(combined_data$FireplaceQu, Qualities))

#Garage - data description says NA means none and label encoding
combined_data$GarageType[is.na(combined_data$GarageType)]<-'None'

Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
combined_data$GarageFinish[is.na(combined_data$GarageFinish)]<-'None'
combined_data$GarageFinish<-as.integer(revalue(combined_data$GarageFinish, Finish))

combined_data$GarageQual[is.na(combined_data$GarageQual)]<-'None'    
combined_data$GarageQual<-as.integer(revalue(combined_data$GarageQual, Qualities))

combined_data$GarageCond[is.na(combined_data$GarageCond)]<-'None'    
combined_data$GarageCond<-as.integer(revalue(combined_data$GarageCond, Qualities))

#Garage - data description says No garage means 0 years,cars or area
combined_data$GarageYrBlt[is.na(combined_data$GarageYrBlt)] <- combined_data$YearBuilt[is.na(combined_data$GarageYrBlt)]
combined_data$GarageCars[is.na(combined_data$GarageCars)]<-0
combined_data$GarageArea[is.na(combined_data$GarageArea)]<-0

#Basement - data description says no basement means none of the below will have value
combined_data$BsmtFinSF1[is.na(combined_data$BsmtFinSF1)]<-0
combined_data$BsmtFinSF2[is.na(combined_data$BsmtFinSF2)]<-0
combined_data$BsmtUnfSF[is.na(combined_data$BsmtUnfSF)]<-0
combined_data$TotalBsmtSF[is.na(combined_data$TotalBsmtSF)]<-0
combined_data$BsmtFullBath[is.na(combined_data$BsmtFullBath)]<-0
combined_data$BsmtHalfBath[is.na(combined_data$BsmtHalfBath)]<-0

#Basement - data description says NA means none and label encoding
combined_data$BsmtQual[is.na(combined_data$BsmtQual)]<-'None'
combined_data$BsmtQual<-as.integer(revalue(combined_data$BsmtQual, Qualities))

combined_data$BsmtCond[is.na(combined_data$BsmtCond)]<-'None'
combined_data$BsmtCond<-as.integer(revalue(combined_data$BsmtCond, Qualities))

Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
combined_data$BsmtExposure[is.na(combined_data$BsmtExposur)]<-'None'
combined_data$BsmtExposure<-as.integer(revalue(combined_data$BsmtExposure, Exposure))

FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
combined_data$BsmtFinType1[is.na(combined_data$BsmtFinType1)]<-'None'
combined_data$BsmtFinType1<-as.integer(revalue(combined_data$BsmtFinType1, FinType))

combined_data$BsmtFinType2[is.na(combined_data$BsmtFinType2)]<-'None'
combined_data$BsmtFinType2<-as.integer(revalue(combined_data$BsmtFinType2, FinType))

#MasVnrType - data description says NA means none
combined_data$MasVnrArea[is.na(combined_data$MasVnrArea)]<-0

combined_data$MasVnrType[is.na(combined_data$MasVnrType)]<-'None'

#Functional - data description says NA means typical and label encoding
Functionality<-c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)
combined_data$Functional[is.na(combined_data$Functional)]<-'Typ'
combined_data$Functional<-as.integer(revalue(combined_data$Functional, Functionality))

#Replacing lot frontage by median value per neighbourhood
for (i in 1:nrow(combined_data)){
  if(is.na(combined_data$LotFrontage[i])){
    combined_data$LotFrontage[i] <- as.integer(median(combined_data$LotFrontage[combined_data$Neighborhood==combined_data$Neighborhood[i]], na.rm=TRUE)) 
  }
}

#Label Encoding - Lot Shape
combined_data$LotShape<-as.integer(revalue(combined_data$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))

#MS Zoning - Replacing missing values with most common values in dataset - eg. RL
combined_data$MSZoning[is.na(combined_data$MSZoning)]<-'RL'

#Extrior 1st and 2nd - Replacing missing values with most common values in dataset
combined_data$Exterior1st[is.na(combined_data$Exterior1st)]<-'VinylSd'

combined_data$Exterior2nd[is.na(combined_data$Exterior2nd)]<-'VinylSd'

#Exterior qual and cond - label encoding
combined_data$ExterQual<-as.integer(revalue(combined_data$ExterQual, Qualities))
combined_data$ExterCond<-as.integer(revalue(combined_data$ExterCond, Qualities))

#Electrical   - Replacing missing values with most common values in dataset
combined_data$Electrical[is.na(combined_data$Electrical)]<-'SBrkr'

#KitchenQual   - Replacing missing values with most common values in dataset and and label encoding
combined_data$KitchenQual[is.na(combined_data$KitchenQual)]<-'TA'
combined_data$KitchenQual<- as.integer(revalue(combined_data$KitchenQual,Qualities))

#SaleType   - Replacing missing values with most common values in dataset
combined_data$SaleType[is.na(combined_data$SaleType)]<-'WD'

#Utilities   - Replacing missing values with most common values in dataset
combined_data$Utilities[is.na(combined_data$Utilities)]<-'AllPub'

###
#Dealing with other categorical variables
###

#streets - label encoding
combined_data$Street<-as.integer(revalue(combined_data$Street, c('Grvl'=0, 'Pave'=1)))

#Land Slope - label encoding
combined_data$LandSlope<-as.integer(revalue(combined_data$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))

#HeatingQC - label encoding
combined_data$HeatingQC<-as.integer(revalue(combined_data$HeatingQC, Qualities))

#Central Air - label encoding
combined_data$CentralAir<-as.integer(revalue(combined_data$CentralAir, c('N'=0, 'Y'=1)))

#Paved Drive - label encoding
Paved<-c('N'=0, 'P'=1, 'Y'=2)
combined_data$PavedDrive<-as.integer(revalue(combined_data$PavedDrive, Paved))

###
#Changing some numerical variables
###

#Month Sold is also an Integer variable. However, December is not "better" than January. Therefore, I will convert MoSold values back into factors.
combined_data$MoSold <- as.factor(combined_data$MoSold)

#MSSubclass - label encoding
combined_data$MSSubClass <- as.factor(combined_data$MSSubClass)
combined_data$MSSubClass<-revalue(combined_data$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))


###
#Feature Engineering
###

#Total no. of bathrooms
combined_data$TotBathrooms <- combined_data$FullBath + (combined_data$HalfBath*0.5) + combined_data$BsmtFullBath + (combined_data$BsmtHalfBath*0.5)

#Age of the house
combined_data$Remod <- ifelse(combined_data$YearBuilt==combined_data$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
combined_data$Age <- as.numeric(combined_data$YrSold)-combined_data$YearRemodAdd
combined_data$YrSold<-as.factor(combined_data$YrSold)

#Total SqFeet
combined_data$TotalSqFeet <- combined_data$GrLivArea + combined_data$TotalBsmtSF

#Porch Variables
combined_data$TotalPorchSF <- combined_data$OpenPorchSF + combined_data$EnclosedPorch + combined_data$X3SsnPorch + combined_data$ScreenPorch

####
#Converting all character variables into factor variables + few numerical varibales into factor variables
###

#removing utilities variable
combined_data<-select(combined_data,-Utilities)

#converting variables below to factor variables
combined_data$GarageYrBlt<-as.factor(combined_data$GarageYrBlt)
combined_data$GarageCond<-as.factor(combined_data$GarageCond)
combined_data$TotRmsAbvGrd<-as.factor(combined_data$TotRmsAbvGrd)

combined_data$GarageFinish<-as.factor(combined_data$GarageFinish)
combined_data$GarageQual<-as.factor(combined_data$GarageQual)
combined_data$BsmtQual<-as.factor(combined_data$BsmtQual)
combined_data$BsmtCond<-as.factor(combined_data$BsmtCond)
combined_data$BsmtExposure<-as.factor(combined_data$BsmtExposure)
combined_data$BsmtFinType1<-as.factor(combined_data$BsmtFinType1)
combined_data$BsmtFinType2<-as.factor(combined_data$BsmtFinType2)
combined_data$Functional<-as.factor(combined_data$Functional)
combined_data$LotShape<-as.factor(combined_data$LotShape)
combined_data$ExterQual<-as.factor(combined_data$ExterQual)
combined_data$ExterCond<-as.factor(combined_data$ExterCond)
combined_data$KitchenQual<-as.factor(combined_data$KitchenQual)
combined_data$Street<-as.factor(combined_data$Street)
combined_data$LandSlope<-as.factor(combined_data$LandSlope)
combined_data$HeatingQC<-as.factor(combined_data$HeatingQC)
combined_data$CentralAir<-as.factor(combined_data$CentralAir)
combined_data$PavedDrive<-as.factor(combined_data$PavedDrive)
combined_data$MSSubClass<-as.factor(combined_data$MSSubClass)
combined_data$OverallQual<-as.factor(combined_data$OverallQual)
combined_data$OverallCond<-as.factor(combined_data$OverallCond)
combined_data$FireplaceQu<-as.factor(combined_data$FireplaceQu)
combined_data$PoolQC<-as.factor(combined_data$PoolQC)
combined_data$Remod<-as.factor(combined_data$Remod)
combined_data$YrSold<-as.factor(combined_data$YrSold)
combined_data$LotConfig <- as.factor(combined_data$LotConfig)
combined_data$MiscFeature<-as.factor(combined_data$MiscFeature)
combined_data$Alley<-as.factor(combined_data$Alley)
combined_data$Fence<-as.factor(combined_data$Fence)
combined_data$GarageType<-as.factor(combined_data$GarageType)
combined_data$MasVnrType<-as.factor(combined_data$MasVnrType)
combined_data$MSZoning<-as.factor(combined_data$MSZoning)
combined_data$Exterior1st<- as.factor(combined_data$Exterior1st)
combined_data$Exterior2nd<- as.factor(combined_data$Exterior2nd)
combined_data$Electrical<- as.factor(combined_data$Electrical)
combined_data$SaleType<- as.factor(combined_data$SaleType)
combined_data$LandContour <- as.factor(combined_data$LandContour)
combined_data$Neighborhood <- as.factor(combined_data$Neighborhood)
combined_data$Condition1 <- as.factor(combined_data$Condition1)
combined_data$Condition2 <- as.factor(combined_data$Condition2)
combined_data$BldgType <- as.factor(combined_data$BldgType)
combined_data$HouseStyle <- as.factor(combined_data$HouseStyle)
combined_data$RoofStyle <- as.factor(combined_data$RoofStyle)
combined_data$RoofMatl <- as.factor(combined_data$RoofMatl)
combined_data$Foundation <- as.factor(combined_data$Foundation)
combined_data$Heating <- as.factor(combined_data$Heating)
combined_data$SaleCondition<-as.factor(combined_data$SaleCondition)

#check count of numeric and category varibales
num_variables <- which(sapply(combined_data, is.numeric)) #index vector numeric variables
factor_variables <- which(sapply(combined_data, is.factor)) #index vector factor variables
cat('There are', length(num_variables), 'numeric variables, and', length(factor_variables), 'categoric variables')

#separating numeric and categorical variables
numeric_combined_data <- combined_data[, names(num_variables)]
factor_combined_data <- combined_data[, names(factor_variables)]

dim(numeric_combined_data)
dim(factor_combined_data)


###
#sales price analysis 
###

#The target variable is right skewed. 
#As (linear) models love normally distributed data , need to transform this variable and make it more normally distributed

#Histogram of Sale price
mu<-mean(train$SalePrice)
sigma<-sqrt(var(train$SalePrice))
hist(train$SalePrice,freq=F, breaks=30)
curve(dnorm(x, mean=mu, sd=sigma), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Histogram of Log of Sale Price
mu1<-mean(log(train$SalePrice))
sigma1<-sqrt(var(log(train$SalePrice)))
hist(log(train$SalePrice), freq=F, breaks=30)
curve(dnorm(x, mean=mu1, sd=sigma1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


###
#Taking log form of numeric data and normalizing the data
###

for(i in 1:ncol(numeric_combined_data[,])){
  if (abs(skewness(numeric_combined_data[,i]))>0.75){
    numeric_combined_data[,i] <- log(numeric_combined_data[,i] +1)
  }
}

Norm_Data <- preProcess(numeric_combined_data, method=c("center", "scale"))
print(Norm_Data)

combined_num_norm <- predict(Norm_Data, numeric_combined_data)
combined_num_norm<-combined_num_norm[,-1]
combined_num_norm<-cbind(combined_data$Id,combined_num_norm)
names(combined_num_norm)[1] = "Id"

dim(combined_num_norm)

###
#Removing a few levels from character variables
###

#Matrix building for character variables
combined_chr<-as.data.frame(model.matrix(~.-1,factor_combined_data))

#removing character varibales absent in test set
combined_chr<-combined_chr[,-which(names(combined_chr) %in% c('Condition2RRAe','Condition2RRAn','Condition2RRNn','HouseStyle2.5Fin','RoofMatlMembran','RoofMatlMetal','RoofMatlRoll','Exterior1stImStucc','Exterior1stStone','Exterior2ndOther','HeatingOthW','ElectricalMix','MiscFeatureTenC'))]

#removing character varibales which are absent in train set
combined_chr<-combined_chr[,-which(names(combined_chr) %in% c("MSSubClass1,5 story PUD all"))]

###
#Deleting Outliers from all dataset variables
###

combined_data <- combined_data[-c(524, 1299),]
train<-train[-c(524, 1299),]
combined_chr<-combined_chr[-c(524, 1299),]
combined_num_norm<-combined_num_norm[-c(524, 1299),]

###
#Train and Test data
###

#Splitting Prediction dataset with train and test dataset
train_test<-cbind(combined_num_norm,combined_chr)
train_1<-filter(train_test,train_test$Id<1461)
prediction_final<-filter(train_test,train_test$Id>1460)

#Train and test split without Sale Price variable
#train_split<-filter(train_1,train_1$Id<1461)
train_split<-filter(train_1,train_1$Id<1241)
test_split<- filter(train_1,train_1$Id>=1241)

#Train and test split with Sale Price variable
train_split_sale<-cbind(train_1,train$SalePrice)
#train_split_sale<-filter(train_split_sale,train_split_sale$Id<1461)
train_split_sale<-filter(train_split_sale,train_split_sale$Id<1241)
names(train_split_sale)[415] = "SalePrice"

test_split_sale<-cbind(train_1,train$SalePrice)
test_split_sale<-filter(test_split_sale,test_split_sale$Id>=1241)
names(test_split_sale)[415] = "SalePrice"

###
#Running Simple Linear Regression
###
reg<-lm(log(SalePrice)~.,train_split_sale)
summary(reg)
plot(reg)

#Predicting House Price on test datset
pred<-exp(predict(reg,test_split))

#Calculating MAPE
error<-abs((pred-test_split_sale$SalePrice)/test_split_sale$SalePrice)*100#calculate and display MAPE
mean(error)


###
#separarting train and test for regularization models
###

#Converting train and test into matrices
x<-model.matrix(~.-1,train_test)
y<-log(train_split_sale$SalePrice)   
dim(x)

# split X into testing, trainig/holdout and prediction as before
#X_training<-subset(x,(x[,1]<1461))
X_training<-subset(x,(x[,1]<1241))
X_testing<-subset(x, (x[,1]>=1241 & x[,1]<=1460))
X_prediction<-subset(x,(x[,1]>1460))

dim(X_training)
dim(X_testing)
dim(X_prediction)

###
#Running regularisation models
###

#Running Lasso Model
lasso_fit<-glmnet(x = X_training, y = y, alpha = 1)
plot(lasso_fit, xvar = "lambda")

#selecting the best penalty lambda
#Crossvalidating the lasso model to select the best optimal lambda
#determine optimal penalty parameter, lambda
crossval<-cv.glmnet(x = X_training, y = y, alpha = 1) 
plot(crossval)
penalty_lasso <- crossval$lambda.min 
log(penalty_lasso) #see where it was on the graph

#running regularilization with lasso with penalty lambda (this lamda is the one we selected) 
#First estimate the model with the optimal penalty
#Then view which of the model coefficients are selected
lasso_opt_fit <-glmnet(x = X_training, y = y, alpha = 1, lambda = penalty_lasso) 
coef(lasso_opt_fit)

# Predicting the House Sale Price on testing set
lasso_testing <- exp(predict(lasso_opt_fit, s = penalty_lasso, newx =X_testing))

#Calculating MAPE
mean(abs((lasso_testing-test_split_sale$SalePrice)/test_split_sale$SalePrice)*100) #calculate and display MAPE

#RMSLE value 
rmsle(test_split_sale$SalePrice,lasso_testing)

#Prediction v/s Actual House Price view
head(lasso_testing,5)
head(test_split_sale$SalePrice,5)

#Predicting sale price for predicted data (i.e Test Dataset of kaggle)
predictions <- exp(predict(lasso_opt_fit, s = penalty_lasso, newx =X_prediction))

#Exporting predictions into CSV file
write.csv(predictions,"C:\\Users\\urvipalvankar\\Urvi\\predictions.csv")

####################------------------------######################


