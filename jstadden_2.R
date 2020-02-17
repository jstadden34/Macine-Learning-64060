#Jared Stadden
#Assignment 2

#Read data into R
bank = read.csv("C:\\Users\\jared\\Desktop\\UniversalBank.csv")

#loading libraries
library(caret)
library(ISLR)
library(class)

#transforming some variables to facor
bank$Education <- factor(bank$Education)
bank$Personal.Loan <- factor(bank$Personal.Loan)

#turning education into dummay variables
dummy_temp <- predict(dummyVars(~Education, data = bank), bank)
bank_dummy <- cbind(bank,dummy_temp)
#removing unnecessary variables
bank_dummy <- bank_dummy[,-c(1,5,8)]

#normalizing data
norm_model<-preProcess(bank_dummy,method = c('range'))
bank_normalized <- predict(norm_model,bank_dummy)

#splitting into train and test sets
Index_Train<-createDataPartition(bank_normalized$Personal.Loan, p=0.6, list = FALSE)
Train<-bank_normalized[Index_Train,]
Validation<-bank_normalized[-Index_Train,]

#setting predictor and target vairables
Train_Predictors <- Train[,c(1:6,8:14)]
Validation_Predictors <- Validation[,c(1:6,8:14)]

Train_labels <- Train[,7]
Validation_labels <- Validation[,7]


#creating prediction dataframe
predict1 <- data.frame("Age"=40, "Experience"=10, "Income"=84, "Family"=2,
                       "CCAvg"=2,"Mortgage"=0,"Securities.Account"=0,"CD.Account"=0,
                       "Online"=1,"CreditCard"=1,"Education.1"=0,"Education.2"=1,
                       "Education.3"=0)

#normalizing prediction values 
predict1_normalized <- predict(norm_model,predict1)
#prediction customer with given attributes
Predicted_Validation_labels <- knn(Train_Predictors,predict1_normalized,cl=Train_labels,k=1)
head(Predicted_Validation_labels)

#finding optimal k
set.seed(123)
Search_grid <-expand.grid(k=c(2,7,9,15))
model <- train(Personal.Loan~Age+Experience+Income+Family+CCAvg+Mortgage+
                 Securities.Account+CD.Account+Online+CreditCard+Education.1+
                 Education.2+Education.3,data=bank_normalized,method="knn"
               ,tuneGrid=Search_grid,preProcess='range')
model


#Set k from above
Predicted_Validation_labels <- knn(Train_Predictors,Validation_Predictors,cl=Train_labels,k=2)
head(Predicted_Validation_labels)

#creating confusion matrix
library("gmodels")
CrossTable(x=Validation_labels,y=Predicted_Validation_labels,prop.chisq = FALSE)


#Set k from above
Predicted_Validation_labels <- knn(Train_Predictors,predict1_normalized,cl=Train_labels,k=2)
head(Predicted_Validation_labels)

#splitting data into train, validate, and test sets
Index_Train2<-createDataPartition(bank_normalized$Personal.Loan, p=0.5, list = FALSE)
Train2<-bank_normalized[Index_Train2,]
temp1<-bank_normalized[-Index_Train2,]
Index_Train3<-createDataPartition(temp1$Personal.Loan, p=0.6, list = FALSE)
Validation2 <- temp1[Index_Train3,]
Test2 <- temp1[-Index_Train3,]

#setting prediction and target variables
Train_Predictors2 <- Train2[,c(1:6,8:14)]
Validation_Predictors2 <- Validation2[,c(1:6,8:14)]
Test_Predictors2 <- Test2[,c(1:6,8:14)]

Train_labels2 <- Train2[,7]
Validation_labels2 <- Validation2[,7]
Test_labels2 <- Test2[,7]

#Set K from above
Predicted_Test_labels2 <- knn(Train_Predictors2,Test_Predictors2,cl=Train_labels2,k=2)
head(Predicted_Test_labels2)
#confusion matrix
CrossTable(x=Test_labels2,y=Predicted_Test_labels2,prop.chisq = FALSE)

#Set K from above
Predicted_Test_labels3 <- knn(Train_Predictors2,Validation_Predictors2,cl=Train_labels2,k=2)
head(Predicted_Test_labels2)
#confusion matrix
CrossTable(x=Validation_labels2,y=Predicted_Test_labels3,prop.chisq = FALSE)









