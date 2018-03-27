#Imported datasets
train <- read.csv(file = "C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Loan Prediction/train_u6lujuX_CVtuZ9i.csv", na.strings=c("","NA"), header = TRUE, stringsAsFactors = FALSE)
test <- read.csv(file = "C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Loan Prediction/test_Y3wMUE5_7gLdaTN.csv", na.strings=c("","NA"), header = TRUE, stringsAsFactors = FALSE)

#Concatenation of train & test dataset
test$Loan_Status <- NA
data <- rbind(train,test)

#View the column names/summary of the dataset
colnames(data)

#Finding NA's in each column
sapply(data, function(x) sum(length(which(is.na(x))))) 

#Imputting missing values
mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
data$Gender[is.na(data$Gender)] <- mode(data$Gender, na.rm = TRUE)
sum(is.na(data$Gender))

data$Married[is.na(data$Married)] <- mode(data$Married, na.rm = TRUE)
sum(is.na(data$Married))

data$Dependents[is.na(data$Dependents)] <- mode(data$Dependents, na.rm = TRUE)
sum(is.na(data$Dependents))

data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] <- mode(data$Loan_Amount_Term, na.rm = TRUE)
sum(is.na(data$Loan_Amount_Term))

data$Self_Employed[is.na(data$Self_Employed)] <- mode(data$Self_Employed, na.rm = TRUE)
sum(is.na(data$Self_Employed))

data$Credit_History[is.na(data$Credit_History)] <- mode(data$Credit_History, na.rm = TRUE)
sum(is.na(data$Credit_History))

data$LoanAmount[is.na(data$LoanAmount)] <- mean(data$LoanAmount, na.rm = TRUE)
sum(is.na(data$LoanAmount))

data$Dependents[data$Dependents == "3+"] <- 3

#R code for categorical variable(converting as factor variable)
dataset <- data
sapply(dataset,class)
dataset[c("Loan_ID","Gender","Married","Dependents","Education","Self_Employed","Loan_Amount_Term","Credit_History","Property_Area","Loan_Status")] <- lapply(dataset[c("Loan_ID","Gender","Married","Dependents","Education","Self_Employed","Loan_Amount_Term","Credit_History","Property_Area","Loan_Status")],factor)
dataset$LoanAmount <- round(dataset$LoanAmount)
dataset$ApplicantIncome <- as.numeric(dataset$ApplicantIncome)

#Slicing dataset back to train & test
testset <- tail(dataset, -614)
trainset <- head(dataset, -367)

#Drop unnecessary columns
testset$Loan_Status <- NULL

#Exporting modified train & test dataset
write.csv(trainset, file="C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Loan Prediction/trainset.csv", row.names = FALSE)
write.csv(testset, file="C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Loan Prediction/testset.csv", row.names = FALSE)

#Modelling using logistic regression
model <- trainset
model$Loan_ID <- NULL
fit <- glm(Loan_Status~Married + CoapplicantIncome + Credit_History,data=model,family=binomial(logit),maxit =100)
summary(fit)

library(MASS)
fit_model <- stepAIC(fit,direction="both")
ls(fit_model)

library(plyr)
count(model,"Property_Area")

source("C:/Users/Gopal/Desktop/Materials/Analytix Labs(Data Science)/BA_COURSE/BA-6, 7 & 8 (Regression)/Concordance.R")
Concordance(fit)

#Scoring using predict function for trainset
score_train <-cbind(trainset, Probability = predict(fit, newdata = trainset, type = "response"))
score_train$New_Pred[score_train$Probability>0.7] <- "Y"
score_train$New_Pred[score_train$Probability<0.7] <- "N"

#Creating confusion matrix
count(score_train,"New_Pred")
count(score_train,"Loan_Status")
with(score_train, table(Loan_Status, New_Pred))

#Scoring using predict function for testset
score_test <-cbind(testset, Probability = predict(fit, newdata = testset, type = "response"))
score_test$New_Pred[score_test$Probability>0.7] <- "Y"
score_test$New_Pred[score_test$Probability<0.7] <- "N"

write.csv(score_test, file="C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Loan Prediction/tested.csv", row.names = FALSE)

#Find the decile locations								
delocations <- quantile(score_train$Probability, probs=seq(0.1,0.9,by=0.1))

#Use findInterval with -Inf and Inf as upper and lower bounds
score_train$decile <- findInterval(score_train$Probability, c(-Inf, delocations, Inf))

#Decile anlysis reports
score_train$Approval[score_train$Loan_Status == "Y"] <- 1
score_train$Approval[score_train$Loan_Status == "N"] <- 0
require(sqldf)											
fit_train_DA <- sqldf("select decile, min(Probability) as Min_prob, max(Probability) as Max_prob, count(decile) as Total,
                      sum(Approval) as Approval_count,(count(decile)-sum(Approval)) as Non_approval_count
                      from score_train
                      group by decile         
                      order by decile desc")
               
sum(fit_train_DA$Approval_count)  
sum(fit_train_DA$Non_approval_count)

