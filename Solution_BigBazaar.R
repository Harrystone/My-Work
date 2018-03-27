#Imported datasets
train <- read.csv(file = "C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Big Mart Sales/Train_UWu5bXk.csv", na.strings=c("","NA"), header = TRUE, stringsAsFactors = FALSE)
test <- read.csv(file = "C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Big Mart Sales/Test_u94Q5KV.csv", na.strings=c("","NA"), header = TRUE, stringsAsFactors = FALSE)

#Concatenation of train & test dataset
test$Item_Outlet_Sales <- NA
data <- rbind(train,test)

#Finding missing values in each variable
apply(is.na(data),2,sum)
apply(!is.na(data),2,sum)

#Basic statistics of variables
summary(data)

#Counted number of unique values in each variable excluding NA
sapply(data, function(x) length(unique(x[!is.na(x)])))

#Counted frequency of unique values in each categorical variable
install.packages("plyr")
library(plyr)

count(data, "Item_Fat_Content")
count(data, "Item_Type")
count(data, "Outlet_Location_Type")
count(data, "Outlet_Size")
count(data, "Outlet_Type")

#Imputing missing values in Item_Weight
data$Item_Weight[is.na(data$Item_Weight)] <- mean(data$Item_Weight, na.rm = TRUE)
sum(is.na(data$Item_Weight))

#Imputing missing values in Outlet size
mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
data$Outlet_Size[is.na(data$Outlet_Size)] <- mode(data$Outlet_Size, na.rm = TRUE)
sum(is.na(data$Outlet_Size))

#Checking Item_Outlet_sales for different category of Outlet_Size, outlet_Location_Type & Outlet_Type
aggregate(Item_Outlet_Sales~Outlet_Size, data, mean)
aggregate(Item_Outlet_Sales~Outlet_Location_Type, data, mean)
aggregate(Item_Outlet_Sales~Outlet_Type, data, mean)

#Considered combining Outlet_Size & Outlet_Location_Type
data$Outlet_Size[data$Outlet_Size == "Medium"] <- "Small"
data$Outlet_Location_Type[data$Outlet_Location_Type == "Tier 3"] <- "Tier 2"

#Modification in Item_Visibility
Zero_Values <- subset(data, Item_Visibility == 0.000000000, select = Item_Identifier)
groupby <- aggregate(Item_Visibility~Item_Identifier, data, mean)
result <- join(Zero_Values,groupby)

#Imputing zero values in Item_Visisbility
data$Item_Visibility[data$Item_Visibility == 0.000000000] <- result$Item_Visibility

#Creation of new variable Item_Visibility_Meanratio
extract <- data[1]
group <- join(extract,groupby)

data$Item_Visibility_Meanratio <- data$Item_Visibility/group$Item_Visibility
summary(data$Item_Visibility_Meanratio)

#Creation of a broad category for Item_Type
library(data.table)
data$Item_Type_Combined[data$Item_Identifier %like% "^FD"] <- "Food"
data$Item_Type_Combined[data$Item_Identifier %like% "^NC"] <- "Non-Consumable"
data$Item_Type_Combined[data$Item_Identifier %like% "^DR"] <- "Drinks"

#Years of operation of a store
data$Outlet_Years <- 2013-data$Outlet_Establishment_Year

#Combining similar categories of Item_Fat_Content
data$Item_Fat_Content[data$Item_Fat_Content == "LF"] <- "Low Fat"
data$Item_Fat_Content[data$Item_Fat_Content == "low fat"] <- "Low Fat"
data$Item_Fat_Content[data$Item_Fat_Content == "reg"] <- "Regular"

#Non-Consumable products cannot have Item_Fat_Content
data$Item_Fat_Content[data$Item_Type_Combined == "Non-Consumable"] <- "Non-Edible"

#R code for categorical variable(converting as factor variable)
dataset <- data
sapply(dataset,class)
dataset[c("Item_Identifier","Item_Fat_Content","Outlet_Identifier","Outlet_Size","Outlet_Location_Type","Outlet_Type","Item_Type_Combined")] <- lapply(dataset[c("Item_Identifier","Item_Fat_Content","Outlet_Identifier","Outlet_Size","Outlet_Location_Type","Outlet_Type","Item_Type_Combined")], factor)
levels(dataset$Item_Fat_Content)

#Drop unnecessary columns
dataset$Item_Type <- NULL

#Slicing dataset back to train & test
testset <- tail(dataset, -8523)
trainset <- head(dataset, -5681)

#Drop unnecessary columns
testset$Item_Outlet_Sales <- NULL


#Exporting modified train & test dataset
write.csv(trainset, file="C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Big Mart Sales/trainset.csv", row.names = FALSE)
write.csv(testset, file="C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Big Mart Sales/testset.csv", row.names = FALSE)

#Using log() to normal the distribution of Item_Outlet_Sales
hist(trainset$Item_Outlet_Sales) 
hist(log(trainset$Item_Outlet_Sales))

trainset$Item_Outlet_Sales <- log(trainset$Item_Outlet_Sales)

#Modelling using linear regression
model <- trainset
model$Item_Identifier <- NULL 
model$Outlet_Size <- NULL
model$Outlet_Location_Type <- NULL
model$Item_Type_Combined <- NULL
model$Outlet_Establishment_Year <- NULL
model$Item_Fat_Content <- NULL
model$Item_Visibility <- NULL
model$Item_Visibility_Meanratio <- NULL
model$Outlet_Identifier <- NULL
model$Item_Weight <- NULL

fit <- lm(Item_Outlet_Sales ~ . , data = model)
summary(fit)

install.packages("car")
library(car)

scatterplotMatrix(model)
vif(fit)

library(MASS)
fit_model <- stepAIC(fit,direction="both")	
ls(fit_model)
fit_model$annova

#Scoring using predict function for trainset
trainset$Item_Outlet_Sales <- exp(trainset$Item_Outlet_Sales)
t1 <- cbind(trainset, Pred_Sales = exp(predict(fit)))	
t1 <- transform(t1, APE=abs(Pred_Sales-Item_Outlet_Sales)/Item_Outlet_Sales)
mean(t1$APE)

#Find the decile locations								
delocations <- quantile(t1$Pred_Sales, probs=seq(0.1,0.9,by=0.1))

#Use findInterval with -Inf and Inf as upper and lower bounds

t1$decile <- findInterval(t1$Pred_Sales, c(-Inf, delocations, Inf))

install.packages("sqldf")
require(sqldf)

t1_DA <- sqldf("select decile, count(decile) as Count, avg(Pred_Sales) as Avg_Pred_Sales,
               avg(Item_Outlet_Sales) as Avg_Actual_Sales
               from t1
               group by decile
               order by decile desc")
t1_DA$rank <- rank(-t1_DA$Avg_Pred_Sales)

plot( x=t1_DA$rank, y=t1_DA$Avg_Pred_Sales, main = "Actual vs Predicted", type="b", col="red" , xlab = "Rank", ylab = "Actual(Green) & Predicted(Red)")
lines( x=t1_DA$rank, y=t1_DA$Avg_Actual_Sales, type="b", col="green", xlab = "Rank", ylab = "Actual(Green) & Predicted(Red)")							


#Scoring using predict function for testset
t2 <- cbind(testset, Pred_Sales = exp(predict(fit,testset)))	

write.csv(t2, file="C:/Users/Gopal/Desktop/Materials/AnalyticsVidhya(Case Studies)/Big Mart Sales/test_predicted.csv", row.names = FALSE)


					


							


