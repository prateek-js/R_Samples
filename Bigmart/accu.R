library(plyr)
library(randomForest)
library(caret)

# load the iris dataset0

path <- "D:/R Project/Bigmart"

setwd(path)
train <- read.csv("new_train.csv")
test <- read.csv("new_test.csv")
t<-train
test$Item_Outlet_Sales <-  0
combi <- rbind(train, test)
combi


# define an 80%/20% train/test split of the dataset
set.seed(2357)
in_train <- createDataPartition( y = train$Outlet_Type, p = 0.75, list = FALSE)
train <- t[ in_train, ]
test <- t[ -in_train, ]

lm(formula = Item_Outlet_Sales~ Item_MRP+Item_Visibility+Item_Fat_Content+Item_Weight+Outlet_Type+Year+Outlet_Location_Type, data = train)
lm(formula = Outlet_Type~ Item_MRP+Item_Visibility+Item_Fat_Content+Item_Weight+Item_Outlet_Sales+Year, data = train)

model_rf <- randomForest( Outlet_Type~Year + Item_Outlet_Sales , data = train, importance = TRUE)

pred <- predict( model_rf, test)
confusionMatrix(pred, test$Outlet_Type)

