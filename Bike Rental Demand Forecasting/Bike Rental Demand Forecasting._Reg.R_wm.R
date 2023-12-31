#Number of rented bike prediction----
setwd("C:/SY/Semester 2/DS/Course Project")
df=read.csv("SeoulBikeData.csv")

library(ggplot2)
library(class)
library(caTools)
library(caret)
library(Matrix)
library(datasets)
#library(magrittr)
library(partykit)
library(randomForest)
library(rpart)
library(e1071)
library(xgboost)
library(corrplot)

#Dropping non useful Columns----
drop<-c("Date","Functioning_Day")
df1=df[,!(names(df) %in% drop)]

#Dropped rows where Bike count is 0----
df2<-df1[!(df1$Rented_Bike_Count==0),]

#Converting string column into numeric----
df3<-transform(df2,Seasons=as.numeric(as.factor(Seasons)),Holiday=as.numeric(as.factor(Holiday)))
dim(df3)

boxplot(df3)
summary(df3)

# Split data into features and target variable
X <- df3[, -1]  # features
y <- df3$Rented_Bike_Count  # target variable

# Function for forward selection
forward_selection <- function(X, y, model, performance_metric) {
  selected_features <- c()
  
  while (length(selected_features) < ncol(X)) {
    best_performance <- -Inf
    best_feature <- NULL
    
    for (i in 1:ncol(X)) {
      if (!(colnames(X)[i] %in% selected_features)) {
        # Add the feature to the list of selected features
        temp_features <- c(selected_features, colnames(X)[i])
        
        # Train the model with the selected features
        model_fit <- train(y = y, x = X[, temp_features], method = model)
        
        # Calculate the performance of the model using the selected performance metric
        performance <- performance_metric(model_fit)
        
        # Update the best performance and best feature if necessary
        if (performance > best_performance) {
          best_performance <- performance
          best_feature <- colnames(X)[i]
        }
      }
    }
    
    if (!is.null(best_feature)) {
      # Add the best feature to the list of selected features
      selected_features <- c(selected_features, best_feature)
    } else {
      # No more improvement in performance, stop the loop
      break
    }
  }
  
  return(selected_features)
}

# Example usage of forward_selection function
selected_features <- forward_selection(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday, y, "glm", function(model) { 
  return(performance(model)$results$Accuracy) 
})

# Print the selected features
cat("Selected features:", selected_features)

#Finding Outliers----

IQR_Temperature = 22.70-3.00
up_freq_Temperature = 22.70+1.5*IQR_Temperature
low_freq_Temperature = 22.70-1.5*IQR_Temperature

IQR_Humidity = 74.00-42.00
up_freq_Humidity = 74.00+1.5*IQR_Humidity
low_freq_Humidity = 74.00-1.5*IQR_Humidity

IQR_Wind_speed = 2.300-0.900
up_freq_Wind_speed = 0.3108+1.5*IQR_Wind_speed
low_freq_Wind_speed = 0.3108-1.5*IQR_Wind_speed

IQR_Visibility = 2000-935
up_freq_Visibility = 2000+1.5*IQR_Visibility
low_freq_Visibility = 2000-1.5*IQR_Visibility

IQR_Dew_point_temperature = 15.200+5.100
up_freq_Dew_point_temperature = 15.200+1.5*IQR_Dew_point_temperature
low_freq_Dew_point_temperature = 15.200-1.5*IQR_Dew_point_temperature

IQR_Solar_Radiation = 0.9300-0.0000
up_freq_Solar_Radiation = 0.9300+1.5*IQR_Solar_Radiation
low_freq_Solar_Radiation = 0.9300-1.5*IQR_Solar_Radiation

#IQR_Rainfall = -
#up_freq_Rainfall = +1.5*IQR_Rainfall
#low_freq_Rainfall = -1.5*IQR_Rainfall

#IQR_Snowfall = 
#up_freq_Snowfall = +1.5*IQR_Snowfall
#low_freq_Snowfall = -1.5*IQR_Snowfall

#dataset after removal of outliers----
clean_df = subset(df3, Temperature<=up_freq_Temperature & Temperature>=low_freq_Temperature 
                  & Humidity<=up_freq_Humidity & Humidity>=low_freq_Humidity 
                  & Wind_speed<=up_freq_Wind_speed & Wind_speed>=low_freq_Wind_speed 
                  & Visibility<=up_freq_Visibility & Visibility>=low_freq_Visibility 
                  & Dew_point_temperature<=up_freq_Dew_point_temperature & Dew_point_temperature>=low_freq_Dew_point_temperature 
                  & Solar_Radiation<=up_freq_Solar_Radiation & Solar_Radiation>=low_freq_Solar_Radiation 
                  #                  & Rainfall<=up_freq_Rainfall & Rainfall>=up_freq_Rainfall 
                  #                  & Snowfall<=up_freq_Snowfall & Snowfall<=up_freq_Snowfall
)

boxplot(clean_df)

#Normalizaztion----
set.seed(123)
clean_df<-clean_df[sample(nrow(df3)),]

n2<-function(b){(b-min(b))/(max(b)-min(b))}
fnor<-as.data.frame(lapply(df3[1:12], n2))

summary(fnor)
boxplot(fnor)

#Correlation
correlation_matrix <- cor(fnor)
corrplot(correlation_matrix, method = "number", type = "full", tl.cex = 0.8)

#Splitting Data----
set.seed(123)
split_data <- sample(1:nrow(fnor), nrow(fnor)*0.8)
training <- fnor[split_data, ]
testing <- fnor[-split_data, ]


#(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday,fnor)

#MLR----
mlr<-lm(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday,training)
#mlr<-lm(Rented_Bike_Count~.,training)
s1<-summary(mlr)
print(s1)

a<-RMSE(testing$Rented_Bike_Count, predict(mlr,testing))
cat("\nRMSE for MLR: ",a)
b<-MAE(testing$Rented_Bike_Count, predict(mlr,testing))
cat("\nMAE for MLR: ",b)
c<-R2(testing$Rented_Bike_Count, predict(mlr,testing))
cat("\nR2 for MLR: ",c)

#Decision Tree----
dt<-rpart(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday, data=training, method = "anova")
#dt<-rpart(Rented_Bike_Count ~., data=training, method = "anova")

#wrapper method
#ctrl <- rfeControl(method = "cv", number = 10)
#rfe <- rfe(training[, -1], training$Rented_Bike_Count, sizes = c(1:5), rfeControl = ctrl) # Perform RFE

#selected_features <- rfe$optVariables
#final_model <- train(y ~ ., data = training[, c("Rented_Bike_Count", selected_features)], method = "anova")
#predicted <- predict(final_model, newdata = testing)

plot(dt)
printcp(dt)
text(dt)
pred_dc <- predict(dt,testing)
var_imp=varImp(dt)
print(var_imp)
#cm<- confusionMatrix(pred_dc,testing$Rented_Bike_Count)

d<-RMSE(pred_dc, testing$Rented_Bike_Count)
cat("\n\nRMSE for Decision Tree: ",d)
e<-MAE(pred_dc, testing$Rented_Bike_Count)
cat("\nMAE for Decision Tree: ",e)
f<-R2(pred_dc, testing$Rented_Bike_Count)
cat("\nR2 for Decision Tree: ",f)

#Random Forest----
rf <- randomForest(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday, data = training, ntree = 1000, importance = TRUE, type = "regression")
#rf <- randomForest(Rented_Bike_Count~., data = training, ntree = 1000, importance = TRUE, type = "regression")
importance(rf)
var_imp=varImp(rf)
print(var_imp)
randomForest::varImpPlot(rf, sort=TRUE, main="Variable Importance Plot")
s2<-summary(rf)
print(s2)
plot(rf)
pred_rf = predict(rf, testing)

g<-RMSE(pred_rf, testing$Rented_Bike_Count)
cat("\n\nRMSE for Random Forest: ",g)
h<-MAE(pred_rf, testing$Rented_Bike_Count)
cat("\nMAE for Random Forest: ",h)
i<-R2(pred_rf, testing$Rented_Bike_Count)
cat("\nR2 for Random Forest: ",i)

#Support Vector Regression----
svr <- svm(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday, data = training, kernel = "radial")
#svr <- svm(Rented_Bike_Count ~ ., data = training, kernel = "radial")
pred_svr <- predict(svr, testing)

j<-RMSE(pred_svr, testing$Rented_Bike_Count)
cat("\n\nRMSE for Support Vector Regression: ",j)
k<-MAE(pred_svr, testing$Rented_Bike_Count)
cat("\nMAE for Support Vector Regression: ",k)
l<-R2(pred_svr, testing$Rented_Bike_Count)
cat("\nR2 for Support Vector Regression: ",l)

#XGBoost----

train_x = data.matrix(training[,-1])
train_y = training[,1]
test_x = data.matrix(testing[,-1])
test_y = testing[,1]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)
xgb <- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 74)

pred_xgb <- predict(xgb, xgb_test)

m<-RMSE(test_y, pred_xgb)
cat("\n\nRMSE for XGBoost: ",m)
n<-MAE(test_y,pred_xgb)
cat("\nMAE for XGBoost: ",n)
o<-R2(test_y,pred_xgb)
cat("\nR2 for XGBoost: ",o)
