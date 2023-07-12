#Number of rented bike prediction----
setwd("C:/SY/Semester 2/DS/Course Project")
df=read.csv("SeoulBikeData.csv")

library(ggplot2)
library(class)
library(caTools)
library(caret)
library(Matrix)
library(datasets)
library(magrittr)
library(partykit)
library(randomForest)
library(rpart)
library(e1071)
library(xgboost)

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
#set.seed(123)
#clean_df<-clean_df[sample(nrow(df3)),]

#n2<-function(b){(b-min(b))/(max(b)-min(b))}
#fnor<-as.data.frame(lapply(df3[1:12], n2))

#summary(fnor)
#boxplot(fnor)

#Splitting Data----
set.seed(123)
sample<-sample(c(TRUE,FALSE),nrow(clean_df),replace = TRUE,prob = c(0.8,0.2))
split<-sample.split(clean_df$Rented_Bike_Count,SplitRatio=0.8)
training<-subset(clean_df,split=TRUE)
testing<-subset(clean_df,split=FALSE)

#MLR----
#mlr<-lm(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Seasons+Holiday,clean_df)
mlr<-lm(Rented_Bike_Count~.,clean_df)
s1<-summary(mlr)
print(s1)

a<-RMSE(clean_df$Rented_Bike_Count, predict(mlr,clean_df))
cat("\nRMSE for MLR: ",a)
b<-MAE(clean_df$Rented_Bike_Count, predict(mlr,clean_df))
cat("\nMAE for MLR: ",b)
c<-R2(clean_df$Rented_Bike_Count, predict(mlr,clean_df))
cat("\nR2 for MLR: ",c)

#Decision Tree----
#dt<-rpart(Rented_Bike_Count ~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Seasons+Holiday, data=training, method = "anova")
dt<-rpart(Rented_Bike_Count ~., data=training, method = "anova")
plot(dt)
printcp(dt)
text(dt)
pred_dc <- predict(dt, newdata = testing)
var_imp=varImp(dt)
print(var_imp)

d<-RMSE(pred_dc, clean_df$Rented_Bike_Count)
cat("\n\nRMSE for Decision Tree: ",d)
e<-MAE(pred_dc, clean_df$Rented_Bike_Count)
cat("\nMAE for Decision Tree: ",e)
f<-R2(pred_dc, clean_df$Rented_Bike_Count)
cat("\nR2 for Decision Tree: ",f)

#Random Forest----
#rf <- randomForest(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday, data = training, ntree = 1000, importance = TRUE, type = "regression")
rf <- randomForest(Rented_Bike_Count~., data = training, ntree = 1000, importance = TRUE, type = "regression")
var_imp=varImp(rf)
print(var_imp)
randomForest::varImpPlot(rf, sort=TRUE, main="Variable Importance Plot")
s2<-summary(rf)
print(s2)
plot(rf)
pred_rf = predict(rf, training)

g<-RMSE(pred_rf, clean_df$Rented_Bike_Count)
cat("\n\nRMSE for Random Forest: ",g)
h<-MAE(pred_rf, clean_df$Rented_Bike_Count)
cat("\nMAE for Random Forest: ",h)
i<-R2(pred_rf, clean_df$Rented_Bike_Count)
cat("\nR2 for Random Forest: ",i)

#Support Vector Regression----
#svr <- svm(Rented_Bike_Count ~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Seasons+Holiday, data = training, kernel = "radial")
svr <- svm(Rented_Bike_Count ~ ., data = training, kernel = "radial")
pred_svr <- predict(svr, testing)

j<-RMSE(pred_svr, clean_df$Rented_Bike_Count)
cat("\n\nRMSE for Support Vector Regression: ",j)
k<-MAE(pred_svr, clean_df$Rented_Bike_Count)
cat("\nMAE for Support Vector Regression: ",k)
l<-R2(pred_svr, clean_df$Rented_Bike_Count)
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
