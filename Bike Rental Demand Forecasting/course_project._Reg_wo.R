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
library(stats)

#Dropping non useful Columns----
drop<-c("Date","Functioning_Day")
df1=df[,!(names(df) %in% drop)]

#Dropped rows where Bike count is 0----
df2<-df1[!(df1$Rented_Bike_Count==0),]

#Converting string column into numeric----
df3<-transform(df2,Seasons=as.numeric(as.factor(Seasons)),Holiday=as.numeric(as.factor(Holiday)))
dim(df3)

boxplot(df3)
#summary(df3)

#Normalizaztion----
set.seed(123)
df3<-df3[sample(nrow(df3)),]

n2<-function(b){(b-min(b))/(max(b)-min(b))}
fnor<-as.data.frame(lapply(df3[1:12], n2))

summary(fnor)
boxplot(fnor)

#PCA(Principal Component Analysis)
pca_result <- prcomp(fnor, scale = TRUE)
summary(pca_result)

#Splitting Data----
set.seed(1502)
split_data <- sample(1:nrow(fnor), nrow(fnor)*0.8)
training <- fnor[split_data, ]
testing <- fnor[-split_data, ]

#(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall+Snowfall+Seasons+Holiday,fnor)

#MLR----
mlr<-lm(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall,fnor)
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
dt<-rpart(Rented_Bike_Count ~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall, data=training, method = "anova")
#dt<-rpart(Rented_Bike_Count ~., data=training, method = "anova")
plot(dt)
#printcp(tree)
text(dt)
pred_dc <- predict(dt,testing)

d<-RMSE(pred_dc, testing$Rented_Bike_Count)
cat("\n\nRMSE for Decision Tree: ",d)
e<-MAE(pred_dc, testing$Rented_Bike_Count)
cat("\nMAE for Decision Tree: ",e)
f<-R2(pred_dc, testing$Rented_Bike_Count)
cat("\nR2 for Decision Tree: ",f)

#Random Forest----
set.seed(1234)
#rf <- randomForest(Rented_Bike_Count~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall, data = training, ntree = 1000, importance = TRUE, type = "regression")
rf <- randomForest(Rented_Bike_Count~., data = training, ntree = 1000, importance = TRUE, type = "regression")
s2<-summary(rf)
print(s2)
var_imp=varImp(rf)
print(var_imp)
randomForest::varImpPlot(rf, sort=TRUE, main="Variable Importance Plot")
pred_rf = predict(rf, testing)

g<-RMSE(pred_rf, testing$Rented_Bike_Count)
cat("\n\nRMSE for Random Forest: ",g)
h<-MAE(pred_rf, testing$Rented_Bike_Count)
cat("\nMAE for Random Forest: ",h)
i<-R2(pred_rf, testing$Rented_Bike_Count)
cat("\nR2 for Random Forest: ",i)

#Support Vector Regression----
svr <- svm(Rented_Bike_Count ~Hour+Temperature+Humidity+Wind_speed+Visibility+Dew_point_temperature+Solar_Radiation+Rainfall, data = training, kernel = "radial")
#svr <- svm(Rented_Bike_Count ~ ., data = training, kernel = "radial")
pred_svr <- predict(svr, testing)

j<-RMSE(pred_svr, testing$Rented_Bike_Count)
cat("\n\nRMSE for Support Vector Regression: ",j)
k<-MAE(pred_svr, testing$Rented_Bike_Count)
cat("\nMAE for Support Vector Regression: ",k)
l<-R2(pred_svr, testing$Rented_Bike_Count)
cat("\nR2 for Support Vector Regression: ",l)

index_df= num(1:6772)
mat = matrix(ncol = 0, nrow = 6772)
visualisation_df=data.frame(mat)
visualisation_df$Actual=training$Rented_Bike_Count

predicted = predict(rf,training)
visualisation_df$Predicted=predicted
visualisation_df$Index= index_df


data <- data.frame(Actual = visualisation_df$Actual,Predicted = visualisation_df$Predicted)

data_ggp <- data.frame(Predicted = data$Actual, Actual= c(data$Actual, data$Predicted), group = c(rep("Actual", nrow(data)), rep("Predicted", nrow(data))))
ggp <- ggplot(data_ggp, aes(Actual,Predicted, col = group)) + geom_line()
ggp
