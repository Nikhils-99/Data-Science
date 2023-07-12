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
library(corrplot)

#Dropping non useful Columns----
drop<-c("Date","Functioning_Day","Dew_point_temperature","Temperature")
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
clean_df = subset(df3, #Temperature<=up_freq_Temperature & Temperature>=low_freq_Temperature 
                 Humidity<=up_freq_Humidity & Humidity>=low_freq_Humidity 
                  & Wind_speed<=up_freq_Wind_speed & Wind_speed>=low_freq_Wind_speed 
                  & Visibility<=up_freq_Visibility & Visibility>=low_freq_Visibility 
            #      & Dew_point_temperature<=up_freq_Dew_point_temperature & Dew_point_temperature>=low_freq_Dew_point_temperature 
                  & Solar_Radiation<=up_freq_Solar_Radiation & Solar_Radiation>=low_freq_Solar_Radiation 
                  #                  & Rainfall<=up_freq_Rainfall & Rainfall>=up_freq_Rainfall 
                  #                  & Snowfall<=up_freq_Snowfall & Snowfall<=up_freq_Snowfall
)

boxplot(clean_df)

#Normalizaztion----
set.seed(123)
clean_df<-clean_df[sample(nrow(df3)),]

n2<-function(b){(b-min(b))/(max(b)-min(b))}
fnor<-as.data.frame(lapply(df3[1:9], n2))

summary(fnor)
boxplot(fnor)
correlation_matrix <- cor(fnor)
corrplot(correlation_matrix, method = "color", type = "full", tl.cex = 0.8)

#Splitting Data----
set.seed(123)
split_data <- sample(1:nrow(fnor), nrow(fnor)*0.8)
training <- fnor[split_data, ]
testing <- fnor[-split_data, ]

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
