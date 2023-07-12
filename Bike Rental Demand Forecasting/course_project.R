setwd("C:/SY/Semister 2/DS/Course Project")
df=read.csv("SeoulBikeData.csv")
library(ggplot2)
library(class)
library(caTools)
library(caret)

#Dropping non useful Columns----
drop<-c("Date","Rented_Bike_Count")
df1=df[,!(names(df) %in% drop)]

#Converting column input into numeric----
df2<-transform(df1,Seasons=as.numeric(as.factor(Seasons)),Holiday=as.numeric(as.factor(Holiday)),Functioning_Day=as.numeric(as.factor(Functioning_Day)))
dim(df2)
boxplot(df2)
summary(df2)

#Normalizaztion----
set.seed(123)
df2<-df2[sample(nrow(df2)),]

n2<-function(b){(b-min(b))/(max(b)-min(b))}
fnor<-as.data.frame(lapply(df2[1:12], n2))

summary(fnor)

#Finding Outliers----
#IQR <- Upper_freq - Lower_freq
#Lower_freq <- quartiles[1] - 1.5*IQR
#Upper_freq <- quartiles[3] + 1.5*IQR 

IQR_Temperature = 0.6533-0.2889
up_freq_Temperature = 0.6533+1.5*IQR_Temperature
low_freq_Temperature = 0.6533-1.5*IQR_Temperature

IQR_Humidity = 0.6667-0.3056
up_freq_Humidity = 0.6667+1.5*IQR_Humidity
low_freq_Humidity = 0.6667-1.5*IQR_Humidity

IQR_Wind_speed = 0.4750-0.1500
up_freq_Wind_speed = 0.4750+1.5*IQR_Wind_speed
low_freq_Wind_speed = 0.4750-1.5*IQR_Wind_speed

IQR_Visibility = 1.000-0.3761
up_freq_Visibility = 1.000+1.5*IQR_Visibility
low_freq_Visibility = 1.000-1.5*IQR_Visibility

IQR_Dew_point_temperature = 0.7380+0.3197
up_freq_Dew_point_temperature = 0.7380+1.5*IQR_Dew_point_temperature
low_freq_Dew_point_temperature = 0.7380-1.5*IQR_Dew_point_temperature

IQR_Solar_Radiation = 0.2974-0.0000
up_freq_Solar_Radiation = 0.2974+1.5*IQR_Solar_Radiation
low_freq_Solar_Radiation = 0.2974-1.5*IQR_Solar_Radiation

IQR_Rainfall = 0.000-0.000
up_freq_Rainfall = 0.000+1.5*IQR_Rainfall
low_freq_Rainfall = 0.000-1.5*IQR_Rainfall

IQR_Snowfall = 0.000-0.000
up_freq_Snowfall = 0.000+1.5*IQR_Snowfall
low_freq_Snowfall = 0.000-1.5*IQR_Snowfall

#IQR_Holiday = 2.000-2.000
#up_freq_Holiday = 2.000+1.5*IQR_Holiday
#low_freq_Holiday = 2.000-1.5*IQR_Holiday

#IQR_Functioning_Day = 2.000-2.000
#up_freq_Functioning_Day = 2.000+1.5*IQR_Functioning_Day
#low_freq_Functioning_Day = 2.000-1.5*IQR_Functioning_Day

boxplot(fnor)
#dataset after removal of outliers----
clean_df = subset(fnor,Temperature<=up_freq_Temperature & Temperature>=low_freq_Temperature 
                  & Humidity<=up_freq_Humidity & Humidity>=low_freq_Humidity 
                  & Wind_speed<=up_freq_Wind_speed & Wind_speed>=low_freq_Wind_speed 
                  & Visibility<=up_freq_Visibility & Visibility>=low_freq_Visibility 
                  & Dew_point_temperature<=up_freq_Dew_point_temperature & Dew_point_temperature>=low_freq_Dew_point_temperature 
                  & Solar_Radiation<=up_freq_Solar_Radiation & Solar_Radiation>=low_freq_Solar_Radiation 
                  & Rainfall<=up_freq_Rainfall & Rainfall>=up_freq_Rainfall 
                  & Snowfall<=up_freq_Snowfall & Snowfall<=up_freq_Snowfall
                 # & Holiday<=up_freq_Holiday & Holiday>=up_freq_Holiday
                 # & Functioning_Day<=up_freq_Functioning_Day & Functioning_Day>=up_freq_Functioning_Day
                 )
boxplot(clean_df)

#KNN----
sample<-sample(c(TRUE,FALSE),nrow(clean_df),replace = TRUE,prob = c(0.8,0.2))
split<-sample.split(clean_df$Functioning_Day,SplitRatio=0.8)
train<-subset(clean_df,split=TRUE)
test<-subset(clean_df,split=FALSE)
train_label = train[,12]
test_label = test[,12]
train1 = train[,-12]
test1 = test[,-12]
dim(clean_df)
#train<-fnor[1:7006,]
#test<-fnor[7007:8760,]
#train_label<-clean_df[1:7006,-12]
#test_label<-clean_df[7007:8760,-12]

p<-knn(train1,test1,train_label,k=7)
t<-table(actual=test_label,predicted=p)
print(t)

acc <- (108+8442)/(108+8442+187+23)
cat("\nAccuracy for KNN: ",acc)

sen = 8442/(8442+23)
cat("\nSpecificity for KNN:",sen)

spe = 108/(108+187)
cat("\nSensitivity for KNN:",spe)

pre = 8442/(8442+187)
cat("\nPrecision for KNN:",pre)

#Decision tree  ----
library(party)

model<- ctree(Functioning_Day ~ ., train)
plot(model)
predict_model<-predict(model, test)
cm1 <- table(test$Functioning_Day, predict_model)
cat("\n")
print(cm1)

ac_Test <- sum(diag(cm1)) / sum(cm1)
print(paste('Accuracy for Decision Tree: ', ac_Test))