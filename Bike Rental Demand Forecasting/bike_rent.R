f=read.csv("SeoulBikeData.csv")
library(ggplot2)
library(skimr)

ggplot(data = f)

sum(is.na(f$Date))

p<-ggplot(data = f, mapping = aes(x = Functioning_Day, y = Temperature, fill=Temperature)) + geom_boxplot(outlier.shape = 4,outlier.color = "red")+theme(legend.position = "none")+labs(title = "Functioning_Day",x="",y="Temperature")+coord_flip()
print(p)

print(ggplot(f) + geom_point(mapping = aes(x = Rented_Bike_Count, y = Hour, color=Seasons)))
print(ggplot(f) + geom_point(mapping = aes(x = Rented_Bike_Count, y = Hour, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Rented_Bike_Count, y = Hour, color=Seasons))) + facet_wrap(Holiday~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Temperature, color=Seasons)))
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Temperature, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Humidity, color=Seasons))) 
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Humidity, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Wind_speed, color=Seasons))) 
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Wind_speed, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Visibility, color=Seasons))) 
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Visibility, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Dew_point_temperature, color=Seasons)))
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Dew_point_temperature, color=Seasons)))+ facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Solar_Radiation, color=Seasons)))
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Solar_Radiation, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Rainfall, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Rainfall, color=Seasons))) 
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Snowfall, color=Seasons))) + facet_wrap(~Seasons)
print(ggplot(f) + geom_point(mapping = aes(x = Functioning_Day, y = Snowfall, color=Seasons))) 

#print(ggplot(f, aes(x = Functioning_Day, y = Seasons)) +geom_point())
#print(ggplot(f, aes(x = Functioning_Day, y = Holiday)) +geom_point())

#SVM----
library(e1071)
library(lattice)

classifier = svm(formula = fnor$Functioning_Day ~ .,
                 data = train1,
                 type = 'C-classification',
                 kernel = 'linear')
#classifier<-svm(formula=Functioning_Day ~ .,data=training,type='C-classification',kernel=('linear'))
#pred=predict(classifier,testing$Functioning_Day)
y_pred = predict(classifier, newdata = test1[-12])

cm = table(test1[, 12], y_pred)
acc <- sum(diag(cm)) / sum(cm)
acc

#Random Forest----
library(randomForest)
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train1,
                             y = train1$Functioning_Day,
                             ntree = 20)
