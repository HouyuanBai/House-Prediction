library(Amelia)
library(dplyr)
library(readxl)
library(plyr)
library(forecast)
library(ggplot2)
library(caret)
library(lattice)
library(tidyverse)
library(corrplot)
library(lubridate)
library(gridExtra)
library(caTools)
library(GGally)

#read and clean the data
data1 <- read.csv("kc_house_data.csv")
is.na(data1)
missmap(data1, col=c("black", "light blue"))
str(data1)
summary(data1)
mean(data1$price)
data1$date <- str_remove(data1$date,"T000000")
data1$date<- ymd(data1$date)

#Add three factors in the dataset
data1$saleYear=as.integer(year(data1$date))
data1$age=data1$saleYear-data1$yr_built
data1$reno=ifelse(data1$yr_renovated==0,0,1)
data1$reno=as.factor(data1$reno)
data1

#visualizate data

#The count of numbers of bedrooms
ggplot(data1, aes(x = bedrooms)) +
  geom_bar(fill = "light blue") +
  geom_text(stat="count", 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

#The count of numbers of bathrooms
qplot(bathrooms, data=data1, geom = "bar",
      main="Number of houses by bathrooms")+geom_bar(fill="light blue")

#The violion plot
data1 =data1%>%
mutate(bedrooms = cut(bedrooms,
                                breaks = c(-Inf, 2, 4 , 6, Inf),
                                labels = c(1, 2, 3 , 4)));
data1%>%
  select(bedrooms, price) %>%
  ggplot(mapping = aes(x = bedrooms,
                       y = price)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# The price density
ggplot(data1, aes(price)) + geom_density() +
  scale_y_continuous(limits = c(0, 2.1e-06)) +
  scale_x_continuous(limits = c(0, 2e+06)) +
  xlab("price") +
  ggtitle("Price distribution")

#Spilt the dataset
set.seed(12345)
row.number <- sample(x=1:nrow(data1), size=0.8*nrow(data1))
train = data1[row.number, ]
test = data1[-row.number,]

#Modeling
lm_model=lm(data=train,price~bedrooms+bathrooms+sqft_living+view+grade+sqft_above+sqft_basement+sqft_living15)
summary(lm_model)

#Dekete insignificant variable
lm_model2=lm(data=train,price~bedrooms+bathrooms+sqft_living+view+grade+sqft_above+age+floors+waterfront)
summary(lm_model2)

#Indentify outliers
outliers=boxplot(train$price,plot=FALSE)$out
outliers_data=train[which(train$price %in% outliers),]
train1= train[-which(train$price %in% outliers),]

# Present data modeling results containing outliers
par(mfrow=c(1, 2))
plot(train$bedrooms, train$price, main="With Outliers", xlab="bedrooms", ylab="price", pch="*", col="blue", cex=2)
abline(lm(price ~ bedrooms, data=train), col="red", lwd=3, lty=2)

# Plot of original data without outliers. Note the change of slope.
plot(train1$bedrooms, train1$price, main="Witout Outliers ", xlab="bedrooms", ylab="price", pch="*", col="blue", cex=2)
abline(lm(price ~bedrooms, data=train1), col="red", lwd=3, lty=2)

#Detect the influential points with cook function
cooksd <- cooks.distance(lm_model2)
mean(cooksd)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  


# influential row numbers
head(train[influential, ])
influential_data=train[influential, ]
influencial_outliers=inner_join(outliers_data,influential_data)
influencial_outliers

#Use the data without outliers to tarin
train2=rbind(train1,influencial_outliers)
lm_model3=lm(data=train2,price~bedrooms+bathrooms+sqft_living+view+grade+sqft_lot+age+floors+waterfront+reno)
summary(lm_model3)

#Delete the sqft_lot and add more varialbes
lm_model4=lm(data=train2,price~bedrooms+bathrooms+sqft_living+view+grade++age+floors+waterfront+long+lat+zipcode+condition+sqft_above+sqft_living15+reno)
summary(lm_model4)

#Accuracy of the model on the train data
pred=lm_model4$fitted.values

validation=data.frame(actual=train2$price, predicted=pred)

classify=mean(abs(validation$actual-validation$predicted)/validation$actual)

accuracy=1-classify

paste('The accuracy of train set is:',accuracy)

#Prediction on the test data

test1=test[,c(4,5,6,8,10,9,12,23,24,17,18,19,11,13,20)]

pred_test=predict(newdata=test1,lm_model4)

#Accuracy of the modelon the test data
validate_1=data.frame(actual=test$price, predicted=pred_test)

classify_test=mean(abs(validate_1$actual-validate_1$predicted)/validate_1$actual)

accuracy_test=1-classify_test

accuracy_test
