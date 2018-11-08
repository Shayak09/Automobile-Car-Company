#setting working location
setwd("F:/Data Science/Iinear Regression/CarPrice")

#loading dataSet
carPrice<-read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)
str(carPrice)

#DataPreparation
carPrice$drivewheel[which(carPrice$drivewheel=="4wd")]<-"fwd"

#breaking carName into company and model name column and binding with main dataset
library(stringr)

y <- as.data.frame(str_split_fixed(carPrice$CarName, " ", 2))
colnames(y)<-c("car_company", "car_model")

carPrice<-cbind(carPrice, y)

#removing unnecessary columns
carPrice$CarName<-NULL
carPrice$car_model<-NULL


#converting fuelType to levels
carPrice$fueltype<-as.factor(carPrice$fueltype)
levels(carPrice$fueltype)<-c(1,0)
carPrice$fueltype<-as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

#applying for other variables also

carPrice$aspiration<-as.factor(carPrice$aspiration)
levels(carPrice$aspiration)<-c(1,0)
carPrice$aspiration<-as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

carPrice$doornumber<-as.factor(carPrice$doornumber)
levels(carPrice$doornumber)<-c(4,2)
carPrice$doornumber<-as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

carPrice$drivewheel<-as.factor(carPrice$drivewheel)
levels(carPrice$drivewheel)<-c(1,0)
carPrice$drivewheel<-as.numeric(levels(carPrice$drivewheel))[carPrice$drivewheel]

carPrice$enginelocation<-as.factor(carPrice$enginelocation)
levels(carPrice$enginelocation)<-c(1,0)
carPrice$enginelocation<-as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

#converting dohcv to dohc as only 1 value is present and it could be error in data
carPrice$enginetype[which(carPrice$enginetype=="dohcv")]<-"dohc"

carPrice$car_company[which(carPrice$car_company=="maxda")]<-"mazda"
carPrice$car_company[which(carPrice$car_company=="Nissan")]<-"nissan"
carPrice$car_company[which(carPrice$car_company=="porcshce")]<-"porsche"
carPrice$car_company[which(carPrice$car_company=="toyouta")]<-"toyota"
carPrice$car_company[which(carPrice$car_company=="vokswagen")]<-"volkswagen"

summary(carPrice$car_company)
#As per summary, incorrectly spelled car company names have now 0 rows

#creating dummy variable for car_company variable
dummy_1 <- data.frame(model.matrix( ~carPrice$car_company, data = carPrice))
dummy_1 <- dummy_1[,-c(1,10,15,18,24,25)] #removing the incorrectly spelled company names

carPrice_1<-cbind(carPrice[,-26], dummy_1) #binding dummy variable with main dataset

#creating dummy variable for cylindernumber variable
dummy_2 <- data.frame(model.matrix( ~cylindernumber, data = carPrice_1))
dummy_2 <- dummy_2[,-1]

carPrice_2<-cbind(carPrice_1[,-15], dummy_2)

#creating dummy variable for carbody variable
dummy_3 <- data.frame(model.matrix( ~carbody, data = carPrice_2))
dummy_3 <- dummy_3[,-1]

carPrice_3<-cbind(carPrice_2[,-6], dummy_3)

#creating dummy variable for enginetype variable
dummy_4 <- data.frame(model.matrix( ~enginetype, data = carPrice_3))
dummy_4 <- dummy_4[,-1]

carPrice_4<-cbind(carPrice_3[,-13], dummy_4)

#creating dummy variable for fuelsystem variable
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = carPrice_4))
dummy_5 <- dummy_5[,-1]

carPrice_5<-cbind(carPrice_4[,-14], dummy_5)


#install.packages("car")
library("car")
#install.packages("MASS")
library(MASS)

#creating test and train data sets
set.seed(100)
carPriceIndices<-sample(1:nrow(carPrice_5), 0.7*nrow(carPrice_5))
train_carPrice<-carPrice_5[carPriceIndices,]
test_carPrice<-carPrice_5[-carPriceIndices,]

model_1<-lm(price~.,data=train_carPrice)
summary(model_1)

#Applying stepAIC function

step <- stepAIC(model_1, direction="both")

model_2<-lm(formula=price ~ car_ID + aspiration + drivewheel + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + citympg + carPrice.car_companyaudi + 
              carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companyhonda + carPrice.car_companyisuzu + 
              carPrice.car_companymazda + carPrice.car_companymercury + 
              carPrice.car_companymitsubishi + carPrice.car_companynissan + 
              carPrice.car_companypeugeot + carPrice.car_companyplymouth + 
              carPrice.car_companyporsche + carPrice.car_companyrenault + 
              carPrice.car_companysaab + carPrice.car_companysubaru + carPrice.car_companytoyota + 
              carPrice.car_companyvolkswagen + carPrice.car_companyvolvo + 
              carPrice.car_companyvw + cylindernumberfive + cylindernumbertwo + 
              fuelsystem2bbl + fuelsystemmpfi, data=train_carPrice)

summary(model_2)

#removing insignificant variables based on p-value
model_3<-lm(formula=price ~ car_ID + aspiration + drivewheel + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + citympg + 
              carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companyhonda + carPrice.car_companyisuzu + 
              carPrice.car_companymazda + carPrice.car_companymercury + 
              carPrice.car_companymitsubishi + carPrice.car_companynissan + 
              carPrice.car_companypeugeot + carPrice.car_companyplymouth + 
              carPrice.car_companyporsche + carPrice.car_companyrenault + 
              carPrice.car_companysaab + carPrice.car_companysubaru + carPrice.car_companytoyota + 
              carPrice.car_companyvolkswagen + carPrice.car_companyvolvo + 
              carPrice.car_companyvw + cylindernumberfive + cylindernumbertwo + 
              fuelsystem2bbl, data=train_carPrice)

summary(model_3)

#removing less significant variables based on p-value
model_4<-lm(formula=price ~ car_ID + aspiration + drivewheel + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companyhonda + carPrice.car_companyisuzu + 
              carPrice.car_companymazda + carPrice.car_companymercury + 
              carPrice.car_companymitsubishi + carPrice.car_companynissan + 
              carPrice.car_companypeugeot + carPrice.car_companyplymouth + 
              carPrice.car_companyporsche + carPrice.car_companyrenault + 
              carPrice.car_companysaab + carPrice.car_companysubaru + carPrice.car_companytoyota + 
              carPrice.car_companyvolkswagen + carPrice.car_companyvolvo + 
              carPrice.car_companyvw + cylindernumberfive + cylindernumbertwo + 
              fuelsystem2bbl, data=train_carPrice)
summary(model_4)
vif(model_4)

#removing highly corellated variables
model_5<-lm(formula=price ~ aspiration + drivewheel + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companyhonda + carPrice.car_companyisuzu + 
              carPrice.car_companymazda + carPrice.car_companymercury + 
              carPrice.car_companymitsubishi + carPrice.car_companynissan + 
              carPrice.car_companypeugeot + carPrice.car_companyplymouth + 
              carPrice.car_companyporsche + carPrice.car_companyrenault + 
              carPrice.car_companysaab + carPrice.car_companysubaru + 
              carPrice.car_companyvw + cylindernumberfive + cylindernumbertwo + 
              fuelsystem2bbl, data=train_carPrice)
summary(model_5)

#removing significant variables
model_6<-lm(formula=price ~ aspiration + drivewheel + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companymitsubishi + 
              carPrice.car_companypeugeot + carPrice.car_companyplymouth + 
              carPrice.car_companysubaru + cylindernumbertwo + fuelsystem2bbl, data=train_carPrice)
summary(model_6)

#removing significant variables
model_7<-lm(formula=price ~ aspiration + drivewheel + enginelocation + carwidth + 
              curbweight + enginesize + stroke + peakrpm + carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companymitsubishi + 
              carPrice.car_companypeugeot + 
              carPrice.car_companysubaru + cylindernumbertwo, data=train_carPrice)
summary(model_7)
vif(model_7)

#removing highly corelated variables
model_8<-lm(formula=price ~ aspiration + drivewheel + enginelocation + carwidth + 
              stroke + peakrpm + carPrice.car_companybmw + carPrice.car_companychevrolet + 
              carPrice.car_companydodge + carPrice.car_companymitsubishi + 
              carPrice.car_companypeugeot + 
              carPrice.car_companysubaru + cylindernumbertwo, data=train_carPrice)
summary(model_8)


#removing insignificant variables
model_9<-lm(formula=price ~ drivewheel + enginelocation + carwidth + carPrice.car_companybmw + 
              carPrice.car_companychevrolet + carPrice.car_companypeugeot, data=train_carPrice)

summary(model_9)
vif(model_9)
#So finally we came down to a model having low coorelation between them and accepted p-value.

Predict_1 <- predict(model_9,test_carPrice)
test_carPrice$test_price <- Predict_1

r<-cor(test_carPrice$price, test_carPrice$test_price)
rsquared<-r^2

test_carPrice$error<-test_carPrice$price-test_carPrice$test_price

library(ggplot2)

#A plot to view devation of predicted car Price from actual car price for test data set.
ggplot(test_carPrice, aes(test_carPrice$car_ID, test_carPrice$price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "carID") + scale_y_continuous(name = "Price") + 
  geom_line(aes(x=test_carPrice$car_ID, y=test_carPrice$test_price, colour="red"))