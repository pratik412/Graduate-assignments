install.packages("psych")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("caTools")
install.packages("apaTables")
install.packages("caret")
library(caret)
theme_set(theme_bw())
library(apaTables)
library(ggplot2)
library(caTools)
library(psych)
library(tidyverse)
library(ggpubr)
mydata<-read.csv("Real_estate_valuation.csv")
sum(is.na(mydata))

#drooping the record_id
mydata$ï..Record_ID<-NULL
str(mydata)
mydata$number_of_stores_nearby<-as.numeric(mydata$number_of_stores_nearby)
str(mydata)

#cor from base R
cor(mydata,method="pearson")
#on the off diagonals we can see 1 because co-relation with iteself is always 1.
#if we look at the association with house_age and distance_to_nearest_station is it 0.02
#Here we get only co-relations values and magnitude, but did not get are they stastitically significant or not
#The p-values(level of significance) in other words.

#corr.test
corr.test(mydata,method="pearson")
#here we can tell how statitically significnt the variables are with each other
#we can if the p-value is lower than level f significance then it is statically significant.
#If we want to see the confidence interval then
cormatrix<-corr.test(mydata,method="pearson")
print(cormatrix,short=FALSE)

#put correlation, p-value and CI in excel
write.csv(cormatrix$r,"correlation.csv")
write.csv(cormatrix$p,"p_value.csv")
write.csv(cormatrix$ci,"confidence interval.csv")

lowerCor(mydata,method="pearson")
#it is easier to read as it does not contains any redundancy
# we can use the below function while reporting the data
#apa.cor.table from apaTables


apa.cor.table(mydata)
#How to write to woord doc
apa.cor.table(mydata,"APA correlation Table.doc")

#############################THE END########################################

#Linear regression

#linear regression
head(mydata,5)

#visualisation
ggplot(mydata, aes(x =distance_to_nearest_station, y = house_price)) +
  geom_point() +
  stat_smooth()
#Training and testing a data
set.seed(123)
training.samples <- mydata$house_price %>%
  createDataPartition(p = 0.9, list = FALSE)#Training the data
train.data  <- mydata[training.samples, ]
test.data <- mydata[-training.samples, ]

# Build the model
model6 <- lm(house_price ~., data = train.data)
# Summarize the model
summary(model6)
# Make predictions
predictions <- model6 %>% predict(test.data)
# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$house_price)
# (b) R-square
R2(predictions, test.data$house_price)
# housing_price based on the distance_to_nearest_station 
model6 <- lm(house_price ~distance_to_nearest_station, data = train.data)

summary(model6)

##Its value ranges between -1 (perfect negative correlation: when x increases, y decreases) 
#A value closer to 0 suggests a weak relationship between the variables. 
#A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome variable (y) is not explained by the predictor (x)

summary(model6)$coef

#we predict house_price units for : 0 and 1000.
newdata <- data.frame(distance_to_nearest_station = c(0,  1000))
model6 %>% predict(newdata)

############################################################################


#Multiple regression
#model summary
model3 <- lm(house_price ~ house_age + distance_to_nearest_station +number_of_stores_nearby , data = train.data)
summary(model3)

#coeficient of significance
summary(model3)$coefficient

#confidence interval

confint(model3)

#Residual error
sigma(model3)/mean(train.data$house_price)

###########################Thank you###########################################

