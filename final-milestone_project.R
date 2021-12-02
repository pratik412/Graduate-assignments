#########################Final Milestone######################################

#Install packages and libraries

install.packages("skimr")
install.packages('plotly')
install.packages('tidyverse')
install.packages('openxlsx')
install.packages("dplyr")
install.packages("pastecs")
install.packages("'ggplot")
install.packages("psych")
install.packages("caret")
install.packages('splines')
library(splines)
library(caret)
theme_set(theme_bw())
library(psych)
library(ggplot2)
library(pastecs)
library(dplyr)
library(skimr)
library(tidyverse)
library(openxlsx)
library(plotly)

# Craeting a variable to store the data
Test1<-read.csv('Wine_tasting-1.csv',header=TRUE)
Test1

#Assign better coloumn names
names(Test1)[1]<-"Index"
names(Test1)[5]<-"rating"
names(Test1)[13]<-"variety_of_wine"

#variable names in data
names(Test1)

#Correct examination of data
skim(Test1)

#3. Removing missing values/NA's
#Identifying NA with complete cases function
complete.cases(Test1)

#It gives summary of data, exacts numbers of rows and coloumns
summary(Test1)

#identify rows with NA's for review
Test1NA<- subset(Test1,is.na(Test1$price))

#Sum of NA values
sum(is.na(Test1))

# Here we are putting mean values in the place of NA values

mean(Test1$price,na.rm=TRUE)

#Replacing Na's with the mean of price

Test1[is.na(Test1)]<-mean(Test1$price,na.rm=TRUE)
Test1
sum(is.na(Test1))
# we will see the data in the variable is normal or not
#there is two way we can check the normality

###############################################################################

#linear regression
#Training and testing a data
set.seed(123)
training.samples <- Test1$price%>%
  createDataPartition(p = 0.9, list = FALSE)#Training the data
train.data  <- Test1[training.samples, ]
test.data <- Test1[-training.samples, ]
# Price is dependent and rating as an independent
model <- lm(price ~rating, data = train.data)

#summary of the model
summary(model)

##Its value ranges between -1 (perfect negative correlation: when x increases, y decreases) 
#A value closer to 0 suggests a weak relationship between the variables. 
#A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome variable (y) is not explained by the predictor (x)

#Coeficient of the model
summary(model)$coef

#Visualisation
ggplot(Test1, aes(x =rating, y = price)) +
  geom_point() +
  stat_smooth()

# The linear model does not fit here
#Lets try som non-linear models and comapre

# Build the model
model2 <- lm(price ~ rating, data = train.data)

# Make predictions
predictions <- model2 %>% predict(test.data)

# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

#visualise the data
ggplot(train.data, aes(rating, price) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

#non-linear regression
#polynomial regression

#Model building
lm(price ~ poly(rating, 2, raw = TRUE), data = train.data)

#it will compute sixth order polynomial fit
lm(price ~ poly(rating, 6, raw = TRUE), data = train.data) %>%
  summary()

#  it can be seen that polynomial terms beyond the fith order are not significant.
#So, just create a fith polynomial regression model 
model <- lm(price ~ poly(rating, 5, raw = TRUE), data = train.data)

# Make predictions
predictions <- model %>% predict(test.data)

# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)
ggplot(train.data, aes(rating, price) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))

# coefficient of the model
summary(model)$coef

#predicting the data
newdata<-data.frame(rating=c(90,100))
model %>%predict(newdata)

#Test if the model with ratig^2 is significantly better or not
#using F-partial test
anova(model,model2)

# 2.Whether their is relation between rating and price in Spain
df2 <- Test1 %>%
  filter(country=="Italy") %>%
  select(country,rating, price)
df2

#visualisation
ggplot(df2, aes(x =rating, y = price)) +
  geom_point() +
  stat_smooth()

#We can graphically say that the price is increases with the rating

#Training and testing a data
set.seed(123)
training.samples <- df2$price %>%
createDataPartition(p = 0.76, list = FALSE)#Training the data
train.data  <- df2[training.samples, ]
test.data <- df2[-training.samples, ]


# Build the model
model8 <- lm(price ~rating, data = train.data)

# Summarize the model
summary(model8)
 
# Make predictions
predictions <- model8 %>% predict(test.data)



##Its value ranges between -1 (perfect negative correlation: when x increases, y decreases) 
#A value closer to 0 suggests a weak relationship between the variables. 
#A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome variable (y) is not explained by the predictor (x)

summary(model8)$coef

#Predicting the new data
newdata<-data.frame(rating=c(90,100))
model8 %>%predict(newdata)
