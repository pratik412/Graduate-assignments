#nstall.packages("gridExtra")
install.packages('fastDummies')

#Importing Library
library(fastDummies)
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(ggplot2)
library(moments)
library(plotrix)
library(data.table)
library(gtools)
library(gmodels)
library(vcd)
library(psych)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(e1071)
library(corrgram)
library(caret)
library(broom)
library(ggcorrplot)
library(tidyr)
library(lubridate)
library(viridisLite)
library(hrbrthemes)
library(tigerstats)
library(moments)
library(Rmisc)
library(ggpubr)
library(stringr)
library(plyr)
library(gridExtra)




#Importing dataset
death_rate = as.data.frame(read.csv('DeathRate_prediction_20210405.csv' , header = TRUE ) )
view(death_rate)

#------------Checking no. of NA's-----------------
NAcon <- sapply(death_rate, function(x) sum(is.na(x))) 
NAcon

glimpse(death_rate)
#------------------
colnames(death_rate)
names(death_rate)[1] <- "TARGET_deathRate"

#-------Seprating column Geography into 2 seprate columns by ','----
death_rate$state<- data.frame(do.call("rbind", strsplit(as.character(death_rate$Geography), ",",
                                  fixed = TRUE)))
view(death_rate)
colnames(death_rate)


#------Adding column city and state to original dataframe----
death_rate$state1 = death_rate$state$X2
#death_rate$city = death_rate$Geography2$X2
str(death_rate)
class(death_rate$state)
#-------------------------------
#------------Table------
state_tb <- table(death_rate$state1)
state_tb

#Scatter plot
par(mfrow=c(1, 2))
scatter.smooth(x=death_rate$TARGET_deathRate, y=death_rate$povertyPercent, main="Death rate~Poverty Percent")
scatter.smooth(x=death_rate$TARGET_deathRate, y=death_rate$MedianAge, main="Death rate~Poverty Percent")

par(mfrow=c(1, 2))
scatter.smooth(x=death_rate$TARGET_deathRate, y=death_rate$AvgHouseholdSize, main="Death rate~Avg household size")
scatter.smooth(x=death_rate$TARGET_deathRate, y=death_rate$povertyPercent, main="Death rate~Poverty percent")


par(mfrow=c(1, 2))  
#---For target death rate
boxplot(death_rate$TARGET_deathRate, main="Target Death")

#----For median income
boxplot(death_rate$medIncome, main="Median Income")  

par(mfrow=c(1, 2))
#-----For Avg house hold size----
boxplot(death_rate$AvgHouseholdSize, main="Avg Household income")

#-----------For poverty percent---------
boxplot(death_rate$povertyPercent, main="Poverty percent")


colnames(death_rate1)
#----Data cleaning
death_rate1 <- remove_outliers(death_rate, c('TARGET_deathRate','medIncome', 'AvgHouseholdSize', 'povertyPercent'))

#----Data cleaning
death_rate1 <- remove_outliers(death_rate1, c('TARGET_deathRate','medIncome', 'AvgHouseholdSize', 'povertyPercent'))

#----Data cleaning for only median age columns
death_rate1 <- remove_outliers(death_rate1, c('medIncome', 'povertyPercent'))


#--------------------Creating box plots-------------------
par(mfrow=c(1, 2))  
#---For target death rate
boxplot(death_rate1$TARGET_deathRate, main="Target Death Rate")

#----For median income
boxplot(death_rate1$medIncome, main="Median Income")  

par(mfrow=c(1, 2))
#-----For Avg house hold size----
boxplot(death_rate1$AvgHouseholdSize, main="Avg Household income")

#-----------For poverty percent---------
boxplot(death_rate1$povertyPercent, main="Poverty percent")

par(mfrow=c(2, 2))
#Density plot for Target death rate
ggdensity(death_rate1, x = "TARGET_deathRate", fill = "Skyblue",
          title = "Target death rate") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#Density plot for Median Income
ggdensity(death_rate1, x = "medIncome", fill = "hot pink",
          title = "Target death rate") +
  stat_overlay_normal_density(color = "green", linetype = "dashed")

#Density plot for Ang household
ggdensity(death_rate1, x = "AvgHouseholdSize", fill = "Yellow",
          title = "Avg household size") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#Density plot for Poverty percent
ggdensity(death_rate1, x = "povertyPercent", fill = "Purple",
          title = "Poverty percent") +
  stat_overlay_normal_density(color = "yellow", linetype = "dashed")


#--------------dummy variables for state1---------------------
death_rate1 <- dummy_cols(death_rate1, select_columns = 'state1')
view(death_rate1)
#----------------------Renaming column-------------------------
colnames(death_rate1)
names(death_rate1)[79] <- "S_Texas"
names(death_rate1)[46] <- "S_Georgia"

#-------Regression-------------------
linearMod <- lm(TARGET_deathRate ~ povertyPercent + AvgHouseholdSize + S_Texas  , data=death_rate1)  # build linear regression model on full data
print(linearMod)
summary(linearMod)


#sorted_bystates <- death_rate1[order(death_rate1$state1),]
states <- c(" Texas"," Georgia", " Virginia", " Wyoming")
sorted_bystates <- death_rate1[death_rate1$state1 %in% states,]
#-------Creating Regression line for Percentage of black and white
m<-sorted_bystates %>%
  ggplot(aes(x=TARGET_deathRate, 
             y=PctWhite, 
             color=state1))+
  geom_point()+
  geom_smooth(method="lm",se = FALSE)

n<-sorted_bystates %>%
  ggplot(aes(x=TARGET_deathRate, 
             y=PctBlack, 
             color=state1))+
  geom_point()+
  geom_smooth(method="lm",se = FALSE)
ggarrange(m, n, labels = c('a', 'b'))#which arrenges two plot in same frame



#Linear regression line
linearMod_G <- lm(TARGET_deathRate ~ povertyPercent + AvgHouseholdSize + S_Georgia  , data=death_rate1)  # build linear regression model on full data
print(linearMod_G)
summary(linearMod_G)


###########################################################################
#Part 2

#1. Target death rate ~ poverty percent
texas_subset <- filter(death_rate1, state1 == ' Texas',)
texas_subset1 <- select(texas_subset, TARGET_deathRate, povertyPercent)
texas_subset1
str(texas_subset1)


#Scatter plot for
scatter.smooth(x=texas_subset1$TARGET_deathRate, y=texas_subset1$povertyPercent, main="Death rate~Poverty Percent")

boxplot(texas_subset1$TARGET_deathRate, main="Target death rate")

#----Data cleaning
texas_subset1 <- remove_outliers(texas_subset1, c('TARGET_deathRate'))

#----Taking look at box plot after removing outliers
boxplot(texas_subset1$povertyPercent, main="Poverty percent")

#Performing regression analysis
pov_targetdeath <- lm(TARGET_deathRate ~ povertyPercent  , data=texas_subset1)  # build linear regression model on full data
print(pov_targetdeath)
summary(pov_targetdeath)
ggplot(texas_subset1, aes(x =povertyPercent, y =TARGET_deathRate)) +
  geom_point() +
  stat_smooth()


#2. Target death rate ~ Avg household size
texas_subset1 <- select(texas_subset, TARGET_deathRate, povertyPercent, AvgHouseholdSize)
texas_subset1
str(texas_subset1)


#Scatter plot for
scatter.smooth(x=texas_subset1$TARGET_deathRate, y=texas_subset1$AvgHouseholdSize, main="Death rate~ Avg household size")

#boxplot(texas_subset1$TARGET_deathRate, main="Target death rate")

#----Data cleaning
texas_subset1 <- remove_outliers(texas_subset1, c('AvgHoulholdSize'))

texas_subset1

#----Taking look at box plot after removing outliers
boxplot(texas_subset1$AvgHouseholdSize, main="Avg Household income")

#Performing regression analysis
avg_targetdeath <- lm(TARGET_deathRate ~ AvgHouseholdSize, data=texas_subset1)  # build linear regression model on full data
print(avg_targetdeath)
summary(avg_targetdeath)
ggplot(texas_subset1, aes(x =AvgHouseholdSize  , y =TARGET_deathRate)) +
  geom_point() +
  stat_smooth()
##################################Thank You#################################
