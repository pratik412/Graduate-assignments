install.packages("skimr")
install.packages('plotly')
install.packages('tidyverse')
install.packages('openxlsx')
install.packages("dplyr")
install.packages("pastecs")
install.packages("'ggplot2")
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

#################T-TEst###########################################


rating1<-head(Test1$rating,100) #usually we take sample data from 10% of data population
summary(rating1)

#Visualising data using box plot
library(ggpubr)
ggboxplot(rating1, 
          ylab = "rating", xlab = FALSE,
          ggtheme = theme_minimal())

# visual representation of data using QQplot(quantile-quantile plot)

mm<-ggqqplot(rating1, ylab = "Rating of wine",
         ggtheme = theme_minimal())
mm
price1<-head(Test1$price,100)
nn<-ggqqplot(price1, ylab = "price of wine",
         ggtheme = theme_minimal())
nn
ggarrange(mm, nn, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Normality is less in price than rating 1 so we will choose rating1

#compute one-sample t-test
Testing <- t.test(rating1, mu = 76)
Testing

t.test(rating1, mu = 76,     alternative = "greater")

t.test(log(rating1),mu = log(76),     alternative = "greater")

wilcox.test(rating1, mu = 76, alternative = "greater")

t.test(rating1, mu = 76,     alternative = "less")

t.test(log(rating1),mu = log(76),     alternative = "less")
#using differnt test to ensure nothing is biased
wilcox.test(rating1, mu = 76, alternative = "less")
# both of these results are agree with our hand calculations from earlier.

###########################Hypothesis testing################################################
#Lets take the variable as rating and then look for t-value
rating1<-head(Test1$rating,50) #usually we take sample data from 10% of data population
rating1
summary(rating1)
#Lets assume the value of 86
t <- (mean(rating1)-86)/(sd(rating1)/sqrt(length(rating1)))
t
ptwo<-2*pt(-abs(t),df=length(rating1)-1)# this will give us two sides T-test
ptwo
pvalue<- pt(-abs(t),df=length(rating1)-1)# this gives us value for one sided T-test
pvalue

##############Maximum prices of wine#######################################
arrenging_data<- arrange(Test1, desc(price))# arrenge the data in descending order of price
arrenging_data
Test3<- select(arrenging_data, price,country)#selecting price and country from arrenged data
Test3
names(Test3)
sum(is.na(Test3))# looking for na values
unique(Test3$country)# looking for unique values
Test3[Test3==""]<-NA# making spaces or blanks as na
Test3
sum(is.na(Test3))
Test3<-na.omit(Test3)#then omiting na
sum(is.na(Test3))
#grouping the countries by maximum prices
myhighdata<-Test3%>%group_by(country)%>%summarise(Price=max(price))
#GGplot that shows maximum prices of wine vs countries
m<-ggplot(data=myhighdata,aes(x=country,y=Price))+geom_bar(stat='identity',fill="steelblue")+theme_light()+
  ggtitle('Max. prices of wine vs countries')+ylab('Max. price of wine')+
         theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
               plot.title =element_text(hjust=0.5))
m       
############################################################################



