#####Week-2 module####

install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("treemap")
install.packages("psych")
install.packages("shiny")
install.packages('magick')
install.packages("tesseract")
install.packages("ggpubr")
library('ggpubr')
library(magick)
library(tesseract)

library(shiny)
library(psych)
library(treemap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)

#Reading file
df<- read.csv("Baseline survey (2).csv")
df

dframe <- as.data.frame(df)    #Converts objects- such as vector, list, matrices and factors to data frame
show(dframe)                 

glimpse(dframe)   #glimpse function is transposed version of print()
str(dframe)       # str() displays internal data structure.\

#---------------------------------------
#-----------Filtering out unwanted columns using select()----------------
df2 = select(dframe,Term, Class, ID, Answer1, Answer2, Answer3, Answer4, Answer5, Answer6, Answer7, Answer8, Answer9)
df2

#---------------Renaming columns------------------
colnames(df2)  # old column names
names(df2)[names(df2) == "Answer1"] <- "Undergrad_Majors"
names(df2)[names(df2) == "Answer2"] <- "Age"
names(df2)[names(df2) == "Answer3"] <- "Work_Exp"
names(df2)[names(df2) == "Answer4"] <- "Stats_and_Anal_Exp"
names(df2)[names(df2) == "Answer5"] <- "RProg_Exp"
names(df2)[names(df2) == "Answer6"] <- "Cen_tendancy"
names(df2)[names(df2) == "Answer7"] <- "varience"
names(df2)[names(df2) == "Answer8"] <- "Bivariate"
names(df2)[names(df2) == "Answer9"] <- "probability"
colnames(df2)   #new column names

#Now we will do data cleaning

df2$Class <- factor(df2$Class)

#Cleaned column Answer 1
df2$Undergrad_Majors <- gsub('<Unanswered>',
                     NA,
                     as.character(df2$Undergrad_Majors))

df2   

#Cleaned column Answer 2
df2$Age <- as.numeric(gsub('<Unanswered>',
                           NA,
                           df2$Age))

#Cleaned column Answer 3
df2$Work_Exp <- as.numeric(gsub('<Unanswered>',
                                NA,
                                df2$Work_Exp))
class(df2$Work_Exp)
View(df2)

#Cleaned column Answer 4
df2$Stats_and_Anal_Exp <- as.numeric(gsub('<Unanswered>',
                                     NA,
                                     as.character(df2$Stats_and_Anal_Exp)))

class(df2$Stats_and_Anal_Exp)
View(df2)
#Cleaned column Answer 5
class(df2$RProg_Exp)
df2$RProg_Exp <- as.numeric(gsub('<Unanswered>',
                                 NA,
                                 as.character(substring(df2$RProg_Exp,
                                                        1,
                                                        1))))
View(df2)

#Cleaned column Answer 6
df2$Cen_tendancy <- round(as.numeric(gsub(" ",
                                    NA,
                                    substring(df2$Cen_tendancy,
                                              1,
                                              1))),2)
View(df2)
class(df2$Cen_tendancy)

df2$Cen_tendancy <- as.numeric(ifelse(df2$Cen_tendancy>5,
                                NA,
                                df2$Cen_tendancy))


#Cleaned column Answer 7
df2$varience <- round(as.numeric(gsub(" ",
                                     NA,
                                     substring(df2$varience,
                                               1,
                                               1))),2)
View(df2)
class(df2$varience)

#Cleaned column Answer 8
df2$Bivariate <- as.numeric(gsub('<Unanswered>',
                              NA,
                              as.character(substring(df2$Bivariate,
                                                     1,
                                                     1))))
class(df2$Bivariate)

#Cleaned column Answer 9
df2$probability <- as.numeric(gsub(" ",
                                NA,
                                df2$probability),
                           digits=-3)

class(df2$Bivariate)

df3<-head(df2)
View(df3)
#---------------------------------
#-------------------------------

#Clearing NA values
sum(is.na(df2))
df2<-na.omit(df2)
sum(is.na(df2))


#Describing the class which gives us mean, median, standard deviation etc
class(df2$Age)
describe(df2$Age)

#------------creating subset as per intake----------------------------------------
class1 <- subset(df2, df2$Class == 1)
class1
class2 <- subset(df2, df2$Class == 2)
class2
class3 <- subset(df2, df2$Class == 3)
class3
class4 <- subset(df2, df2$Class == 4)
class4
class5 <- subset(df2, df2$Class == 5)
class5
 ##----------------

#------------Descriptive statistics------------
class_tb <- describeBy(df2, df2$Class)
class_tb

term_tb <- describeBy(df2, df2$Term)
term_tb

major_tb <- describe.by(df2, df2$Undergrad_Majors)
major_tb


##Three line table format (with xtab)
table_2<-xtabs(~ Age+Undergrad_Majors+Stats_and_Anal_Exp, data=df3)
table_2<-ftable(table_2)
table_2


##Visualisation##########
par(mfrow=c(1,2))
plot(df2$Age,df2$Work_Exp, xlab = "Age of Student", 
     ylab = "Work Experience", main = "Age Vs Work Expereince",
     pch=16, col = "green", xlim = c(20,30),ylim = c(0,20))
abline(lm(df2$Work_Exp ~ df2$Age), col="red")


plot(df2$Age,df2$RProg_Exp, xlab = "Age of Student", 
     ylab = "R prgm Experience", main = "Age Vs R_prgm exp",
     pch=16, col = "blue", xlim = c(20,30),ylim = c(0,20))
abline(lm(df2$RProg_Exp ~ df2$Age), col="Brown")



#Visualization2
#GGplot
ss<-ggplot(data=df2,mapping=aes(x=Undergrad_Majors, y=RProg_Exp,color=RProg_Exp))+
  geom_jitter(size=2.5)+
  scale_color_gradient(low="red",high="green")+
  labs(title="Majors and Rprog_xperience ",xlab="Undergrad_majors",ylab="Rprogramming experience",color="Rprog_experience")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))
ss
mm<-ggplot(data=df2,mapping=aes(x=Undergrad_Majors, y=Work_Exp,color=RProg_Exp))+
  geom_jitter(size=2.5)+
  scale_color_gradient(low="green",high="red")+
  labs(title="Experience by major ",xlab="Undergrad major",ylab="Normal work experience",color="Rlang experience")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

mm


#histograms

p<-ggplot(data=df2,aes(x=Cen_tendancy))+ geom_histogram(fill="green",col="red")+ 
theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

p
q<-ggplot(data=df2,aes(x=probability))+ geom_histogram(fill="blue",col="pink")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

q
r<-ggplot(data=df2,aes(x=Bivariate))+ geom_histogram(fill="pink",col="yellow")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

r
s<-ggplot(data=df2,aes(x=varience))+ geom_histogram(fill="black",col="white")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

s
t<-ggplot(data=df2,aes(x=RProg_Exp))+ geom_histogram(fill="yellow",col="black")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

t
ggarrange(p, q, r,s, 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

#Bar_plot
#1 Bar plot to measure counts of major
i<-ggplot(data=df2,aes(x=Undergrad_Majors,fill=Undergrad_Majors))+geom_bar()+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

i
#Bar plot to measure beginner to expert in probability and distribution w.r.t.major
j<-ggplot(data=df2,aes(x= probability,fill=Undergrad_Majors))+geom_bar()+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

j
#Bar plot to measure beginner to expert in variety w.r.t.major
k<-ggplot(data=df2,aes(x= varience,fill=Undergrad_Majors)) +geom_bar(show.legend = FALSE)+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

k
#Bar plot to measure beginner to expert in bivariate concepts w.r.t.major
l<-ggplot(data=df2,aes(x= Bivariate,fill=Undergrad_Majors)) +geom_bar(show.legend = FALSE)+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

l
m<-ggplot(data=df2,aes(x=Cen_tendancy,fill=Undergrad_Majors))+geom_bar(show.legend=FALSE)+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

m
ggarrange(j,k,l,m,
          labels = c("W", "X","Y","Z"),
          ncol = 2, nrow = 2)
#BarPlot
ggplot(data=df2,aes(x=Work_Exp,fill=Undergrad_Majors))+geom_bar()
ggplot(data=df2,aes(x=RProg_Exp,fill=Undergrad_Majors))+geom_bar()+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))

#box_plot
ggplot(data=df2,aes(x=Undergrad_Majors,y=Age,col=Undergrad_Majors))+geom_boxplot()+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))


#scatter_plot
pp<-ggplot(data=df2,aes(x=Stats_and_Anal_Exp,y=RProg_Exp,col=RProg_Exp))+geom_point(size=2.5)+scale_color_gradient(low="red",high="green")+
  theme_light()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.title =element_text(hjust=0.5))+geom_smooth(method='lm',se=FALSE)
pp


#############################Thank you##########################

