install.packages("Mass")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("PairedData")
library(dplyr)
library(MASS)
library(ggpubr)
library(PairedData)
cats
#grouping by values
group_by(cats, Sex) %>% summarise(count = n(),
    mean = mean(Bwt, na.rm = TRUE),
    sd = sd(Bwt, na.rm = TRUE)
  )

#visualising the data
ggboxplot(cats, x = "Sex", y = "Bwt", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Bwt", xlab = "sexes")
#checking normality of the data
with(cats, shapiro.test(Bwt[Sex== "M"]))# p = 0.1
with(cats, shapiro.test(Bwt[Sex== "F"]))# here p value is less than 0.05 so we wil use non-parametric test
#wilcox.test(non-parametric)

res <- wilcox.test(Bwt ~ Sex, data = cats,
                   exact = FALSE)
res

#show only the P-value
res$p.value
#The p-value of the test is 8.201e-11, 
#which is less than the significance level alpha = 0.05. 
#We can conclude that men's BMI is significantly different from womens BMI
#with a p-value = 8.201e-11.

#if you want to test whether the median men's Body is less than the median women's body weight, type this
wilcox.test(Bwt ~ Sex, data = cats, 
            exact = FALSE, alternative = "less")

wilcox.test(Bwt ~ Sex, data = cats, 
            exact = FALSE, alternative = "greater")

############################Part2################################################
#we have to calculate that was there is difference in slepping scores after meditation

before<-c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after<-c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),#replicating elements
  sleeping_score = c(before,  after)
)
my_data
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(sleeping_score, na.rm = TRUE),
    sd = sd(sleeping_score, na.rm = TRUE)
  )
#visualisation of data
ggboxplot(my_data, x = "group", y = "sleeping_score", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "sleeping_score", xlab = "Groups")


# Subset sleeping scores data before meditation
before <- subset(my_data,  group == "before",sleeping_score ,
                 drop = TRUE)

# Subset sleeping scores data after meditation
after <- subset(my_data,  group == "after", sleeping_score,
                drop = TRUE)

# Plot paired data
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

# compute the difference
d <- with(my_data, 
          sleeping_score[group == "before"] - sleeping_score[group == "after"])
d
# Shapiro-Wilk normality test for the differences
shapiro.test(d)#p-value=0.2177

#p-value>0.05 so it is normally distributed
# Compute t-test
res <- t.test(before, after, paired = TRUE)
res

#if the significance level changes to 0.1  then it will change and null hypothesis will be rejected


##############################THE END#########################################
