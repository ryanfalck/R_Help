#######################################################################################
# Introduction to R (Descriptive Statistics)
# Author: Ryan Falck
#######################################################################################
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone)
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")
data1<- read_excel("Test Data.xlsx")

data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))

#_________________________________#
#     Basic Descriptive Stats     #
#_________________________________#

#Before we do anything else, let's start with the original data
data1<- read_excel("Test Data.xlsx")


#As a rule of thumb, make sure that dplyr and plyr are not loaded unless you plan to use them for something in particular
detach(dplyr)
detach(plyr)

pacman::p_load(readxl, lme4,car, broom, ggplot2, psych, hmisc, tableone)


#Let's all use the same data set, for clarity. We'll also only look at baseline... easier to interpret this way.
Baseline<-subset(data1, Time == 1)

colnames(Baseline)
#rename those pesky variables from last time
Baseline<-rename(Baseline, c("Height (cm)"="Height", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total"="ADAS_COG"))

#Basic descriptives for continuous variables
describe(Baseline$Age)

#We can also use describeBy which can provide a cleaner view of a variable
describeBy(Baseline$Age)

#Or we can use describeBy to look at everything together
describeBy(Baseline)

#Summary provides the five number summary for everything we're interested in
summary(Baseline$Age)
summary(Baseline)

#Histograms also provide some nice visual view of the data
hist(Baseline$Age)

#We cna also view categorical variables in tables
table(Baseline$Group)
table(Baseline$Sex)

prop.table(table(Baseline$Sex))


#We can also use the describeBy function to describe a variable by group (or anything else)

describeBy(Baseline$Age, Baseline$Group)

table(Baseline$Sex, Baseline$Group)
prop.table(table(Baseline$Sex, Baseline$Group), margin=2)


#Table One - Making life easy for summarizing group differences
colnames(Baseline)
vars<-dput(names(Baseline[c(4,9,10,11)]))
catVars<-dput(names(Baseline[c(5,6)]))

Table1_continuous<-CreateTableOne(vars=vars, strata = "Group", data=Baseline)
print(Table1_continuous,contDigits=2,missing=TRUE,quote=TRUE)

Table1_category<-CreateTableOne(vars=catVars, strata = "Group", data=Baseline)
print(Table1_category,exact=catVars,contDigits=2,missing=TRUE,quote=TRUE) #These are reported as means so we have to remake these variables a character variables first


Baseline$Sex[Baseline$Sex==1]<-"Male" #we have to make these into categorical variables first
Baseline$Sex[Baseline$Sex==0]<-"Female"

Baseline$Education[Baseline$Education==3]<- "High School or Less" #we have to make these into categorical variables first
Baseline$Education[Baseline$Education==2]<- "Trade School"
Baseline$Education[Baseline$Education==1]<- "University"

#Now let's try this again...(so we can look at group differences in sex and education)
catVars<-dput(names(Baseline[c(5,6)]))
Table1_category<-CreateTableOne(vars=catVars, strata = "Group", data=Baseline)
print(Table1_category,exact=catVars,contDigits=2,missing=TRUE,quote=TRUE)

#_________________________________________________#
#PRACTICE QUESTIONS (NOTE: USE BASELINE DATA ONLY)#
#_________________________________________________#

#1) Calculate the Mean, Median, SD,Skew, and Kurtosis for BMI in each group? What is the proportion for the full sample?

#2) What is the proportion of people with a High School Education (i.e., Education == 1) or less in the Intervention Group?

#3) What is the proportion of people with a University Degree (i.e., Education == 3) in the Full Sample?

#4) How many women (Sex == 0) have a university degree in the full sample? How many women have a university degree in the Control Group?

#5) What is the average ADAS-Cog score for women in the full sample? What about for men?

#6) What is the average ADAS-Cog score for men in the intervention group? For women in the control group?

#7) Use the table one package to create a table with the following variables (in order): 6MWT, Age, ADAS-CoG, and Weight.





#__________________________________#
#Correlations                      #
#__________________________________#

#Easy ways to find correlations
cor(Baseline$Age, Baseline$ADAS_COG) #Will give the pearson's r (no mention of p-value)
cor.test(Baseline$Age, Baseline$ADAS_COG) #Provides' p-value, df, correlation, and the 95% CI

#An Introduction to Scatterplots
plot(Baseline$Age, Baseline$ADAS_COG, main= "My First Scatter Plot") #Simple scatterplot of Age (x axis) and ADAS-Cog (y axis)
plot(Baseline$Age, Baseline$ADAS_COG, main= "My First Scatter Plot", 
     xlab="Age", ylab="ADAS-Cog Total", pch = 19) #Add labels and change the plot points


abline(v= mean(Baseline$Age)) #vertical line where mean age is
abline(h= 5, col="green", lty= 2, lwd= 4) #horizontal line at ADAS-Cog score of 5

regline<-lm(ADAS_COG~Age, Baseline) #Simple regression of Age (IV) and ADAS-Cog (DV)
abline(regline) #now let's add that to the plot
abline(lm(ADAS_COG~Age, Baseline), col="blue", lty = 4, lwd=5)#We could also add this directly to the plot if we wanted to, without having to run the regression first
lines(lowess(Baseline$Age, Baseline$ADAS_COG),col="red")#Lowess line 


#Scatterplot Matrices Using the package lattice (we can look at more scatterplots across more variables)
library(lattice)
splom(Baseline[c(4,9,10,11)])
splom(Baseline[c(4,9,10,11)], groups=Baseline$Group)


#More data visualization....using Performance Analytics
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

Baseline.chart<-subset(Baseline, select = c(1,3,4,9:11))

chart.Correlation(Baseline.chart[,3:6], histogram=TRUE, pch=19)


#__________________________________#
#t-test's and chi-square           #
#__________________________________#

#One Sample t-test
t.test(Baseline$Age)

#Two Sample t-test
t.test(Baseline$Age~Baseline$Group)

#You can also do this with Table One
agevar<-dput(names(Baseline[c(4)]))

Table_one_age_diff<-CreateTableOne(vars=agevar, strata = "Group", data=Baseline)
print(Table_one_age_diff,contDigits=2,missing=TRUE,quote=TRUE)


#Chi-square test
chisq.test(x = table(Baseline$Education, Baseline$Group))

#You can also do this with Table One (note that it uses Fisher's Exact Test... can sometimes lead to slightly different p-value)
educationvar<-dput(names(Baseline[c(6)]))

Table_one_education_diff<-CreateTableOne(vars=educationvar, strata = "Group", data=Baseline)
print(Table_one_education_diff,contDigits=2,missing=TRUE,quote=TRUE)




#___________________________________#
#ANOVA and ANCOVA                   #
#___________________________________#

#Fortunately, ANOVA and ANCOVA use the same mathematics as regression


#Let's first look at whether there are differences in ADAS-Cog based on group at baseline

group.lm<- lm(ADAS_COG~Group, Baseline)
anova(group.lm)

sqrt(0.0099)
t.test(Baseline$ADAS_COG~Baseline$Group)#notice that the t-value is the same as the f-value


#The point is that this is all based on the same mathematics. That's why a lot of the coding for linear regression
#and for ANOVA/ANCOVA is the same in R (same mathematical structure). We'll discuss this more next time when we talk
#about linear regression.

#There's a lot more that we'll discuss about ANOVA too (don't worry). Today is just a primer :)



#____________________#
# PRACTICE QUESTIONS
#____________________#

#1) What's the correlation between BMI and Meters Walked at Final (i.e., Time = 3)



#2) Make a scatterplot of Age (X) and BMI (Y) at Baseline (Time = 1).



#3) Include a regression line in your scatter plot (make it blue), a vertical line at mean age (in red), and a horizontal line at BMI=27 (in green).


#4) Include a Lowess curve on the scatter plot (black dotted line and make it much larger than your other lines)


#5) OK now let's make a new scatter plot. This time of ADAS-Cog and Meters Walked at Midpoint (Time = 2). However, this time see if you can get separate dots 
#   for each group (i.e., INT and CON). You may need to google how to do this. Also see if you can make separate regression lines for each intervention group.


#6) Use Performance Analytics to get the correlations and histograms of BMI, Age, and ADAS-Cog at Final.


#7) Is there a significant difference in ADAS-Cog between Males and Females at Final? What's the t-value and p-value?


#8) Is there a significant difference in ADAS-Cog between males and females when we control for Age at Final? What's the F-value and p-value?


#9) Does the model looking at sex differences in ADAS-Cog (controlling for Age) meet the assumptions of ANOVA?


#10) Do males and females have different education levels at baseline? 



