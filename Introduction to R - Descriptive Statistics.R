#######################################################################################
# Introduction to R (Day 2)
# Author: Ryan Falck
# December 4, 2020
#######################################################################################


updateR()

#______________________________#
#Answers to the Problems#
#______________________________#

#Let's load the data first...
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone)
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")
data1<- read_excel("Test Data.xlsx")

#1) Turn sex (gender) into a categorical variable?
data1$gender<-NA
data1$gender[data1$Sex==1]<-"Male"
data1$gender[data1$Sex==0]<-"Female"
data1$gender<-as.factor(data1$Sex)
View(data1)

#2) Reorder the education variable with 3 being the lowest education (i.e., high school) and 1 being the highest education (e.g., college)
data1$EDU<- NA
data1$EDU[data1$Education==3]<- "High School or Less"
data1$EDU[data1$Education==2]<- "Trade School"
data1$EDU[data1$Education==1]<- "University"
View

#3) Try to rename the variables of Education.v2 into something shorter (e.g., HS, College, PhD)
#Here's what we did originally...
data1$Education.v2<- NA
data1$Education.v2[data1$Education==1]<- "High School or Less"
data1$Education.v2[data1$Education==2]<- "Trade School"
data1$Education.v2[data1$Education==3]<- "University"

#Here's one way of doing it... (there are others)
data1$Education.v3<- NA
data1$Education.v3[data1$Education.v2=="High School or Less"]<- "HS"
data1$Education.v3[data1$Education.v2=="Trade School"]<- "Trade"
data1$Education.v3[data1$Education.v2=="University"]<- "College"



#4) Rename the variable Education.v2 into EDU
#A reminder of how we can rename variables (note: sometimes you have to switch the order of "new" = "old" to "old" = "new" or vice versa)
data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))

#let's remove the old version of EDU... since we already have a variable named EDU
data2<-subset(data1, select = -c(EDU))

#Rename Education.v2 to EDU
data3<-rename(data2, c("EDU"="Education.v2", "Meters_Walked" = "6MWT"))


#5) Meters walked is currently in meters, lets turn it into km (i.e., divide it by 1000). We can call this new variable Walked_Km

data3$walked_km<- data3$Meters_Walked/1000

#6) Make a new data set with only ID, Age, Sex, EDU, and Walked_Km included. Only include data from Timepoint 2.
data4<-subset(data3, select = c(1,4))
Midpoint<-subset(data4, Time == 2)

data3<-rename(Midpoint,c("Meters_Walked" = "walked_km"))

Midpoint$Older



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