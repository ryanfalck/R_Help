#######################################################################################
# Introduction to R (Day 1)
# Author: Ryan Falck
# November 20, 2020
#######################################################################################

#__________________________________________#
#             Getting Started
#__________________________________________#

#Step 1: Loading and installing packages 

      #Option 1: Using Code#
install.packages("installr")
library(installr)
library(tidyverse)

      #Option 2: Using Pacman#
if (!require("pacman")) install.packages("pacman") 
#(Translation: If the pacman program is not already installed, then install the program)


pacman::p_load(readxl, plyr, lme4,nlme, robustlmm, car, broom, lsmeans, ggplot2, psych, HLMdiag, tableone,mice)
#(The Pacman program installs and loads any package which is not already loaded on your R program)


#Step 2: Setting the working directory (i.e., where is your data located on your computer?)

    #Intermediate - Using Code#
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")



#Step 3: Load your data sheet (Excel or CSV file)
data1<- read_excel("Test Data.xlsx")


#There are other ways to load your data, and there is no "correct" way to do this. Use the method
#which you feel most comfortable with!



#__________________________________________#
#             Basic Data Management
#__________________________________________#

#What does your data look like?
View(data1)
data1



#What are the variable names?
colnames(data1)



#How do I look at one variable?
data1$Age
View(data1$Age)



#What type of data is it?
typeof(data1$Age)
typeof(data1$Time)
typeof(data1$ID)




#Delete a variable

  #Option 1: Subsetting
data2<- subset(data1, select = -c(Age))
View(data2)

data2<- subset(data1, select = c(ID, Time, Group, Age, Sex, Education))
View(data2)

  #Option 2: Calling variables

data2<-data1[c(1:6)]

data2<-data1[c(1,2,3,4,5,7,8,9,10,11)]

data2<-data1[c(1:5,7:11)]





#Renaming Variables
data1$`Height (cm)` #These variable names are long and will be pain to enter by hand. We should rename them.
data1$`Weight (kg)`
data1$`BMI (kg/m2)`
data1$6MWT#Note: variables are not allowed to start with a number! We'll need to rename this one.


    #Option 1: The Rename Function

data2<-rename(data1, c("Height"="Height (cm)", "Weight"="Weight (kg)", "BMI" = "BMI (kg/m2)" , "Meters_Walked" = "6MWT"))
View(data2)


    #Option 2: The spaceless function - remove spaces
spaceless <- function(x) {colnames(x) <- gsub(" ", "_", colnames(x));x}
data2<-spaceless(data1)
View(data2)




#Here's a good version of the dataset to use for right now
data2<-rename(data1, c("Height"="Height (cm)", "Weight"="Weight (kg)", "BMI" = "BMI (kg/m2)" , "Meters_Walked" = "6MWT", "ADAS_Cog" = "ADAS-Cog Total"))





#Making new variables (continuous variable)
data2$BMI.v2<- (data2$Weight)/((data2$Height/100)^2) #Note that BMI is being calculated with "sitting height", so this won't make sense
View(data2)



#Making new variables (continuous to character variable)
data2$Education.v2<-NA
View(data2)

data2$Education.v2[data2$Education==1]<- "High School or Less"
data2$Education.v2[data2$Education==2]<- "University"
data2$Education.v2[data2$Education==3]<- "Graduate School"
View(data2)






#Making new variables (character to continuous variable)
data2$Education.v3<-NA
data2$Education.v3[data2$Education.v2=="High School or Less"] <- 1
data2$Education.v3[data2$Education.v2=="University"] <- 2
data2$Education.v3[data2$Education.v2=="Graduate School"] <- 3





#Making continuous variable into a factor
typeof(data2$Education.v3)
data2$Education.v3<-as.factor(data2$Education.v3)




#Removing participants
data3<-subset(data2, ID!="T_003")
data3<-subset(data2, !is.na(Meters_Walked))
data3<-subset(data2, is.na(Meters_Walked))





#Attach data and detach data - this helps when you have your dataset all set up and you no longer
#want to deal with using the $ sign every time to call a variable

attach(data3)

Age
Sex
Education.v2

detach(data3)

Age #This no longer works now that the dataset is detached

data3$Age #Need to call the dataframe first




#Only include data from timepoint 1 (i.e., Baseline)

Baseline<-subset(data2, Time == 1)



#Creating a new excel worksheet (Baseline Data only)- Make sure you have downloaded and installed the openxlsx package, and loaded it beforehand

library(openxlsx)
write.xlsx(Baseline, "Baseline.xslx")

#______________________________#
#Practice Problems#
#______________________________#

#1) Turn sex (gender) into a categorical variable?


#2) Reorder the education variable with 3 being the lowest education (i.e., high school) and 1 being the highest education (e.g., college)


#3) Try to rename the variables of Education.v2 into something shorter (e.g., HS, College, PhD)


#4) Rename the variable Education.v2 into EDU


#5) Meters walked is currently in meters, lets turn it into km (i.e., divide it by 1000). We can call this new variable Walked_Km


#6) Make a new data set with only ID, Age, Sex, EDU, and Walked_Km included. Only include data from Timepoint 2.

