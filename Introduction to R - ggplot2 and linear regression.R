#######################################################################################
# Introduction to R (Day 5)
# Author: Ryan Falck
# April 9, 2021
#######################################################################################
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone, lsmeans)
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")
data1<- read_excel("Test Data.xlsx")

data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))
 

Baseline<- subset(data2, Time==1)


#____________________________#
# Linear Regression          #
#____________________________#

#Simple regression is the same as correlation (relationship of ADAS-Cog and Age)
lm1<-lm(ADAS_COG~Age, Baseline)
summary(lm1)
cor.test(Baseline$Age, Baseline$ADAS_COG) 
sqrt(0.1425)#Note that the R-Square is the same as the correlation

#Adding a few more variables (i.e., Does Age predict baseline ADAS-Cog after accounting for sex and education?)
lm2<-lm(ADAS_COG~Age+Sex+Education, Baseline)
summary(lm2)

#Checking for assumptions (gvlma)
install.packages("gvlma")
library(gvlma)

gvlma(lm2)
plot(lm2) #plots each of the residual assumptions, and willl give you an idea for individual data points which are outliers or have leverage


#Plotting the results of our regression: An introduction to ggplot2#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
install.packages("ggplot2")
library(ggplot2)

####IMPORTANT: GRAPHING A LINEAR REGRESSION IS REALLY JUST A GRAPH OF THE RESIDUALS OF THE X AND Y VARIABLES###

#Get the residuals of ADAS-Cog: run a regression with all variables included
resid.adas<-residuals(lm(ADAS_COG~Sex+Education,Baseline))

#Get the residuals of Age: run a regression where age is the dependent variable with all covariates included
resid.age<-residuals(lm(Age~Sex+Education,Baseline))

#Create a data set of these residuals
data.resid<-as.data.frame(cbind(resid.adas, resid.age))

#Add mean ADAS-Cog or Age to each set of residuals, respectively. This will make your values >0 (it will make more sense... try running it without if you want to see)
data.resid$resid.adas<- data.resid$resid.adas + mean(Baseline$ADAS_COG)
data.resid$resid.age<- data.resid$resid.age + mean(Baseline$Age)

#Now use ggplot to plot these variables (X = Age, Y= ADAS-Cog)
ggplot(data = data.resid, aes(x = resid.age, y = resid.adas)) + 
  labs(x="Age", y="ADAS-Cog Plus Score") +geom_point(color='black') + geom_smooth(method= 'lm',  se = TRUE, color= 'black') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

