#######################################################################################
# Introduction to R (RCT Analyses: ANCOVA and Mixed Linear Modeling)
# Author: Ryan Falck
#######################################################################################
detach("package:dplyr", unload = TRUE)
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone, lsmeans)
setwd("/Users/ryan/Desktop/UBC-Postdoctoral Fellowship/R Help/Youtube R Videos/Data and R Code")
data1<- read_excel("Test Data.xlsx")

data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))

data2$Education[data2$Education==3]<- "High School or Less"
data2$Education[data2$Education==2]<- "Trade School"
data2$Education[data2$Education==1]<- "University"

data2$Sex[data2$Sex==1]<-"Male"
data2$Sex[data2$Sex==0]<-"Female"

#For these exercises, let's assume that we ran an intervention to improve walking fitness (i.e., 6MWT or "Meters Walked"). A secondary outcome would be cognitive
#performance measured using the ADAS-Cog. There were three time points: Baseline (Time=1), Midpoint (Time=2; 3 months), and Final (Time=3; 6 months).
#The primary endpoint of the intervention is 6 months.INT= experimental group; CON= control group.

#____________________________#
# ANCOVA                     #
#____________________________#

#First, we need to set up the data for ANCOVA. 
#This means we need our data to be switched from a long format to a wide format

#Creates a data subset of data for timevarying variables
varying<-data2[c(1,2,10:11:length(data2))]

#Creates a data subset for variables which do not vary over time
baseline<-subset(data2[-c(10:11:length(data2))], Time==1)

#Creates a data subset for variables which vary over time at each timepoint
varying.1 <- subset(varying,Time==1)
varying.2 <- subset(varying,Time==2)
varying.3 <- subset(varying,Time==3)

#Label time varying variables at each time point (e.g., ADAS-Cog.1, ADAS-Cog.2, ADAS-Cog.3...)
colnames(varying.1) <- paste(colnames(varying.1),"1",sep=".")
colnames(varying.2) <- paste(colnames(varying.2),"2",sep=".")
colnames(varying.3) <- paste(colnames(varying.3),"3",sep=".")


#Create wide dataset including all timepiints
library(dplyr)
wide.data <- left_join(baseline,varying.1,by=c("ID"="ID.1")) %>% 
  left_join(.,varying.2,by=c("ID"="ID.2")) %>% 
  left_join(.,varying.3,by=c("ID"="ID.3"))

Timevars <- grep("Time",colnames(wide.data),value=TRUE)
wide.data <- wide.data[,!(colnames(wide.data)%in%Timevars)]

colnames(wide.data) <- (gsub("_","",colnames(wide.data)))
colnames(wide.data) <- (gsub(".1","baseline",colnames(wide.data)))


#Now that we have our data set-up, we can look at the effects of this intervention to improve walking fitness. 
#As a reminder, our primary endpoint is 6 months (MetersWalked.3). We are thus going to look at the effects
#of the intervention on walking fitness following 6 months training, while controlling for baseline walking fitness.

#As per usual, we would first run some descriptive statistics to describe our groups. The package Tableone can be used
#to look at baseline group data; however, we should ignore the hypothesis tests given for between group differences.

Baseline<-subset(data2, Time == 1)

#For simplicity, our baseline descriptors which we're interested in are Age, BMI, Sex, Education, ADAS_Cog, and Meters_Walked
vars<-dput(names(Baseline[c(4:6,9,10,11)]))

Table1_continuous<-CreateTableOne(vars=vars, strata = "Group", data=Baseline)
print(Table1_continuous,contDigits=2,missing=TRUE,quote=TRUE)

#Now that we'ver described our sample, let's run our primary model where we are interested in the effects of the program on 
#walking fitness following 6 months training, while controlling for baseline walking performance. 

#Basic ANCOVA model
model1<-lm(MetersWalked.3~Group + MetersWalkedbaseline, data=wide.data)
summary(model1)

#The results suggest that the intervention did not improve walking fitness. However, we think that maybe
#age and sex might account for some of the underlying variance in improvements in walking from the intervention so let's
#include them now as covariates

#ANCOVA model with additional covariates
model2<-lm(MetersWalked.3~Group + MetersWalkedbaseline + Age + Sex, data=wide.data)
summary(model2)

#Still no effect, but let's go ahead and see what the between group differences are. We would look at this
# by using the lsmeans procedure.

lsmeans(model2, ~Group)

#We can also contrast the lsmeans to determine the between group differences at 6 months in walking fitness
contrast(lsmeans(model2, ~Group), "trt.vs.ctrl", adjust = "none") 

#We can also see the confidence intervals for the differences if we want
contrast.1<-contrast(lsmeans(model2, ~Group), "trt.vs.ctrl", adjust = "none") 
confint(contrast.1, parm, level = 0.95)

#Looking at the effects fo the ANCOVA, there doesn't seem to be any between group difference in walking fitnes.
#However, if we look closely at the data, there are quite a few people (INT=3; CON=3) with missing data at 6 months.

describeBy(wide.data$MetersWalked.3, wide.data$Group)

#This means that are ANCOVA is only including 34 people in the final analysis, and we need to make sure everyone is included.
#A simple fix for this is to use last observation carried forward (LOCF) or baseline carried forward (BCF). We can do this pretty
#easily and then re-run the analysis. The downside to this approach is that this may inaccurately represent that individual's 
#performance at that timepoint.

#We run both LOCF and BCF below
wide.data2<-wide.data %>%
  mutate(MetersWalkedLOCF.3 = if_else(is.na(MetersWalked.3), MetersWalked.2, MetersWalked.3),
         MetersWalkedBCF.3 = if_else(is.na(MetersWalked.3), MetersWalkedbaseline, MetersWalked.3)
  )

describeBy(wide.data2$MetersWalkedLOCF.3, wide.data2$Group)
describeBy(wide.data2$MetersWalkedBCF.3, wide.data2$Group)


#We can the re-run our analysis to see if the results change after imputing using LOCF or BCF. 

#LOCF
model3<-lm(MetersWalkedLOCF.3~Group + MetersWalkedbaseline + Age + Sex, data=wide.data2)
contrast(lsmeans(model3, ~Group), "trt.vs.ctrl", adjust = "none") 

#BCF
model4<-lm(MetersWalkedBCF.3~Group + MetersWalkedbaseline + Age + Sex, data=wide.data2)
contrast(lsmeans(model4, ~Group), "trt.vs.ctrl", adjust = "none") 


#Another imputation method for missing data in ANCOVA is multiple imputation. This technique is a bit more complex
#and requires further considerations (e.g., are the data missing at random vs. missing completely at random vs. missing not at random, etc.).
#There are packages available in R for conducting multiple imputation analyses; however, this technique is pretty advanced for the
#scope of this course.


#______________________________________#
# Mixed Linear Models                  #
#______________________________________#

#While ANCOVAs provide an easy solution to examining the effects of an intervention, mixed linear models provide a more robust and powerful
#solution for looking at the effects of an experiment. However, we need to have at least 3 timepoints in order to run a mixed linear model
#(i.e., baseline, midpoint, and final).

#First we need to switch our data back into a modified long format. As you'll notice, this new long format only includes data which vary at timepoints
# 2 and 3 (i.e., midpoint and final). Baseline data is included separately as a fixed effect.

data3 <- reshape(as.data.frame(wide.data),idvar="ID",varying=11:14,direction="long",sep=".")

View(data3)


#Now that we have our data set up for a mixed linear model, let's run a random intercepts model using the lmer package.
#To start, let's check to see if there's any variance in the data across time. We can do this by buidling a null model.

nullmodel <- lmer(MetersWalked~(1|ID), data=data3)
summary(nullmodel)

#When looking at the amount of residual variance in comparison to the variance of the intercepts (IDs), we note that there is
# 23% of the residual variance can be accounted for by the intercepts - suggesting that we can run a random effects model.
summary(nullmodel)
3855/16775

#Let's now go ahead and run a random intercepts model looking at the effects of the intervention after accounting for baseline outcome,
#age, and sex. We need to ensure that time is a categorical variable first, as this will make the interpretation easier.

model5 <- lmer(MetersWalked~Group*factor(time) + MetersWalkedbaseline + Age + Sex + (1|ID), data=data3)
summary(model5)

#However, we're not done yet if we want to determine if there are between group differences. Next, we need to contrast the groups at our 
#primary endpoint (6 months) using the lsmeans and contrast procedures.
lsmeans(model5, ~Group|time)
contrast(lsmeans(model5, ~Group|time), "trt.vs.ctrl", adj="none")

#Here's what the 95% CIs look like
confint(contrast(lsmeans(model5, ~Group|time), "trt.vs.ctrl", adj="none"), parm, level = 0.95)


#__________________________________#
# Practice Questions               #
#__________________________________#

#Let's assume that we ran a RCT looking at the effects of an intervention on cognitive function (ADAS-Cog).
#There are three timepoints (Baseline, Final [6 months], and then a 12 month follow-up).

#1) Describe each group at baseline. Are there important baseline differences which we need to account for in our analysis?

#2) Using ANCOVA, what are the effects of the intervention on ADAS-Cog at Final? What covariates should I include and why?

#3) Are there missing data when running the ANCOVA? If so, what can we do to impute those missing data? Re-run the anlaysis including imputed data.

#4) What are the estimated marginal mean ADAS-Cog scores for each group at Final in your model (don't use imputed data)? What is the between group difference in ADAS-Cog scores?

#5) Using Mixed Linear Modeling, run a random intercept null model. How much variance is accounted for by the intercepts? Now include a random slope for time. 
#How much variance is now accounted for? Should we include a random slope as well as an intercept?

#6) Using Mixed Linear Modeling, what are the estimated marginal mean ADAS-Cog scores and between group differences at Final? Are these results the same or different than your ANCOVA model?
