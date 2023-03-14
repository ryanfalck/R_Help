#######################################################################################
# Introduction to R (Data Visualization Part 1 Practice Problem Answers)
# Author: Ryan Falck
#######################################################################################
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone, lsmeans)
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")
data1<- read_excel("Test Data.xlsx")

data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))


#_____________________________________________#
#          Answers to Practice Problems       #
#_____________________________________________#


#1) Is there a group difference in Six Minute Walk at Midpoint (i.e., Time=2), when controlling for 
#   Age, Sex, and baseline Six Minute Walk?

varying <- data2[c(1:2,10:11:length(data2))]
baseline <- subset(data2[-c(10:11:length(data2))],Time==1)

varying.1 <- subset(varying,Time==1)
varying.2 <- subset(varying,Time==2)
varying.3 <- subset(varying,Time==3)

colnames(varying.1) <- paste(colnames(varying.1),"1",sep=".")
colnames(varying.2) <- paste(colnames(varying.2),"2",sep=".")
colnames(varying.3) <- paste(colnames(varying.3),"3",sep=".")

library(dplyr)
wide.data <- left_join(baseline,varying.1,by=c("ID"="ID.1")) %>% 
  left_join(.,varying.2,by=c("ID"="ID.2")) %>% 
  left_join(.,varying.3,by=c("ID"="ID.3"))
detach("package:dplyr", unload = TRUE)

group.lm <- lm(Meters_Walked.2~Group+Age+Sex+Meters_Walked.1, wide.data)
anova(group.lm)
#No group differences

#2) What are the estimated marginal means for each group at Midpoint for the above model?
lsmeans(group.lm, ~Group)



#3) What is the estimated mean difference between groups for this model at Midpoint?
lsmeans.1<-lsmeans(group.lm, ~Group)
contrast(lsmeans.1, "trt.vs.ctrl", adjust = "none") 


#4) What is the estimated mean difference between the sexes (without respect to group) for this model?
lsmeans.2<-lsmeans(group.lm, ~Sex)
contrast(lsmeans.2, "trt.vs.ctrl", adjust = "none") 


#5) Graph the group differences in Six Minute walk. See if you can make the groups 
#   differentiated by colour.
lm.frame<-lsmeans(group.lm, ~Group)
lm.frame<-as.data.frame(lm.frame)

ggplot(lm.frame, aes(x = Group, y = lsmean, fill=Group)) + geom_bar(stat="identity", position= "identity")


