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


#_______________________________________#
#       ANOVA Interaction Effects       #
#_______________________________________#

#What about if we want to determine whether there are sex differences in the effects of the intervention?
group.lm.3<- lm(ADAS_COG.3~Group*Sex+ADAS_COG.1,wide.data)
anova(group.lm.3) # There is no main effect of group or sex, and there is no interaction between Group and Sex

#lsmeans is also great when we want to model interaction effects. 
#For instance, let's go back to the Group*Sex interaction we looked at.
group.lm.3<- lm(ADAS_COG.3~Group*Sex+ADAS_COG.1,wide.data)
lsmeans(group.lm.3, ~Group)#Group means 
lsmeans(group.lm.3, ~Sex)#Sex means
lsmeans(group.lm.3, ~Group|Sex)#Group means by sex

#What about when we contrast groups by sex?
lsmeans.2<-lsmeans(group.lm.3, ~Group|Sex)
contrast(lsmeans.2, "pairwise", adjust = "none") 
#While not significant, it looks like Females in the INT group performed better
#than Females in the CON group at final assessment, but the opposite is true for 
#Males (i.e., CON performed better at final assessment)

####GRAPHING THE INTERACTION EFFECTS####

#OK, now let's graph the interaction of group and sex

#First, we need to create the dataframe
lm.frame.2<-lsmeans(group.lm.3, ~Group|Sex)
lm.frame.2<-as.data.frame(lm.frame.2)


#Let's also create a separate data frame of the contrasts we just ran (we'll come back to this later)
lm.frame.3<-contrast(lsmeans.2, "pairwise", adjust = "none") 
lm.frame.3<-as.data.frame(lm.frame.3)


#Let's graph the mean ADAS-Cog score for each group by sex
ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex")


#How about if I don't want colour?
ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex") + scale_fill_grey()


#Or I can change the colour palatte (these are just a few of the choices)
ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) + theme_minimal() +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex") + scale_fill_brewer()


#Or I can change the colour palette (these are just a few of the choices)
ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) + theme_minimal() +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex") + scale_fill_brewer(palette=4)

ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) + theme_minimal() +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex") + scale_fill_brewer(palette=10)

ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) + theme_minimal() +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex") + scale_fill_brewer(palette=15)

#Or maybe you want to manually choose your colour palette
ggplot(lm.frame.2, aes(x = Group, y = lsmean, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5) + theme_minimal() +
  labs(x="Intervention Group", y="Change in ADAS-Cog", fill="Sex") + 
  scale_fill_manual(values= c("violet", "seagreen3"))


#We can also graph the contrasts, rather than the means
ggplot(lm.frame.3, aes(x = factor(Sex), y = estimate, fill=factor(Sex))) + 
  geom_bar(stat="identity", position= "dodge", width = .5)


#Or I might be interested in graphing the contrasts as well as the group differences. To do
#this, I will first need to create a single data frame including both the estimates of means,
#and the contrast estimates. First, let's create a dataframe which has the requisite information
#we'll need to graph the results

lm.frame.2 #let's enter these estimates first, we'll als make sex more interpretable than 1,0
lm.frame.3 #Let's flip the estimate around so it's INT-CON

Estimate <- c(12.755,10.789,8.9499,11.283,-1.966,2.334) #Make sure this is correctly ordered throughout
Group <- c("CON", "INT","CON", "INT","INT - CON", "INT - CON")
Sex <- c("Female", "Female", "Male", "Male", "Female", "Male")
lm.frame.4<-as.data.frame(cbind(Estimate,Group,Sex)) #Create the new dataframe with all information together
lm.frame.4$Estimate<-as.numeric(lm.frame.4$Estimate) #Needs to be numeric


#Now let's put the graph together
ggplot(lm.frame.4, aes(x = Group, y = Estimate, fill = Sex)) +
  geom_bar(stat="identity", position = position_dodge()) 
#Now we can clearly see that there was an improvement for females, but not for males



#______________________________________________#
#         Practice Problems                    #
#______________________________________________#

#1) Determine if there is an interaction effect between Education and Sex on ADAS-Cog performance at Baseline (Time=1) controlling for age and BMI?


#2) What are the least-squared means for males and females by education? Contrast the differences between males and females by each education level (i.e., pairwise).


#3) Graph the interaction effect of education and sex. 


#4) Graph the differences (i.e., contrast) in ADAS-COG between males and females by each education level.


#5) Create a graph which shows both the least-squared means and the contrast differences of the interaction effect of sex x education.




