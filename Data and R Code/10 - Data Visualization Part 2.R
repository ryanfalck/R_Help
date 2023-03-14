#######################################################################################
# Introduction to R (Data Visualization Part 2)
# Author: Ryan Falck
#######################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone, lsmeans)
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")
data1<- read_excel("Test Data.xlsx")

data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))

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

