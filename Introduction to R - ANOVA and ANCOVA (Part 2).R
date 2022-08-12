#######################################################################################
# Introduction to R (Day 4)
# Author: Ryan Falck
# February 26, 2020
#######################################################################################
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, plyr, lme4,car, broom, ggplot2, psych, Hmisc, tableone)
setwd("C:/Users/falckr/Desktop/UBC-Postdoctoral Fellowship/R Help")
data1<- read_excel("Test Data.xlsx")

data2<-rename(data1, c("Height (cm)"="Height", "Weight (kg)"="Weight", "BMI (kg/m2)" = "BMI" , "6MWT" = "Meters_Walked", "ADAS-Cog Total" = "ADAS_COG"))

#______________________________#
#Answers to the Problems#
#______________________________#
#1) What's the correlation between BMI and Meters Walked at Final (i.e., Time = 3)
FINAL<-subset(data2, Time ==3)
cor.test(FINAL$BMI, FINAL$Meters_Walked, na.action = complete.obs)


#2) Make a scatterplot of Age (X) and BMI (Y) at Baseline (Time = 1).
BASELINE<-subset(data2,Time == 1)
plot(BASELINE$BMI~BASELINE$Age)


#3) Include a regression line in your scatter plot (make it blue), a vertical line at mean age (in red), and a horizontal line at BMI=27 (in green).
abline(lm(BMI~Age,BASELINE), col = "blue")
abline(v = mean(BASELINE$Age), col = "red")
abline(h= 27, col = "green")

#4) Include a Lowess curve on the scatter plot (black dotted line and make it much larger than your other lines)

lines(lowess(BASELINE$Age, BASELINE$BMI), lty = 3, lwdd=6)


#5) OK now let's make a new scatter plot. This time of ADAS-Cog and Meters Walked at Midpoint (Time = 2). However, this time see if you can get separate dots 
#   for each group (i.e., INT and CON). You may need to google how to do this. Also see if you can make separate regression lines for each intervention group.
MIDPOINT<- subset(data2, Time == 2)

INT <- subset(MIDPOINT, Group == "INT" )
CON <- subset(MIDPOINT, Group == "CON")

MIDPOINT$tx<-NA
MIDPOINT$tx[MIDPOINT$Group == "INT"]<-1
MIDPOINT$tx[MIDPOINT$Group == "CON"]<-0

plot(MIDPOINT$ADAS_COG~MIDPOINT$Meters_Walked, pch=MIDPOINT$Group)
abline(lm(ADAS_COG~Meters_Walked,INT), col = "blue")
abline(lm(ADAS_COG~Meters_Walked,CON), col = "red")

plot(MIDPOINT$ADAS_COG~MIDPOINT$Meters_Walked, pch=MIDPOINT$tx)
abline(lm(ADAS_COG~Meters_Walked,INT), col = "blue")
abline(lm(ADAS_COG~Meters_Walked,CON), col = "red")
legend("topleft", c("Intervention","Control"),pch=MIDPOINT$tx)


#6) Use Performance Analytics to get the correlations and histograms of BMI, Age, and ADAS-Cog at Final.
library("PerformanceAnalytics")
Final.chart<-subset(FINAL, select = c(1,9,4,10))
chart.Correlation(Final.chart[,2:4], histogram=TRUE, pch=19)


#7) Is there a significant difference in ADAS-Cog between Males and Females at Final? What's the t-value and p-value?
t.test(ADAS_COG~Sex, FINAL) #No effect of sex on ADAS Cog at Final


#8) Is there a significant difference in ADAS-Cog between males and females when we control for Age at Final? What's the F-value and p-value?
anova.lm<-lm(ADAS_COG~Sex+Age,FINAL)
anova(anova.lm)
summary(anova.lm)


#9) Does the model looking at sex differences in ADAS-Cog (controlling for Age) meet the assumptions of ANOVA?
library(gvlma)
gvlma(anova.lm) #Yes
plot(anova.lm)


#10) Do males and females have different education levels at baseline? 
chisq.test(x = table(BASELINE$Education, BASELINE$Sex))


#____________________________#
#ANOVA and ANCOVA            #
#____________________________#

#Thus far, we have discussed how to do ANOVAs and ANCOVAs using data from a single timepoint. 
#However, we typically are looking at changes over time. Let's look at one way of examining group 
#differences in ADAS-Cog from the intervention from baseline to final (time = 3)

#First we need to manipulate our data so that it is in a wide format
data2 #Note: our data is initially in a long form, where each variable is listed by time
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

#Now our data is in a wide format where each time-varying variable is 
#listed by it's timepoint (e.g., ADAS_COG.1 = ADAS_COG at Time = 1)
wide.data

#One way of looking at group differences in an intervention could be just looking at differences at final timepoint
group.lm <- lm(ADAS_COG.3~Group, wide.data)
anova(group.lm) #no differences between groups at Final


#However, the best practice for RCTs (and most pre-post studies in general) is to also control for baseline
group.lm.2<- lm(ADAS_COG.3~Group+ADAS_COG.1,wide.data)
anova(group.lm.2) # Still no effect


#______________________________#
# Estimated Marginal Means     #
#______________________________#

#Estimated marginal means is the mean response for a given variable adjusted for other factors.
#There is a great amount of flexibility in R for how we can view those means using the lsmeans package

install.packages("lsmeans")
library(lsmeans)

#Let's start by looking simply at group differences at follow-up after controlling for baseline 
group.lm.2<- lm(ADAS_COG.3~Group+ADAS_COG.1,wide.data)
lsmeans(group.lm.2, ~Group) #Here we can see that the control group has "slightly" better performance at follow-up

#But we are probably also going to be interested in how much of difference there is between groups. 
#We can do this using contrast statements
lsmeans.1<-lsmeans(group.lm.2, ~Group)
contrast(lsmeans.1, "trt.vs.ctrl", adjust = "none") 

#We can also see the confidence intervals for the differences if we want
contrast.1<-contrast(lsmeans.1, "trt.vs.ctrl", adjust = "none") 
confint(contrast.1, parm, level = 0.95)


#_____________________________#
# Introduction to ggplot2     #
#_____________________________#

#Most importantly, lsmeans allows us to graph the results of our ANOVA.Ggplot2 
#is how we will visualize most of our results moving forward. It's a highly
#flexible program capable of visualizing most GLM analyses. How we graph our
#results will be highly dependent on what it is we are trying to visualize, so
#what we will cover is not a one-size-fits-all method for graphing your results and you will
#100% need to use Google to fit how you want to visualize the data.


#First we need to turn this into a data frame
lm.frame<-lsmeans(group.lm.2, ~Group)
lm.frame<-as.data.frame(lm.frame)

#Now let's use ggplot2 to graph this. 
ggplot(lm.frame, aes(x = Group, y = lsmean)) + geom_bar(stat="identity", position= "identity")
  
#However, this graph is not very informative. So let's add some labels
ggplot(lm.frame, aes(x = Group, y = lsmean)) + geom_bar(stat="identity", position= "identity") +
   labs(x="Intervention Group", y="Change in ADAS-Cog")

#Personally, I don't like the widths of the bars... They're too wide, so let's make them narrower
ggplot(lm.frame, aes(x = Group, y = lsmean)) + geom_bar(stat="identity", position= "identity", width = .5) +
  labs(x="Intervention Group", y="Change in ADAS-Cog")
 
#Let's also add some standard error bars
ggplot(lm.frame, aes(x = Group, y = lsmean)) + geom_bar(stat="identity", position= "identity", width = .5) +
  labs(x="Intervention Group", y="Change in ADAS-Cog") +geom_errorbar(aes(ymin=lsmean - SE, ymax=lsmean + SE), width=.2)

#Or we could make this into 95% confidence intervals...
ggplot(lm.frame, aes(x = Group, y = lsmean)) + geom_bar(stat="identity", position= "identity", width = .5) +
  labs(x="Intervention Group", y="Change in ADAS-Cog") +geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2)

#We can also make this more colourful
ggplot(lm.frame, aes(x = Group, y = lsmean, fill=Group)) + geom_bar(stat="identity", position= "identity", width = .5) +
  labs(x="Intervention Group", y="Change in ADAS-Cog") +geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2)

#Maybe a little less colour
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(x="Intervention Group", y="Change in ADAS-Cog") +geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2)

#Now let's give this graph a title
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2)

#If you want to move where the tile is, then it requires a bit of manual labour and eyeballing
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2)

#We can also change other aspects of the graph. There are several themes available to 
#the graph which we can run through briefly
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + theme_bw()

#We can also change other aspects of the graph. There are several themes available to 
#the graph. Here are a few options (there are many others)
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + theme_bw()

ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + theme_classic()

ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + theme_dark()

ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + theme_minimal()

#We can also alter individual aspects of the graph theme
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + 
  theme(plot.background = element_rect(fill = "green"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#Finally, let's alter the scale (i.e., let's make the y-axis a lot bigger)
ggplot(lm.frame, aes(x = Group, y = lsmean, color=Group)) + geom_bar(stat="identity", position= "identity", width = .5, fill= "white") +
  labs(title= "           My first ggplot2 graph" , x="Intervention Group", y="Change in ADAS-Cog") +
  geom_errorbar(aes(ymin=lsmean - SE*1.96, ymax=lsmean + SE*1.96), width=.2) + 
  theme(plot.background = element_rect(fill = "green"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  ylim(0,50)


#A great resource for the primary aspects of ggplot which you can alter can be found here:
#https://rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf



#_____________________________________________#
#          Practice Questions                 #
#_____________________________________________#


#1) Is there a group difference in Six Minute Walk at Midpoint (i.e., Time=2), when controlling for 
#   Age, Sex, and baseline Six Minute Walk?


#2) What are the estimated marginal means for each group at Midpoint for the above model?


#3) What is the estimated mean difference between groups for this model at Midpoint?


#4) What is the estimated mean difference between the sexes (without respect to group) for this model?


#5) Graph the group differences in Six Minute walk. See if you can make the groups 
#   differentiated by colour.








