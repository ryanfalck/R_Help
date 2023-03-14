#######################################################################################
# Introduction to R (Correlations, Linear Regressions, and ANOVA practice problem answers)
# Author: Ryan Falck
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
