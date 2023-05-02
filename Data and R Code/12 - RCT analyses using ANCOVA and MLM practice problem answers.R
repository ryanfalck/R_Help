#######################################################################################
# Introduction to R (ANCOVA and MLM Practice Question Answers)
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

#__________________________________#
# Answers                          #
#__________________________________#

#Let's assume that we ran a RCT looking at the effects of an intervention on cognitive function (ADAS-Cog; higher scores on ADAS-Cog = worse cognition).
#There are three timepoints (Baseline, Final [6 months], and then a 12 month follow-up). 

#ANCOVA data set
varying<-data2[c(1,2,10:11:length(data2))]

baseline<-subset(data2[-c(10:11:length(data2))], Time==1)

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

Timevars <- grep("Time",colnames(wide.data),value=TRUE)
wide.data <- wide.data[,!(colnames(wide.data)%in%Timevars)]

colnames(wide.data) <- (gsub("_","",colnames(wide.data)))
colnames(wide.data) <- (gsub(".1","baseline",colnames(wide.data)))


#MLM data set
data3 <- reshape(as.data.frame(wide.data),idvar="ID",varying=11:14,direction="long",sep=".")

#1) Describe each group at baseline. Are there important baseline differences which we need to account for in our analysis?

Baseline<-subset(data2, Time == 1)
vars<-dput(names(Baseline[c(4:6,9,10,11)]))

Table1_continuous<-CreateTableOne(vars=vars, strata = "Group", data=Baseline)
print(Table1_continuous,contDigits=2,missing=TRUE,quote=TRUE)

#In an RCT, baseline differences should not be compared using hypothesis testing. Randomization means that any group differences are due to random chance.


#2) Using ANCOVA, what are the effects of the intervention on ADAS-Cog at Final? What covariates should I include and why?

#If we want to include covariates, we should do so based on what variables we think are likely to help improve our ability to determine if indeed there really is an effect of the intervention. 
#Put another way, does including a covariate help improve our ability to detect the signal of the effects of an intervention from the surrounding noise? For this RCT, we should probably include
#age, sex, and education as covariates - since these variables liklely are related to changes in cognition.

model1<-lm(ADASCOG.2~Group + ADASCOGbaseline + Age + Sex + Education, data=wide.data)
summary(model1)
lsmeans(model1, ~Group)
contrast(lsmeans(model1, ~Group), "trt.vs.ctrl", adjust = "none") 
confint(contrast(lsmeans(model1, ~Group), "trt.vs.ctrl", adjust = "none") , parm, level = 0.95)

#Unfortunately, it looks like the intervention group got WORSE at Final in cognitive function after accounting for age, sex, and education

#3) Are there missing data when running the ANCOVA? If so, what can we do to impute those missing data? Re-run the anlaysis including imputed data.

describeBy(wide.data$ADASCOG.2, wide.data$Group)

#There are 2 missing in CON and 1 in INT. Two easy solutions would be to use either last observation carried forward (LOCF), or
#baseline carried forward (BCF). In this case, we don't missing data at timepoint 2, so LOCF and BCF are essentially the same thing. Here's how we would do that:

wide.data2<-wide.data %>%
  mutate(ADASCOGBCF.2 = if_else(is.na(ADASCOG.2), ADASCOGbaseline, ADASCOG.2)
  )

describeBy(wide.data2$ADASCOGBCF.2, wide.data2$Group)


model2<-lm(ADASCOGBCF.2~Group + ADASCOGbaseline + Age + Sex + Education, data=wide.data2)
summary(model2)
lsmeans(model2, ~Group)
contrast(lsmeans(model2, ~Group), "trt.vs.ctrl", adjust = "none") 
confint(contrast(lsmeans(model2, ~Group), "trt.vs.ctrl", adjust = "none") , parm, level = 0.95)

#However, even after imputing data, it looks like the intervention group still got WORSE at Final in cognitive function after accounting for age, sex, and education


#4) What are the estimated marginal mean ADAS-Cog scores for each group at Final in your model (don't use imputed data)? What is the between group difference in ADAS-Cog scores?

lsmeans(model1, ~Group)
contrast(lsmeans(model1, ~Group), "trt.vs.ctrl", adjust = "none") 
confint(contrast(lsmeans(model1, ~Group), "trt.vs.ctrl", adjust = "none") , parm, level = 0.95)

#INT= 15.8; CON= 11.6; estimated mean difference: 4.18 [0.39, 7.98]

#5) Using Mixed Linear Modeling, run a random intercept null model. How much variance is accounted for by the intercepts? Now include a random slope for time. 
#How much variance is now accounted for? Should we include a random slope as well as an intercept?

nullmodel <- lmer(ADASCOG~(1|ID), data=data3)
summary(nullmodel)

2.336/54.871

#~4% of the variance in ADAS-Cog at timepoints 2 and 3 can be explained with random intercepts. This seems to suggest that a random intercepts model might be somewhat useful.

nullmodel2 <- lmer(ADASCOG~(time|ID), data=data3)
summary(nullmodel2)

#Unfortunately, it doesn't look like we have enough data or observations to be able to run a random slopes model. Thus, we'll stick with a random intercepts.


#6) Using Mixed Linear Modeling, what are the estimated marginal mean ADAS-Cog scores and between group differences at Final? Are these results the same or different than your ANCOVA model?

model3 <- lmer(ADASCOG~Group*factor(time) + ADASCOGbaseline + Age + Sex + Education + (1|ID), data=data3)
summary(model3)

lsmeans(model3, ~Group|time)
contrast(lsmeans(model3, ~Group|time), "trt.vs.ctrl", adj="none")

confint(contrast(lsmeans(model3, ~Group|time), "trt.vs.ctrl", adj="none"), parm, level = 0.95)

#Estimated marginal means at final: INT = 13.9; CON = 11.6; estimated mean difference = 2.28 [-2.94, 7.50]. These results suggest that there is not a difference between groups in ADAS-Cog at Final.
