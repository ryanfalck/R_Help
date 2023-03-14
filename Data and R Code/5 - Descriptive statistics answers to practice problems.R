#######################################################################################
# Introduction to R (Descriptive Statistics practice problem answers)
# Author: Ryan Falck
#######################################################################################

#______________________________#
#Answers to the Problems#
#______________________________#

#1) Calculate the Mean, Median, SD,Skew, and Kurtosis for BMI in each group? What is the proportion for the full sample?
Baseline<-subset(data2, Time == 1)

describeBy(Baseline$BMI, Baseline$Group)
describe(Baseline$BMI)

#2) What is the proportion of people with a High School Education (i.e., Education == 1) or less in the Intervention Group? 

Baseline$Education[Baseline$Education==1]<- "High School or Less" #we have to make these into categorical variables first
Baseline$Education[Baseline$Education==2]<- "Trade School"
Baseline$Education[Baseline$Education==3]<- "University"

table(Baseline$Education, Baseline$Group)
8/20

vars<-dput(names(Baseline[c(4,9,10,11)]))
catVars<-dput(names(Baseline[c(5,6)]))

Table1_continuous<-CreateTableOne(vars=vars, strata = "Group", data=Baseline)
print(Table1_continuous,contDigits=2,missing=TRUE,quote=TRUE)

Table1_category<-CreateTableOne(vars=catVars, strata = "Group", data=Baseline)
print(Table1_category,exact=catVars,contDigits=2,missing=TRUE,quote=TRUE) #These are reported as means so we have to remake these variables a character variables first


#3) What is the proportion of people with a University Degree (i.e., Education == 3) in the Full Sample?
table(Baseline$Education)
12/40

Table1_category<-CreateTableOne(vars=catVars, data=Baseline)
print(Table1_category,exact=catVars,contDigits=2,missing=TRUE,quote=TRUE) #These are reported as means so we have to remake these variables a character variables first

#4) How many women (Sex == 0) have a university degree in the full sample? How many women have a university degree in the Control Group?

table(Baseline$Sex, Baseline$Education)
6/40

table(Baseline$Sex, Baseline$Education, Baseline$Group =="CON")
4/20

#5) What is the average ADAS-Cog score for women in the full sample? What about for men?
describeBy(Baseline$ADAS_COG, Baseline$Sex)

#6) What is the average ADAS-Cog score for men in the intervention group? For women in the control group?
Control<-subset(Baseline, Group == "CON")
Intervention<-subset(Baseline, Group == "INT")

describeBy(Intervention$ADAS_COG, Intervention$Sex)
describeBy(Control$ADAS_COG, Control$Sex)


#7) Use the table one package to create a table with the following variables (in order): 6MWT, Age, ADAS-CoG, and Weight.

vars<-dput(names(Baseline[c(4,9,10,11)]))
catVars<-dput(names(Baseline[c(5,6)]))

Table1_continuous<-CreateTableOne(vars=vars, strata = "Group", data=Baseline)
print(Table1_continuous,contDigits=2,missing=TRUE,quote=TRUE)
