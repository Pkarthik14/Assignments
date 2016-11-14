library(dplyr)


getwd()
setwd("D:/DataScientist_Course/DSCT_INTRO_TO_R_ASSIGN-master")
SurveyData <- read.csv("35478-0001-Data.csv")

######## Are there any missing values for each column/row in the dataset? Please identify
########and highlight the attribute and the specific survey records which have missing values

# using subset function
Q1Data <-select(SurveyData,ID,MARITAL,AGE,GENDER1,HOMPOP,HAPPY,HEALTH,SATJOB,REALINC)
Q1Data_NA <- subset(Q1Data,MARITAL== 9 | AGE == 98 | AGE == 99 | GENDER1 == 0 | GENDER1 == 8 | GENDER1 == 9 |HOMPOP == 98 | HOMPOP == 99 | HAPPY == 0 | HAPPY == 8 | HAPPY == 9 | HEALTH == 0 | HEALTH == 8 | HEALTH == 9 | SATJOB == 0 | SATJOB == 8 | SATJOB == 9 | REALINC == 0 | REALINC == 999998 | REALINC == 999999,  
                  select=c(ID,MARITAL,AGE,GENDER1,HOMPOP,HAPPY,HEALTH,SATJOB,REALINC))

#### Missing values are represented by the symbol NA. Recode all missing values to NA in the data frame

Q2Data_NA <- Q1Data
Q2Data_NA$MARITAL[Q2Data_NA$MARITAL==9] <-NA
Q2Data_NA$AGE[Q2Data_NA$AGE==98 | Q2Data_NA$AGE==99] <-NA
Q2Data_NA$GENDER1[Q2Data_NA$GENDER1==0 | Q2Data_NA$GENDER1==8 | Q2Data_NA$GENDER1==9] <-NA
Q2Data_NA$HOMPOP[Q2Data_NA$HOMPOP==98 | Q2Data_NA$HOMPOP==99]<-NA
Q2Data_NA$HAPPY[Q2Data_NA$HAPPY==0 | Q2Data_NA$HAPPY==8 | Q2Data_NA$HAPPY==9] <-NA
Q2Data_NA$HEALTH[Q2Data_NA$HEALTH==0 | Q2Data_NA$HEALTH==8 | Q2Data_NA$HEALTH==9] <-NA
Q2Data_NA$SATJOB[Q2Data_NA$SATJOB==0 | Q2Data_NA$SATJOB==8 | Q2Data_NA$SATJOB==9] <-NA
Q2Data_NA$REALINC[Q2Data_NA$REALINC==0 | Q2Data_NA$REALINC==999998 | Q2Data_NA$REALINC==999999 ] <-NA

##### Factors are used to represent categorized variables. Encode the categorical variables as factors and label the various levels acording to the descriptions in the codebook

Q2Data_NA$MARITAL.f <- factor(Q2Data_NA$MARITAL, labels = c("Married","Widowed","Divorced","Separated","Never Married"))
Q2Data_NA$GENDER1.f <- factor(Q2Data_NA$GENDER1, labels = c("Male","Female"))
Q2Data_NA$HAPPY.f <- factor(Q2Data_NA$HAPPY, labels = c("Very happy","Pretty happy","Not too happy"))
Q2Data_NA$HEALTH.f <- factor(Q2Data_NA$HEALTH,labels = c("Excellent","Good","Fair","Poor"))
Q2Data_NA$SATJOB.f <- factor(Q2Data_NA$SATJOB,labels = c("Very Satisfied","Moderately Satisfied","A little dissatisfied","Very dissatisfied"))

#### Family's quality of life is likely to be more related to per per-capita household income than family income. Per-capita household income = totao household income / No. of family members
#### Taking the lowest value from each income category,create a new variable that represents per-capita household income called percapinc

percapinc <-  Q2Data_NA$REALINC/Q2Data_NA$HOMPOP

Q2Data_percap <- cbind(Q2Data_NA,percapinc)

Q2Data_percap$HAPPY
Q2Data_percap$REALINC
Q2Data_percap$percapinc

##### Provide a summary statistical result to that data (with the new vaiable created in Q4). Describe what the summary statistics tell us about the data

cor.test(Q2Data_percap$HAPPY,Q2Data_percap$REALINC)
cor.test(Q2Data_percap$HAPPY,Q2Data_percap$percapinc)

# Happiness value is expressed as 1,2,3 wih lowest numer represent the happiest (the most happy)
# correlation of happiness with percapinc is -0.16
# correlation of happiness with realinc is -0.10
# that indicates using percapinc yields better correlation with happiness as opposed to using realinc 
#MARITAL,AGE,GENDER1,HOMPOP,HAPPY,HEALTH,SATJOB,REALINC,percapinc
#ggplot(mydata_percap, aes(x = HAPPY)) 	+ geom_histogram(colour = "darkgreen", fill = "blue", binwidth = 1) 

summary_modal<-summary(Q2Data_percap)
summary_modal

### Are women happier than men? use Chi.sq() and a bar plot

barplot(table(Q2Data_percap$HAPPY.f), main="Happiness Distribution by Male & Female",xlab="Happiness Rating",ylab="Frequency")
barplot(table(Q2Data_percap$HAPPY.f[Q2Data_percap$GENDER1.f=="Male"]), main="Happiness Distribution by Male",xlab="Happiness Rating",ylab="Frequency")
barplot(table(Q2Data_percap$HAPPY.f[Q2Data_percap$GENDER1.f=="Female"]), main="Happiness Distribution by Female",xlab="Happiness Rating",ylab="Frequency")


MaleHappyTable <- table(Q2Data_percap$HAPPY.f[Q2Data_percap$GENDER1.f=="Male"])
CntMalePop   <- sum( MaleHappyTable)

MaleHappyTable[1]<-MaleHappyTable[1]*100/CntMalePop
MaleHappyTable[2]<-MaleHappyTable[2]*100/CntMalePop
MaleHappyTable[3]<-MaleHappyTable[3]*100/CntMalePop

FemaleHappyTable <- table(Q2Data_percap$HAPPY.f[Q2Data_percap$GENDER1.f=="Female"])
CntFemalePop   <- sum(FemaleHappyTable)

FemaleHappyTable[1]<-FemaleHappyTable[1]*100/CntFemalePop
FemaleHappyTable[2]<-FemaleHappyTable[2]*100/CntFemalePop
FemaleHappyTable[3]<-FemaleHappyTable[3]*100/CntFemalePop


aTableCnt <-cbind( MaleHappyTable, FemaleHappyTable)

barplot(aTableCnt, main="Happiness Distribution by Male & Female",xlab="Gender",ylab="Frequency",legend= rownames(count),
        beside= TRUE)


aTableCnt <-cbind( table(Q2Data_percap$HAPPY.f[Q2Data_percap$GENDER1.f=="Male"]), 
                   table(Q2Data_percap$HAPPY.f[Q2Data_percap$GENDER1.f=="Female"]))
barplot(aTableCnt, main="Happiness Distribution by Male & Female",xlab="Gender",ylab="Frequency",legend= rownames(count),
        beside= TRUE)
## from the bar plots obviously female didn't fair better than male in the categories
## of "very happy" and "not too happy"

chisq.test(aTableCnt)

# the test gives p-value=0.4765, greater than the 0.05 significance level
# chisq test independendence of two variables, which in this independendence of gender and happiness (null hypothesis)
# p-value=0.4765 means 47% chance that null hypothesis is correct
# that is greater than 5% significant level which lead to conclusion 
# we don't reject null hypothesis that happiness is independent of gender


### Are younger people happier? (Hint: convert age into 3 categories, young, middle and old age and perform the comparison. You may use prop.table() and a plot to help answer the question)

percapinc <-  Q2Data_percap$AGE

Q2Data_percap <- cbind(Q2Data_NA,percapinc)
class(Q2Data_percap)
Q2Data_percap[,"AgeCategory"] <- NA

Q2Data_percap$AgeCategory[Q2Data_percap$AGE < 35 ] <- "young"
Q2Data_percap$AgeCategory[Q2Data_percap$AGE >= 35 & Q2Data_percap$AGE <60 ] <- "middle age"
Q2Data_percap$AgeCategory[Q2Data_percap$AGE >=60 ] <- "old"

youngTbl  = table(Q2Data_percap$HAPPY.f[Q2Data_percap$AgeCategory=="young"])
middleTbl = table(Q2Data_percap$HAPPY.f[Q2Data_percap$AgeCategory=="middle age"])
oldTbl   =  table(Q2Data_percap$HAPPY.f[Q2Data_percap$AgeCategory=="old"])

aContingencyTbl = cbind(youngTbl,middleTbl,oldTbl)

aPropTblByCol = prop.table(aContingencyTbl,2)
#aPropTblByRow = prop.table(aContingencyTbl,1)

chisq.test(aContingencyTbl)
#chisq.test gives p-value = 0.00234, below 0.05
#we reject null hypothesis that ageCategory is independent of happiness

aPropTblByCol = prop.table(aContingencyTbl,2)
barplot(aPropTblByCol, main="Happiness Distribution by Age Group",xlab="Age Group",ylab="Ratio",legend= rownames(count),
        beside=  FALSE)
#aProbTblByCol and its barpot shows young group's "not too happy" 
#ratio was the lowest compares to middle and old age group

#### Are people with higher income happier income happier? (Hint: income is a continuous variable so you may wish to compare median incomes of the 3 subsets of data - very happy, pretty happy, Not Too happy)

VeryHappy_realinc <- median(Q2Data_percap$REALINC[Q2Data_percap$HAPPY.f =="Very happy"],na.rm = TRUE)
VeryHappy_realinc
PrettyHappy_realinc<- median( Q2Data_percap$REALINC[Q2Data_percap$HAPPY.f =="Pretty happy"],na.rm = TRUE)
PrettyHappy_realinc
NotHappy_realinc<- median(Q2Data_percap$REALINC[Q2Data_percap$HAPPY.f =="Not too happy"],na.rm = TRUE)
NotHappy_realinc

# Median income of "very happy" category is 26950, 
#  higher that "Pretty happy" (22050), 
# of which also higher than "Not Happy" (13475)


#### Are people in better health conditions richer?

# Health = {"Excellent","Good","Fair","Poor"}
ExcellentHealth_realinc <-(Q2Data_percap$REALINC[Q2Data_percap$HEALTH.f =="Excellent"])
ExcellentHealth_realinc
GoodHealth_realinc <-(Q2Data_percap$REALINC[Q2Data_percap$HEALTH.f =="Good"])
GoodHealth_realinc
FairHealth_realinc <-(Q2Data_percap$REALINC[Q2Data_percap$HEALTH.f =="Fair"])
FairHealth_realinc
PoorHealth_realinc <-(Q2Data_percap$REALINC[Q2Data_percap$HEALTH.f =="Poor"])
PoorHealth_realinc

boxplot(ExcellentHealth_realinc,GoodHealth_realinc,FairHealth_realinc,PoorHealth_realinc)
# the boxplot shows ppl with better health condition richer

#### Does happiness depend on marital status?

# Marital  = {"Married","Widowed","Divorced","Separated","Never Married"}
MarriedHappiness  = table(Q2Data_percap$HAPPY.f[Q2Data_percap$MARITAL.f =="Married"])
WidowedHappiness  = table(Q2Data_percap$HAPPY.f[Q2Data_percap$MARITAL.f =="Widowed"])
DivorcedHappiness = table(Q2Data_percap$HAPPY.f[Q2Data_percap$MARITAL.f =="Divorced"])
SeparatedHappiness = table(Q2Data_percap$HAPPY.f[Q2Data_percap$MARITAL.f =="Separated"])
NeverMarriedHappiness = table(Q2Data_percap$HAPPY.f[Q2Data_percap$MARITAL.f =="Never Married"])

aContingencyMarryTbl = cbind(MarriedHappiness,WidowedHappiness,DivorcedHappiness,SeparatedHappiness,NeverMarriedHappiness)

chisq.test(aContingencyMarryTbl)

# the test gives p-value=2.2e-16, much lower than the 0.05 significance level
# chisq test independendence of two variables, which in this case independendence of Marital and happiness (null hypothesis)
# p-value=2.2e-16 means almost zero chance that null hypothesis is correct
# we reject the null hypothesis that happiness is independent of marital status

# marital status has effect on happiness

### Are there any household income diff between races?

#The data set used doesn't race info for analysis







library(gdata)
help("35478-0001-Data.xlsx")


#######

setRepositories()

ap <- available.packages()
view(ap) "XLConnect" %in% rownames(ap)

######

mydata <- read.xls("C:\Users\karthikeyanp\Desktop\DataScientist_Course\DSCT_INTRO_TO_R_ASSIGN-master\35478-0001-Data.xls")

edit(file = system.file("XLConnect.R", package = "XLConnect"))

#####

vignette ( " XLConnect ")

setwd("D:\DataScientist Course\DSCT_INTRO_TO_R_ASSIGN-master")

help ( XLConnect )

###########

# Decimal / Real Values data types
x <- 10.5
x
class(x)
is.integer(x)



