HospitalCosts = read.csv(file= choose.files())

library(tidyverse)

hosp = as_tibble(HospitalCosts)

#Visually inspecting the data
hosp

summary(hosp)
#glimpse(hosp)

#There is one NA value available in RACE column, which needs to be imputed 
#with the mean value

hosp$RACE[is.na(hosp$RACE)] = round(mean(hosp$RACE, na.rm = T), digits = 0)

#Plot the corr plot to visualize inter-relations if any
library(MASS)
library(corrplot)
corrplot(cor(hosp))

#Below are some of the findings for TOTCHG
#1 Total Discharge has below correlations
  #1 high +ve co-relation with Length Of Stay
  #2 low -ve co-relation with Diagnosis Groups
  #3 low +ve co-relation with Age
#There are also co-relations exists with
#High +ve co-relation between Age and Gender
#Low -ve co-relation between Age and Length of Stay
#High +ve co-relation between Age and Diagnosis Groups
#There is +ve co-relation between Gender and Diagnosis Groups

#Visualize the data and convert non-continuous data to factor variables
table(hosp$AGE)
table(hosp$FEMALE)
table(hosp$LOS)
table(hosp$RACE)
table(hosp$TOTCHG)
summary(hosp$TOTCHG)
table(hosp$APRDRG)


#Age is ordinal categorical variable and needs to be converted to factor
#Gender is a bivariate categorical variable and to be converted to factor
#Length of stay ordinal categorical variable and to be converted to factor
#Race is a categorical variable
#Discharge cost is continuous variable
#Diagnosis Group is categorical variable




#1 To record the patient statistics, 
#the agency wants to find the age category of people who frequent the hospital and
#has the maximum expenditure. 

tapply(X = hosp$TOTCHG, INDEX = list(hosp$AGE), FUN = sum)
names(which.max(tapply(X = hosp$TOTCHG, INDEX = list(hosp$AGE), FUN = sum)))

#-------------------------------------------------------------------------------------
#Conclusion
#Infants spend the max amount of 678118 for the hospitalization
#-------------------------------------------------------------------------------------

#2 In order of severity of the diagnosis and treatments and to find out the expensive treatments,
#the agency wants to find the diagnosis related group that has maximum hospitalization and 
#expenditure.
 
tapply(X = hosp$TOTCHG, INDEX=list(hosp$APRDRG), FUN = sum)

names(which.max(tapply(X = hosp$TOTCHG, INDEX=list(hosp$APRDRG), FUN = sum)))

#-------------------------------------------------------------------------------------
#Conclusion
#Diagnosis group#640 spend the max amount of 437978 on the hospitalization

#-------------------------------------------------------------------------------------


#3 To make sure that there is no malpractice,
#the agency needs to analyze if the race of the patient is related to the hospitalization costs.

aov(TOTCHG~RACE, data = hosp) -> aov_model
summary(aov_model)

#-------------------------------------------------------------------------------------
#Conclusion

#pvalue comes out to be very high 68.7% this means we can reject the null hypothesis.
#this means there is no relation between the race of patient and the hospital cost
#-------------------------------------------------------------------------------------

#4 To properly utilize the costs, the agency has to analyze the severity of the hospital
#costs by age and gender for proper allocation of resources.

#Note : Data has not been splitted into Test and Train due to small sample size
lm_age_gender = lm(formula = TOTCHG~AGE + FEMALE ,data = hosp)
summary(lm_age_gender)

#-------------------------------------------------------------------------------------
#Conclusion
#Null hypothesis is rejected as the pvalue is 0.001 
#Age is significant factors for calculating Total Discharge cost and null hypothesis
#is rejected at 0.1% significance level
#Gender is also a significant factor for calculating Total Discharge cost and 
#null hypothesis is rejected at 5% significance level

#linear regression equation will be as given below

#Total Charge = 86.28 * Age - 748.19 * Female + 2718.63
#-------------------------------------------------------------------------------------


#5 Since the length of stay is the crucial factor for inpatients, the agency wants to find if
#the length of stay can be predicted from age, gender, and race.

#Note : Data has not been splitted into Test and Train due to small sample size
lm_age_gender_race = lm(formula = LOS~AGE + FEMALE + RACE ,data = hosp)
summary(lm_age_gender_race)

#Model shows the p_value is 0.2763 which is very high and relation with the Age at 10% 
#significance, but as there is no direct corelation model needsd to be verified by
#removing Gender and Race parameters

#Note : Data has not been splitted into Test and Train due to small sample size
lm_age = lm(formula = LOS~AGE, data = hosp)
summary(lm_age)

#-------------------------------------------------------------------------------------
#Conclusion
#Null hypothesis is retained as the pvalue is 0.2763 
#Age, Gender are Race are not significant factors for calculating Total Discharge charges
#-------------------------------------------------------------------------------------

#6 To perform a complete analysis, the agency wants to find the variable that mainly affects
#the hospital costs

#Note : Data has not been splitted into Test and Train due to small sample size
lm_tot_chg = lm(formula = TOTCHG~., data = hosp)
summary(lm_tot_chg)


lm_age_los_aprdrg = lm(formula = TOTCHG ~ AGE + LOS + APRDRG, data = hosp)
summary(lm_age_los_aprdrg)

#-------------------------------------------------------------------------------------
#Conclusion
#Null hypothesis is rejected as the pvalue is very small - 2.2 * e^-16 
#Age, Length of stay and diagnosis group are significant factors for calculating
#Total Discharge cost at the significance level of 0.1%

#The equation is as given below

#Total cost = 128.59 * Age + 740.83 * Length of Stay - 8.01 * Diag Grp fact + 4959.86
#-------------------------------------------------------------------------------------
