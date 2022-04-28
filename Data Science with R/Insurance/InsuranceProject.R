#Sweeding Insurance Project

#Load the input file
raw.data = read.csv(choose.files())

data = raw.data #Take the backup of the data and proceed

##################################################################################################
#The committee is interested to know each field of the data collected through descriptive
#analysis to gain basic insights into the data set and to prepare for further analysis. 
##################################################################################################
data[1:3,]
summary(data)

str(data)
table(data$Kilometres)
table(data$Zone)
table(data$Bonus)
table(data$Make)

table(data$Insured)
plot(data$Insured)

boxplot(data$Insured)
boxplot(log(data$Insured))

table(data$Claims)
plot(data$Claims)

boxplot(data$Claims)
boxplot(log(data$Claims))

table(data$Payment)
plot(data$Payment)

boxplot(data$Payment)
boxplot(log(data$Payment))
##################################################################################################

##################################################################################################
#The total value of payment by an insurance company is an important factor to be monitored.
#So the committee has decided to find whether this payment is related to number of claims and
#the number of insured policy years. They also want to visualize the results for better understanding.
##################################################################################################

plot(data$Claims, data$Payment)
plot(data$Insured, data$Payment)
#Visually Claims seems to be best predictor of Payment

library(corrplot)
cor(data)
corrplot(cor(data))
#From the corrplot there seems to be Strong positive corellation with Claims and strong positive 
#correlation with Insured.
#However there is also strong between claims and Insured, so individual models also needs to be 
#verified for Adj. R-squared value, the model with the highes Adj. R-squared value needs to be
#selected

formula_string = c("Payment ~ Claims + Insured", "Payment ~ Claims", "Payment ~ Insured")

#Create a function so that the models can be called in loops
model = function (formula_string)
{
  for( i in formula_string)
  {
    print(i)
    log_model = lm(formula = i, data = data)
    print(summary(log_model))
    
    lm_tuned = step(object = log_model, direction = "both")
    print(summary(lm_tuned))
    
  }
  remove(log_model, lm_tuned)
}

model(formula_string)

remove(formula_string)

#From the summary both Claims and Insured seems to be best predictors of Payment and null hypothesis
#is rejected at 0.1%
#The equation is
#   Payment = 4294.7750 * Claims + 28.3881 * Insured + 3250.7447

##################################################################################################

##################################################################################################
#The committee wants to figure out the reasons for insurance payment increase and decrease.
#So they have decided to find whether distance, location, bonus, make, and insured amount or 
#claims are affecting the payment or all or some of these are affecting it. 
##################################################################################################

formula_string = c("Payment ~ .")
model(formula_string)

cor(data)

#Pass#1 - Kilometres, Insured and Claims seems to be best predictors of Payment
#       - zone is also a predictor of payment
#       - However corrplot shows kilometres and zone both have week negative correlation with 
#       - payment;remove these feature in 2nd pass and verify the model

formula_string = c("Payment ~ .", "Payment ~ Insured + Claims")
model(formula_string)

#After pass#2 Adj-Rsquared value got reduced by only 0.0001; so 2nd model with only Insured and 
#Claims seems to be best predictors of Payment and the null hypothesis is rejected at 0.1%
#Equation remains
#   Payment = 4294.7750 * Claims + 28.3881 * Insured + 3250.7447

remove(formula_string)

##################################################################################################

##################################################################################################
#The insurance company is planning to establish a new branch office, 
#so they are interested to find at what location, kilometer, and bonus level their insured amount, 
#claims, and payment get increased. (Hint: Aggregate Dataset) 
##################################################################################################

library(dplyr)

data %>% 
  group_by(Zone, Kilometres, Bonus) %>%
  summarise(max.Insured = max(Insured), max.claims = max(Claims), max.payment = max(Payment)) %>%
  arrange(desc(max.payment, max.claims, max.Insured))

#Insurance Amounts, Claims and payments get increased when the zone = 4, Bonus = 7 and Kilometers = 2
#Note that for kilometers = 1, Insurance amounts are the highest
##################################################################################################


##################################################################################################
#The committee wants to understand what affects their claim rates so as to decide the right premiums
#for a certain set of situations. Hence, they need to find whether the insured amount, zone,
#kilometer, bonus, or make affects the claim rates and to what extent.
##################################################################################################

formula_string = c("Claims ~ Insured + Zone + Kilometres + Bonus + Make",
                   "Claims ~ Insured + Zone + Make + Kilometres")
model(formula_string)

#Pass 1 : Insured, Zone, Bonus and Make seems to be good predictors of Claims; and Kilometres also
#       : has an impact on the Claims
#       : Bonus has week positive corelation with Claims; remove Bonus from the model and verify
#       : Adj-Rsquare value
# Adjusted R-squared:  0.8421 

cor(data)

#Pass 2 : Model 2 seems to be better predictor of Claims and null hypothesis can be rejected at 0.1%
#       : where Claims is impacted by Insured, Zone, Make and Kilometres
#       : Equation remains
#       : Claims = 0.316064 * Insured + 6.8717950 * Make - 6.3872292 * Zone 
#                      - 4.1291667 * Kilometres + 20.7305186

remove(formula_string)

##################################################################################################


