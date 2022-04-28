#Internet Project

library(readxl)
raw.data = read_xlsx(choose.files())

data = raw.data

#######################################################################################################
#The team wants to analyze each variable of the data collected through data summarization to get a 
#basic understanding of the dataset and to prepare for further analysis.
#######################################################################################################

data[1:3,]

#Remove BouncesNew columns which looks to be added in error

data$BouncesNew = NULL

str(data)

summary(data)

table(data$Bounces)
table(data$Exits)
table(data$Continent)
table(data$Sourcegroup)

plot(data$Timeinpage)
boxplot(data$Timeinpage)

table(data$Uniquepageviews)
table(data$Visits)

colSums(is.na(data))

#######################################################################################################

#######################################################################################################
#As mentioned earlier, a unique page view represents the number of sessions during which that page was
#viewed one or more times. A visit counts all instances, no matter how many times the same visitor may
#have been to your site. So the team needs to know whether the unique page view value depends on visits. 
#######################################################################################################

plot(data$Visits, data$Uniquepageviews)

#Visually Uniquepageviews and Visits seems to be related 

cor(data$Visits, data$Uniquepageviews)

#Also there exists high positive co-relation between Uniquepageviews and Visits

log_model = glm(formula = Uniquepageviews ~ Visits, data = data, family = "poisson")
summary(log_model)

remove(log_model)

#Visits is very good predictor of Uniquepageviews and we reject the null hypothesis at 0.1%
#The equation is
#    Uniquepageviews = 0.114183 * Visits - 0.004604

#######################################################################################################

#######################################################################################################
#Find out the probable factors from the dataset, which could affect the exits. Exit Page Analysis is 
#usually required to get an idea about why a user leaves the website for a session and moves on to
#another one. Please keep in mind that exits should not be confused with bounces.
#######################################################################################################
library(corrplot)

corrplot(cor(data[,-c(3,4)]))
#There seems to be high co-relation between Exits and Bounces, Uniquepageviews and Visits

model.matrix(object = Exits ~ ., data = data) -> matrix
matrix = matrix[,-1] #remove intercept column as this will not be required
matrix = as.data.frame(matrix)
data = cbind(data$Exits, matrix) #add the Exits column to the matrix

names(data)[1] = "Exits"

corrplot(cor(data))
cor(data$Exits,data)
#From the corr plot it looks like Exits has below co-relations with other features
#High Positive - Bounces, Uniquepageviews, Visits
#Low Positive - Few Continents, Few Sources, Timeinpage
#Low Negative - Few Continents, Few Sources


log_model = glm(formula = Exits ~ ., data = data, family = "poisson")
summary(log_model)

lm_tuned = step(object = log_model, direction = "both")
summary(lm_tuned)

#Pass 1: Few of the good predictors of Exits are Bounces, SourcegroupOthers,
#        Sourcegrouppublic.tableausoftware.com, Sourcegroupvisualisingdata.com, Timeinpage, Visits
#        Uniquepageviews and Sourcegroupgoogle are not intuitive with the corr so remove in pass 2

#AIC: 65155
log_model_p2 = glm(formula = Exits ~ Bounces + SourcegroupOthers + 
                  Sourcegrouppublic.tableausoftware.com + Sourcegroupvisualisingdata.com +
                  Timeinpage + Visits, data = data, family = "poisson")
summary(log_model_p2)


lm_tuned = step(object = log_model_p2, direction = "both")
summary(lm_tuned)
#AIC: 65605

#Check if there exists any multicolinearity within features

library(car)
vif(lm_tuned)

#Bounces and Visits are co-related features; try models by removing one of the feature

#Removing Visits from the model
log_model_p3 = glm(formula = Exits ~ Bounces + SourcegroupOthers + 
                     Sourcegrouppublic.tableausoftware.com + Sourcegroupvisualisingdata.com +
                     Timeinpage, data = data, family = "poisson")
summary(log_model_p3)

lm_tuned = step(object = log_model_p3, direction = "both")
summary(lm_tuned)

#AIC: 66070
#try new model by removing Bounces

log_model_p4 = glm(formula = Exits ~ SourcegroupOthers + 
                     Sourcegrouppublic.tableausoftware.com + Sourcegroupvisualisingdata.com +
                     Timeinpage + Visits, data = data, family = "poisson")


summary(log_model_p4)

lm_tuned = step(object = log_model_p4, direction = "both")
summary(lm_tuned)
#AIC: 67060             

# log_model_p2 seems to be better model with AIC: 65605
# Equation remains
#    Exits = 4.440e-01 * Bounces - 7.691e-02 * SourcegroupOthers
#             -  2.355e-01 * Sourcegrouppublic.tableausoftware.com
#             - 1.713e-01 * Sourcegroupvisualisingdata.com + 2.576e-05 * Timeinpage
#             - 2.208e-01 * Visits

remove(lm_tuned, log_model, log_model_p2, log_model_p3, log_model_p4, matrix)

################################################################################################

################################################################################################
#Every site wants to increase the time on page for a visitor. This increases the chances of the
#visitor understanding the site content better and hence there are more chances of a transaction
#taking place. Find the variables which possibly have an effect on the time on page
################################################################################################
cor(data$Timeinpage,data)

#From the corr plot it looks like Timeinpage has below co-relations with other features
#Low Positive - Exits, Few Continents, Few sourcegroup, Uniquepageviews, Visits
#Low Negative - Bounces, Few Continents, Few sourcegroup

log_model = glm(formula = Timeinpage ~ ., data = data, family = "poisson")
summary(log_model)

lm_tuned = step(object = log_model, direction = "both")
summary(lm_tuned)

#AIC: 7601395

#For pass2 select only those features which are intuitive with cor

log_model_p2 = glm(formula = Timeinpage ~ Exits + Bounces + ContinentN.America + ContinentSA +
                     Sourcegroupfacebook + Sourcegrouppublic.tableausoftware.com + 
                     Sourcegroupreddit.com + Sourcegrouptableausoftware.com + 
                     Sourcegroupvisualisingdata.com + Visits, data = data, family = "poisson")
summary(log_model_p2)

lm_tuned = step(object = log_model_p2, direction = "both")
summary(lm_tuned)

#AIC: 7634747

#Check for multicolinearity
vif(lm_tuned)

remove(lm_tuned, log_model, log_model_p2)
#AIC: 8893941

#log_model_p2 seems to be good model and below is the equation for the same
#Timeinpage = 0.4382445 * Exits - 2.3712710 * Bounces + 0.1096034 * ContinentN.America
# + 0.8166005 * ContinentSA - 0.0845087 * Sourcegroupfacebook
# + 0.2032100 * Sourcegrouppublic.tableausoftware.com - 0.0935245 * Sourcegroupreddit.com                 
# + 0.1039734 * Sourcegrouptableausoftware.com + 0.0070385 * Sourcegroupvisualisingdata.com
# + 0.9376057 * Visits + 4.0734356
################################################################################################

################################################################################################
#A high bounce rate is a cause of alarm for websites which depend on visitor engagement. Help
#the team in determining the factors that are impacting the bounce 
################################################################################################
cor(data$Bounces,data)

log_model = glm(formula = Bounces ~ ., data = data, family = "poisson")
summary(log_model)

lm_tuned = step(object = log_model, direction = "both")
summary(lm_tuned)

# AIC: 52684

#For pass2 select only those features which are intuitive with cor

log_model_p2 = glm(formula = Bounces ~ Exits + SourcegroupOthers + Sourcegrouppublic.tableausoftware.com
                   + Sourcegrouptableausoftware.com + Sourcegroupvisualisingdata.com + Timeinpage + Visits,
                   data = data, family = "poisson")
summary(log_model_p2)

lm_tuned = step(object = log_model_p2, direction = "both")
summary(lm_tuned)

# AIC: 60058

#Check for multicolinearity
vif(lm_tuned)

#There exists multicolinearity between Exits and Visits; try model removing those features

#Try a model by removing Exits
log_model_p3 = glm(formula = Bounces ~ SourcegroupOthers + Sourcegrouppublic.tableausoftware.com
                   + Sourcegrouptableausoftware.com + Sourcegroupvisualisingdata.com + Timeinpage + Visits,
                   data = data, family = "poisson")
summary(log_model_p3)

lm_tuned = step(object = log_model_p3, direction = "both")
summary(lm_tuned)

#AIC: 60427
#Try model by removing Visits
log_model_p4 = glm(formula = Bounces ~ Exits + SourcegroupOthers + Sourcegrouppublic.tableausoftware.com
                   + Sourcegrouptableausoftware.com + Sourcegroupvisualisingdata.com + Timeinpage,
                   data = data, family = "poisson")
summary(log_model_p4)

lm_tuned = step(object = log_model_p4, direction = "both")
summary(lm_tuned)

#AIC: 60060

remove(lm_tuned, log_model, log_model_p2, log_model_p3, log_model_p4)

#Model_2 looks to be good model and the equation is
#      Bounces = 2.492e-01 * Exits - 1.498e-01 * SourcegroupOthers -
#                6.829e-01 * Sourcegrouppublic.tableausoftware.com -
#                3.386e-01 * Sourcegrouptableausoftware.com -2.985e-01 * Sourcegroupvisualisingdata.com -
#                2.278e-03 * Timeinpage - 2.362e-02 * Visits - 4.178e-01
################################################################################################

  


