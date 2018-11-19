#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#8- Linear Modelling Homework -Making Predictions
#Date of assignment due Wednesday,October 24, 2018
#Date in which the assignment is submitted is October 24, 2018.


#installing required packages
install.packages("RJSONIO")
library(RJSONIO)

#Step A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveySherison), as a JSON file.
dataset.name <- "hotelSurveySherison.json" #storing the json file in a variable
hotelSurveyOut <- fromJSON(dataset.name, simplify = TRUE, nullValue = NA) #transfering the json data into R and replacing null values with NA
hotelSurvey <- data.frame(hotelSurveyOut) #converting into a data frame

#2.	Use the str command to make sure you can see the attributes
str(hotelSurvey)

#installing ggplot2 to create bivariate plots
install.packages("ggplot2")
library(ggplot2)

#Step B: Explore the data  
#3.	Create bivariate plots for each of the attributes.
#Your code should produce nine separate plots. Make sure the Y-axis and X-axis are labeled.

#Keeping in mind that the overall customer satisfaction is the outcome (or dependent) variable, which axis should it go on in your plots?
#OverallCustSat being a dependent variable it should go on the Y-Axis and all the other independent variables should go on the X-Axis

plot1 <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelSize,y=hotelSurvey$overallCustSat)) + geom_point() #defining the aesthetics and selecting scatter point method
plot1  #choosing X-Axis for the independent variable and Y-Axis for the dependent variable
hotelSurvey$overallCustSatJ <- jitter(hotelSurvey$overallCustSat) #adding jitter to the variable
hotelSurvey$hotelSizeJ <- jitter(hotelSurvey$hotelSize)
plot1.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelSizeJ,y=hotelSurvey$overallCustSatJ)) + geom_point() #plot with jitter
plot1.new

plot2 <- ggplot(hotelSurvey,aes(x=hotelSurvey$checkInSat,y=hotelSurvey$overallCustSat)) + geom_point()
plot2
hotelSurvey$checkInSatJ <- jitter(hotelSurvey$checkInSat)
plot2.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$checkInSatJ,y=hotelSurvey$overallCustSatJ)) + geom_point()
plot2.new

plot3 <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelClean,y=hotelSurvey$overallCustSat)) + geom_point()
plot3
hotelSurvey$hotelCleanJ <- jitter(hotelSurvey$hotelClean)
plot3.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelCleanJ,y=hotelSurvey$overallCustSatJ)) + geom_point()
plot3.new

plot4 <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelFriendly,y=hotelSurvey$overallCustSat)) + geom_point()
plot4
hotelSurvey$hotelFriendlyJ <- jitter(hotelSurvey$hotelFriendly)
plot4.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelFriendlyJ,y=hotelSurvey$overallCustSatJ)) + geom_point()
plot4.new

plot5 <- ggplot(hotelSurvey,aes(x=hotelSurvey$guestAge,y=hotelSurvey$overallCustSat)) + geom_point()
plot5
hotelSurvey$guestAgeJ <- jitter(hotelSurvey$guestAge)
plot5.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$guestAgeJ,y=hotelSurvey$overallCustSatJ)) + geom_point()
plot5.new
  
plot6 <- ggplot(hotelSurvey,aes(x=hotelSurvey$lengthOfStay,y=hotelSurvey$overallCustSat)) + geom_point()
plot6
hotelSurvey$lengthOfStayJ <- jitter(hotelSurvey$lengthOfStay)
plot6.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$lengthOfStayJ,y=hotelSurvey$overallCustSatJ)) + geom_point()
plot6.new

plot7 <- ggplot(hotelSurvey,aes(x=hotelSurvey$whenBookedTrip,y=hotelSurvey$overallCustSat)) + geom_point()
plot7
hotelSurvey$whenBookedTripJ <- jitter(hotelSurvey$whenBookedTrip)
plot7.new <- ggplot(hotelSurvey,aes(x=hotelSurvey$whenBookedTripJ,y=hotelSurvey$overallCustSatJ)) + geom_point()
plot7.new

plot8 <- ggplot(hotelSurvey,aes(x=hotelSurvey$hotelState,y=hotelSurvey$overallCustSat)) + geom_point()
plot8 <- plot8+ theme(axis.text.x=element_text(angle = 90, hjust = 1))
plot8

plot9 <- ggplot(hotelSurvey,aes(x=hotelSurvey$gender,y=hotelSurvey$overallCustSat)) + geom_point()
plot9

#4.	What do you observe from the plots? Note via a block comment.
#Here, we have taken OverallCustSat as a dependent variable and all the other variables are used once as the predictors
#or the independent variables. By plotting these bivariate plots, we can observe the relationships between the two plotted 
#variables which can help us to determine the changes. It shows that when the independent variable changes how much change can be
#observed in the independent variable.



str(hotelSurvey)
hotelSurvey <- hotelSurvey[,-11:-19] #removing the extra columns
str(hotelSurvey)

#Step C: Generate a linear model  
#5.	Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response).
#Make sure to include all predictors in one model – NOT different models each with one predictor.
m1 <- lm(formula = overallCustSat ~ ., data = hotelSurvey) #Model m1 included all the variables as predictors except for the OverallCustSat which is the dependent variable
summary(m1)

#6.	Report the R-Squared in a comment. Which of the predictors are statistically significant in the model? 
#In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant.

#Multiple R-squared:  0.6702,	Adjusted R-squared:  0.6682 
#The statistically significant predictors are checkInSat, hotelClean, hotelFriendly, guestAge,lengthOfStay, whenBookedTrip
                            #     Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)                     8.321e+00  1.024e-01  81.276  < 2e-16 ***
# checkInSat                     -2.381e-01  5.544e-03 -42.940  < 2e-16 ***
# hotelClean                      4.042e-02  6.941e-03   5.824 5.93e-09 ***
# hotelFriendly                   1.122e+00  8.863e-03 126.557  < 2e-16 ***
# guestAge                       -1.205e-01  1.815e-03 -66.400  < 2e-16 ***
# lengthOfStay                   -3.284e-01  1.677e-02 -19.575  < 2e-16 ***
# whenBookedTrip                  6.421e-03  1.005e-03   6.387 1.77e-10 ***


#7.	Write a block comment that explains in a narrative your overall interpretation of the model. 
#Make sure to refer to each variable (one dependent and three independent) by a descriptive name (i.e., not X1, X2, etc.).

#In this particular model, on the Y-Axis we have the dependent variable - OverallCustSat and on the X-Axis we have all the independent predictors
#which will help in predicting the overall customer satisfaction. The R-square value is 0.67 which can be considered as good. An R-square value of 1 indicates
# that the independent variable perfectly predicts the dependent variable. Such linear models can be used to predict the changes when one or more independent
# variables increase or decrease

#Step D: Generate a different linear model  
#8.	Next, create a different regression model predicting the overall customer satisfaction from the one variable you think is best. 
#Then create another using two variables.
m2 <- lm(formula = overallCustSat ~ hotelFriendly, data = hotelSurvey) #Model m2 with one independent variable hotelFriendly which predicts OverallCustomer Satisfaction
summary(m2)

m3 <- lm(formula = overallCustSat ~ hotelFriendly+ guestAge, data = hotelSurvey)#Model m3 with two independent variables 
summary(m3)

#9.	Write a block comment comparing the two lm models in #8.

#I think the best independent variable used for prediction of the overall customer satisfaction is the hotelFriendly variable.
# After trying all the combinations of significant predictors, using hotelFriendly variable as the independent variable gives us the best R-squared value and the
#lowest residual error. 

#The second model consists of two independent variables hotelFriendly and guestAge which are used to predict OverallCustSat.Here,
# the combination of two variables gives us a increase in the r-square value. This indicates that the combinations of these two predictors 
# can predict the changes more accurately in comparison with only the single predictor- hotelfriendly



#Extra code for reference 
# Note- i used this code to find out the best predictor for the predicting the Overall customer Satisfaction by comparing
#all the r-square values and Residual error for each of the following models
m4 <- lm(formula = overallCustSat ~ hotelFriendly, data = hotelSurvey)
summary(m4)

m5 <- lm(formula = overallCustSat ~ hotelClean, data = hotelSurvey)
summary(m5)

m6 <- lm(formula = overallCustSat ~ checkInSat, data = hotelSurvey)
summary(m6)


m7 <- lm(formula = overallCustSat ~ guestAge, data = hotelSurvey)
summary(m7)

m8 <- lm(formula = overallCustSat ~ lengthOfStay, data = hotelSurvey)
summary(m8)

m9 <- lm(formula = overallCustSat ~ whenBookedTrip, data = hotelSurvey)
summary(m9)
