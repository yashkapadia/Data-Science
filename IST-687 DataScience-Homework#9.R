#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#8- Finding Patterns with rules
#Date of assignment due Wednesday,November 8, 2018
#Date in which the assignment is submitted is November 8, 2018.



#installing the required packages
install.packages("RJSONIO")
library(RJSONIO)

#Part A: Explore Data Set
#1)	Load the dataset: hotelSurveyBarriot.json 
dataset.new <- "hotelSurveyBarriot.json" #storing the json file in a variable
hotelSurveyOut <- fromJSON(dataset.new, simplify = TRUE, nullValue = NA) #transfering the json data into R and replacing null values with NA
hotelSurvey <- data.frame(hotelSurveyOut) #converting into a data frame

#2)	Name the dataframe hotelSurvey
View(hotelSurvey)


#Part B: Explore Data Set
#3)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
str(hotelSurvey)
summary(hotelSurvey)


hotelSurvey <- hotelSurvey[,-11] #removing the unwanted column freetext


#4)	Map each numeric attribute to a category  – Since we want to create rules, we should convert the attributes that have a numeric range into buckets (ex. low or high)
#Creating a function to map each numeric attribute ranging between 0 to 10 to a particular category
createBucketSurvey <- function(vec){
  vBuckets <- replicate(length(vec), "Average") #Make every observation as "Average"
  vBuckets[vec > 7] <- "High" #anything above 7 is considered as "High"
  vBuckets[vec < 7] <- "Low" #anything below 7 is considered as "Low"
  return(vBuckets)
}
#creating a function to map other survey attributes
createBuckets <- function(vec){
  q <- quantile(vec, c(0.4, 0.6)) 
  vBuckets <- replicate(length(vec), "Average") #Make every observation as "Average"
  vBuckets[vec <= q[1]] <- "Low" # anything below 40th percentile is "Low"
  vBuckets[vec > q[2]] <- "High" #anything above 6oth percentile is "High"
return(vBuckets)
}
#creating buckets for each attribute and storing them into new variables, respectively.
happyCust <- createBucketSurvey(hotelSurvey$overallCustSat)
CheckIn <- createBucketSurvey(hotelSurvey$checkInSat)
Clean <- createBucketSurvey(hotelSurvey$hotelClean)
Friendly <- createBucketSurvey(hotelSurvey$hotelFriendly)


Length <- createBuckets(hotelSurvey$lengthOfStay)
Size <- createBuckets(hotelSurvey$hotelSize)
Age <- createBuckets(hotelSurvey$guestAge)
Booked <- createBuckets(hotelSurvey$whenBookedTrip)

#5)	Count the people in each category of for the age and friendliness attributes
t1 <- table(Age)
t1

t2 <- table(Friendly)
t2

#6)	Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command
prop.table(t1)
prop.table(t2)

#7)	Show a “contingency table” of percentages for the age and the overall satisfaction variables together.
contigency <- table(Age,happyCust)
contigency
prop.table(contigency)
#Write a block comment about what you see.
#It can be seen from the contigency table that Customers with highest satisfaction are the most aged Customers. 
#Many of such insights can be genrated by combining such attributes into a table

#Part C: Coerce the data frame into transactions
#8)	Install and library two packages: arules and arulesViz.

#installing the required packages for Apriori Algorithm
install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

#9)	Coerce the hotelSurvey data frame into a sparse transactions matrix 
ruleDF <- data.frame(happyCust,CheckIn,Clean,Friendly,Length,Size,Age,Booked) #creating a new dataframe consisting of the bucketed attributes


hotelSurveyX <- as(ruleDF,"transactions") #creation of a saprse matrix which has transactions

hotelSurveyX

# Looking at the sparse matrix using methods in the arules package
inspect(hotelSurveyX)
itemFrequency(hotelSurveyX)
itemFrequencyPlot(hotelSurveyX)

#Part D: Use arules to discover patterns
#11)	Run the apriori command to try and predict happy customers 
#12)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 

#1st iteration
ruleset <- apriori(hotelSurveyX,parameter = list(support=0.1,confidence=0.5),appearance = list(default="lhs",rhs=("happyCust=High")))
inspect(ruleset) #This generates 73 rules with RHS being our dependent attribute "happyCust". The goal is to pick out maximum lift.
#lhs                                                               rhs              support confidence lift     count
#[62] {CheckIn=High,Clean=High,Friendly=Average,Booked=High}         => {happyCust=High} 0.1083  0.9926673  2.089386 1083 

#In this case, Lift is 2.08 which is the highest. This rule states that If the hotels are highly clean, Average friendly,freqeuntly booked and checkInSat is high, then the customers are most happy.

#2nd iteration
#increasing the support to reduce the number of rules
ruleset2 <- apriori(hotelSurveyX,parameter = list(support=0.2,confidence=0.5),appearance = list(default="lhs",rhs=("happyCust=High")))
inspect(ruleset2)

#lhs                                          rhs              support confidence lift     count
#{CheckIn=High,Clean=High,Booked=High}      => {happyCust=High} 0.2186  0.9014433  1.897376 2186
#In this case, the number of rules have reduced to 25, but the maximum lift is now 1.89 which lower than the previous iteration.
#The aim is to maximize the lift and also trying to minimize the rules

#13)	 If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, 
#what would those two rules be?  Use a block comment to explain your answer.

#According to me, The two rules that drive the overall customer satisfaction are as follows:-
#lhs                                                                 rhs            support confidence   lift     count
#{CheckIn=High,Clean=High,Friendly=Average,Booked=High}         => {happyCust=High} 0.1083  0.9926673  2.089386 1083 
#{Clean=High,Friendly=Average,Booked=High}                      => {happyCust=High} 0.1141  0.9904514  2.084722 1141 

#These two rules have the highest lift values.

#In summary, if the hotel checkInSat is high, friendly quotient is average, the hotel is very well cleaned and its most frequently booked, then th
#customers are highly satisfied with the hotel.

#Hence these factors drive the satisfaction of the customers.



#Extra code just trying out things...
ruleset3 <- apriori(hotelSurveyX,parameter = list(support=0.15,confidence=0.5),appearance = list(default="lhs",rhs=("happyCust=High")))
inspect(ruleset3)
