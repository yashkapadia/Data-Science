#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#5- Accident Analysis using JSON
#Date of assignment due Wednesday,October 3, 2018
#Date in which the assignment is submitted is October 3, 2018.


# Installing some packages
install.packages("RCurl")
library(RCurl)


install.packages("RJSONIO")
library(RJSONIO)

#Step A: Load the data
#1	Read in the following JSON dataset
jsonLocation <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD" #storing the URl
jsonLocation

dataset <- getURL(jsonLocation) #getting the JSON data from the URL
mydata <- fromJSON(dataset, simplify = FALSE, nullValue = NA) #Transfomation of JSON data into R 
View(mydata)

myList <- mydata[[2]] # We dont need metadata therefore indexing only the real data


numRows <- length(myList)
numRows
df <- data.frame(matrix(unlist(myList), nrow=numRows, byrow=T), stringsAsFactors = FALSE) #Unlisting the dataset into a R dataframe for analysis
View(df)

#Step B: Clean the data
#2.	Remove the first 8 columns,
df <- df[,-1:-8] #removing the first 8 columns from the dataframe
str(df)

#3.	Then, to make it easier to work with, changing the name of the rest of the columns as follows:
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
colnames(df) <- namesOfColumns
View(df)

#Step C: Explore the data – using the dataframe you created
#installing SQLDF package for SQl queries
install.packages("sqldf")
library(sqldf)

#4.	What was the total number of accidents with injuries?
# 1st Solution: Running an SQL query on the dataframe
injuries <- sqldf("SELECT COUNT(INJURY) FROM df WHERE INJURY='YES'")
injuries
#2nd Solution: Using tapply function
d2 <- tapply(df$INJURY,df$INJURY,length)
d2
injuries.new <- d2[2]
injuries.new

#5.	How many accidents happened on Sunday?
#sunday_accidents <- sqldf("SELECT COUNT(DAY_OF_WEEK) FROM df GROUP BY DAY_OF_WEEK")
#sunday_accidents

# 1st Solution: Running an SQL query on the dataframe
sunday_accidents <- sqldf("SELECT COUNT(DAY_OF_WEEK) FROM df WHERE DAY_OF_WEEK='SUNDAY   '")
sunday_accidents

#2nd Solution: Using tapply function
d3 <- tapply(df$DAY_OF_WEEK,df$DAY_OF_WEEK,length)
d3
sunday_accidents.new <-d3[4]
sunday_accidents.new

#6.	How many injuries occurred each day of the week?
dfInjured <- sqldf("SELECT * FROM df WHERE INJURY='YES'") #Creating a filtered dataframe for accidents with injuries
eachday.injury <- tapply(dfInjured$INJURY,dfInjured$DAY_OF_WEEK,length) #calculating each day injury
eachday.injury



#Step D: Explore the data – using dplyr
#installing dplyr package
install.packages("dplyr")
library(dplyr)


df.GroupBydays <- group_by(df, DAY_OF_WEEK) #grouping the dataframe columns according to the days of the week
accidents <- summarize (df.GroupBydays, count = n(), injury = length(INJURY)) 
#creating a summary of accidents and injuries according to each day. n() gives total observations
#NOTE: In the assignment doc the function 'sum' was used instead of length which gives an error, as we cannot sum a column with character values.
accidents
str(df.GroupBydays)

# Then answer the following questions:
# 7.	What was the total number of accidents with injuries?
#Additonal Note: Filter is a function from dplyr package that works in a similar way as where in dfsql
#Total Accidents with Injury
print(paste("Total Accidents with Injury (using dplyr package): ",count(filter(df,INJURY=="YES"))))

# 8.	How many accidents happened on Sunday?
print(paste("Total Accidents on Sunday (using dplyr package): ",count(filter(df, DAY_OF_WEEK=="SUNDAY   "))))

# 9.	How many injuries occurred each day of the week?
count(filter(df.GroupBydays,INJURY=="YES"))

# 10.	 In a block comment, explain if you find doing the analysis with the dataframe directly, or using dplyr easier
#I believe sql is familar and comfortable which makes sqldf package easy to use, but the efficiency of sqldf may not be good.
#Also, after the knowing certain functions and methods of the dplyr package, it is very easy to use for analysis of R data.
# Therefore, I believe that using dplyr directly for analysis of the data frame can be much easier.


library(sqldf)

#Step E: Explore the distribution of the number of vehicles in accidents
#11.	What is the distribution of the number of vehicles in accidents on Friday?
#DISTRIBUTION OF FRIDAY ACCIDENT VEHICLES
df.GroupBydays.Friday <- sqldf("Select VEHICLE_COUNT,DAY_OF_WEEK From df where DAY_OF_WEEK='FRIDAY   '") #filtering out friday accidents
df.GroupBydays.Friday

df.GroupBydays.Friday$VEHICLE_COUNT <- as.numeric(df.GroupBydays.Friday$VEHICLE_COUNT) # As the VEHICLE_COUNT Column is a character column, we need to convert it into numeric data type to plot the histogram. 
is.na(df.GroupBydays.Friday$VEHICLE_COUNT) # finding out if there are any null values 
df.GroupBydays.Friday$VEHICLE_COUNT[is.na(df.GroupBydays.Friday$VEHICLE_COUNT)] <- 0 # Replacing null values with the integer '0'.
hist(df.GroupBydays.Friday$VEHICLE_COUNT) #plotting the histogram for the distribution of vehicles
quantile(df.GroupBydays.Friday$VEHICLE_COUNT,prob=c(0.25,0.5,0.75)) #quantile distribution of vehicles on Friday
summary(df.GroupBydays.Friday$VEHICLE_COUNT) # getting a summary of values 


#12.	How does this distribution compare with the distribution of the number of vehicles in accidents on Sunday?  
#DISTRIBUTION OF SUNDAY ACCIDENT VEHICLES
df.GroupBydays.Sunday <- sqldf("Select VEHICLE_COUNT,DAY_OF_WEEK From df where DAY_OF_WEEK='SUNDAY   '")
df.GroupBydays.Sunday

df.GroupBydays.Sunday$VEHICLE_COUNT <- as.numeric(df.GroupBydays.Sunday$VEHICLE_COUNT)
df.GroupBydays.Sunday$VEHICLE_COUNT[is.na(df.GroupBydays.Sunday$VEHICLE_COUNT)] <- 0
hist(df.GroupBydays.Sunday$VEHICLE_COUNT)
quantile(df.GroupBydays.Sunday$VEHICLE_COUNT,prob=c(0.25,0.5,0.75)) 
summary(df.GroupBydays.Sunday$VEHICLE_COUNT)

#By looking at the distribution of number of vehicles in accidents on Sunday and Friday, we can observe that there is a lesser chance of 
# a vehicle meeting with an accident on Sunday than on Friday. This can observed by comparing the frequency of the number of vehicles involved in 
# accident on both days



