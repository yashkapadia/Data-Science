#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#6- Data Visualization
#Date of assignment due Wednesday,October 10, 2018
#Date in which the assignment is submitted is October 10, 2018.

#Step A: Load and Merge datasets
#1)	Read in the census dataset (using the function created in HW 3)
MyCleanDataframe <- function(){
  states <- read.csv(file ="states.csv")
  
  states<-states[-1,] # deleting 1st row
  
  
  states <- states[-52,]# deleting last row
  
  
  
  states <-states[-1:-4]#deleting 1st 4 columns
  
  
  
  colnames(states) <- c("stateName","population","popOver18","percentOver18")#changing column names
  str(states)
return(states)
}
states <- MyCleanDataframe()
View(states)

#2)	Copy the USArrests dataset into a local variable
arrests <- USArrests

#3)	Create a merged dataframe -- with the attributes from both dataset
arrests$stateName <- row.names(arrests) #adding the name of the column 
mergedf <- merge(arrests,states,by="stateName")
View(mergedf)

#Step B: Explore the Data – Understanding distributions
#4)	Create a histogram using ggplot2() for the population and a different histogram for the murder rate
install.packages("ggplot2")
library(ggplot2)

#Histogram for Population
myplotpop <- ggplot(mergedf,aes(x=population)) #choosing the dataframe and setting the aesthetics for the plot
myplotpop <- myplotpop + geom_histogram(binwidth = 500000) #defining the geometry of the plot ie. Histogram
myplotpop <- myplotpop + ggtitle("Histogram of the Population") #adding a title for the plot
myplotpop

#Histogram for Murder rate
myplotmurder <- ggplot(mergedf,aes(x=Murder)) #choosing the dataframe and setting the aesthetics for the plot
myplotmurder <- myplotmurder + geom_histogram(binwidth = 5,color='black',fill='blue') #defining the geometry of the plot ie. Histogram
myplotmurder <- myplotmurder + ggtitle("Histogram of the Murder rate") #adding a title for the plot
myplotmurder

#Then build similar code to create histograms of each of the other three variables in the merged data frame. 
#What parameter will you have to adjust to make the other histograms look right?

#The other three variables are UrbanPop, Assault and Rape
#Histogram for UrbanPop
myploturban <- ggplot(mergedf,aes(x=UrbanPop)) #choosing the dataframe and setting the aesthetics for the plot
myploturban <- myploturban + geom_histogram(binwidth = 5,color='black',fill='red') #defining the geometry of the plot ie. Histogram
myploturban <- myploturban + ggtitle("Histogram of the UrbanPopulation") #adding a title for the plot
myploturban

#Histogram for Rape
myplotrape <- ggplot(mergedf,aes(x=Rape)) #choosing the dataframe and setting the aesthetics for the plot
myplotrape <- myplotrape + geom_histogram(binwidth = 5,color='black',fill='green') #defining the geometry of the plot ie. Histogram
myplotrape <- myplotrape + ggtitle("Histogram of the Rape") #adding a title for the plot
myplotrape

#Histogram for Assault
myplotassault <- ggplot(mergedf,aes(x=Assault)) #choosing the dataframe and setting the aesthetics for the plot
myplotassault <- myplotassault + geom_histogram(binwidth = 50,color='black',fill='orange') #defining the geometry of the plot ie. Histogram
myplotassault <- myplotassault + ggtitle("Histogram of the Assault") #adding a title for the plot
myplotassault

#What parameter will you have to adjust to make the other histograms look right?
#We need to adjust the aesthetics ie. X-axis variable  and sometimes the binwidth to create histograms for different variables.
#Adjusting these parameters will make the histogram look right.


#5)	Create a boxplot for the population, and a different boxplot for the murder rate.
#Boxplot for Population
myplotpop2 <- ggplot(mergedf,aes(y=population,x=factor(0))) #choosing the dataframe and setting the aesthetics for the plot
myplotpop2 <- myplotpop2 + geom_boxplot() #defining the geometry of the plot ie. Boxplot
myplotpop2 <- myplotpop2 + ggtitle("Boxplot of the Population") #adding a title for the plot
myplotpop2

#Boxplot for Murder rate
myplotmurder2 <- ggplot(mergedf,aes(y=Murder,x=factor(0))) #choosing the dataframe and setting the aesthetics for the plot
myplotmurder2 <- myplotmurder2 + geom_boxplot() #defining the geometry of the plot ie. Boxplot
myplotmurder2 <- myplotmurder2 + ggtitle("Boxplot of the Murder Rate") #adding a title for the plot
myplotmurder2

#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
#I think the histogram visulization is more helpful because it is easy to visualize and understand when there are wide variences
#in the obsereved frequencies in a particular dataset. It becomes easy to gain insights and answer questions using an histogram rather than a boxplot.

#Step C: Which State had the Most Murders – bar charts
#7)	Calculate the number of murders per state
#Barchart for Murder
mergedf$numMurders <- mergedf$population*mergedf$Murder/100000 #adding a new column numMurders and calculating number of murders per state.
View(mergedf)

#8)	Generate a bar chart, with the number of murders per state
murder.bar <- ggplot(mergedf,aes(x=stateName,y=numMurders))#setting the required aesthetics 
murder.bar <- murder.bar + geom_col() # defining the geometry of the plot ie. Bar chart~geom_col() method
murder.bar <- murder.bar +ggtitle("Total Murders") #adding a title
murder.bar

#9)	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels, also add a title named “Total Murders”.
#Bar chart for Number of murders per state.
murder.bar1 <- ggplot(mergedf,aes(x=stateName,y=numMurders))#setting the required aesthetics 
murder.bar1 <- murder.bar1 + geom_col() # defining the geometry of the plot ie. Bar chart~geom_col() method
murder.bar1 <- murder.bar1+ theme(axis.text.x =
                                  element_text(angle = 90, hjust = 1))#adjusting the state name text on the X-Axis
murder.bar1 <- murder.bar1 +ggtitle("Total Murders") #adding a title
murder.bar1


#Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate
#Reordering bar chart
murder.bar2 <- ggplot(mergedf,aes(x=reorder(stateName,numMurders),y=numMurders)) # sorting the X-axis stateNames according to the Murder rate
murder.bar2 <- murder.bar2 + geom_col() # defining the geometry of the plot ie. Bar chart~geom_col() method
murder.bar2 <- murder.bar2+ theme(axis.text.x =
                                  element_text(angle = 90, hjust = 1)) #adjusting the state name text on the X-Axis
murder.bar2 <- murder.bar2 +ggtitle("Total Murders-Sorted")#adding a title
murder.bar2

#Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
#PercentOver18 is filled as a colorbar
murder.bar3 <- ggplot(mergedf,aes(x=reorder(stateName,numMurders),y=numMurders,fill=percentOver18))#setting the aesthetics and using fill with values of perecentOver18 which becomes the color reference for the bar.
murder.bar3 <- murder.bar3 + geom_col() # defining the geometry of the plot ie. Bar chart~geom_col() method
murder.bar3 <- murder.bar3+ theme(axis.text.x =
                                    element_text(angle = 90, hjust = 1))#adjusting the state name text on the X-Axi
murder.bar3 <- murder.bar3 +ggtitle("Total Murder-filled with percentOver18")
murder.bar3


#Step D: Explore Murders – scatter chart
#12)	 Generate a scatter plot – have population on the X axis, the percent over 18 on the y axis, and the size & color represent the murder rate
murder.scatter <- ggplot(mergedf,aes(x=population,y=percentOver18))#setting the required aesthetics
murder.scatter <- murder.scatter + geom_point(aes(size=Murder,color=Murder))  #defining the geometry of the plot ie.Scatter plot~geom_point() method and setting size and color with Murder as the aesthetics.
murder.scatter <- murder.scatter + ggtitle("Murders-Scatter Plot") #adding a title
murder.scatter


