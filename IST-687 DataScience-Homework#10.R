#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#10- Prediction with Support Vector Machine
#Date of assignment due Wednesday,November 15, 2018
#Date in which the assignment is submitted is November 15, 2018.

#installing packages for ksvm model
install.packages("kernlab")
library(kernlab)

install.packages("RJSONIO")
library(RJSONIO)

#Part A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveyBarriot), as a JSON file.

dataset.new <- "hotelSurveyBarriot.json" #storing the json file in a variable
hotelSurveyOut <- fromJSON(dataset.new, simplify = TRUE, nullValue = NA) #transfering the json data into R and replacing null values with NA
hotelSurvey <- data.frame(hotelSurveyOut) # transfering the data into a dataframe


#Part B: Create a happy customer variable 
#2.	To focus on predicting happy customers, we need to generate a new column 
hotelSurvey$happyCustomer <- hotelSurvey$overallCustSat > 7 #adding a logical column in the dataframe
View(hotelSurvey)

#Part C: Create training and test data sets

#4.	Use the dim( ) function to demonstrate that the resulting training data set and test data set contain the appropriate number of cases.

dim(hotelSurvey)
randIndex <- sample(1:dim(hotelSurvey)[1]) #generating random indexes
summary(randIndex)
length(randIndex)


cut_point2_3 <- floor(2*dim(hotelSurvey)[1]/3) #dividing the data frame into 2/3 and 1/3 parts for training and testing datasets
cut_point2_3

train.data <- hotelSurvey[randIndex[1:cut_point2_3],]
dim(train.data)

test.data <- hotelSurvey[randIndex[(cut_point2_3+1):dim(hotelSurvey)[1]],]
dim(test.data)

#Part D: Build a Model using ksvm( ) 
#5.	Build a support vector model using the ksvm( ) function using two or three of the variables to predict a happy customer. 

#building the svm model
svm.output <- ksvm(happyCustomer~hotelFriendly+hotelClean+whenBookedTrip, data = train.data, kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)


#6.	Write a block comment that summarizes what you learned from the book about those parameters. The two parameters of greatest interest are C=5 and cross=3.
# The parameter kernel defines which kernel is to be used for building the model. The kernel can be radial based function, linear etc.
# The next parameter kpar refers to the parameters that control the operation of the radial basis function.
# The C stands for costs of contraints. As we increase the value of C we get more number of support vectors but at the cost of increasing the cross-validation error
# The cross parameter is for the cross-validation. Cross Validation helps to avoid overfitting.

#7.	Store the output of kvsm( ) in a variable and then echo that variable to the console.   
svm.output

#Part E: Predict Values in the Test Data and Create a Confusion Matrix
#8.	Use the predict( ) function to validate the model against test data. 
svm.predict <- predict(svm.output,test.data, type='votes')

happypred <-data.frame(svm.predict[,1]) #creating buckets for the predicted output as '1' and '0'
happypred[happypred >= .8] <- 1
happypred[happypred < .8] <- 0

#Review the contents of svmPred using str( ) and head( ).
str(happypred)
head(happypred)

#10.	Create a confusion matrix (a 2 x 2 table) that compares the second row of svmPred to the contents of testData$happy variable.
#creating a confusion matrix
confusion.matrix <- data.frame(test.data[,12],happypred)
matrix <- table(confusion.matrix)
matrix

str(matrix)

#11.	Calculate an error rate based on what you see in the confusion matrix
#calculating the error rate - which is summation of False positive and False negative divided by total number.
error.rate <- (matrix[1,2]+matrix[2,1])/(matrix[1,1]+matrix[1,2]+matrix[2,1]+matrix[2,2])
error.rate

#Part F: Find a good prediction
#12.	Repeat Parts C and D to try and improve your prediction
#improving SVM algorithm
#increasing the "C" value which in turn, increases the number of support vectors generated and also slightly reduces the error
svm.output2 <- ksvm(happyCustomer~hotelFriendly+hotelClean+whenBookedTrip, data = train.data, kernel="rbfdot",kpar="automatic",C=50,cross=3, prob.model=TRUE)
svm.output2
svm.predict2 <- predict(svm.output2,test.data, type='votes')
happypred2 <-data.frame(svm.predict2[,1])
happypred2[happypred2 >= .8] <- 1
happypred2[happypred2 < .8] <- 0


confusion.matrix2<- data.frame(test.data[,12],happypred2)
matrix2 <- table(confusion.matrix2)
matrix2


#13.	Explain, in a block comment, why it is valuable to have a “test” dataset that is separate from a “training” dataset?

#It is important to have a test data set different from a training dataset because after training our model with the training dataset,
#there should be some fresh and unseen data available for the model to predict. If we use the same dataset for training as well as prediction, it 
#will lead to a problem of overfitting of the model.


#extra code just to try things out
svm.output3 <- ksvm(happyCustomer~hotelFriendly+hotelClean+whenBookedTrip, data = train.data2, kernel="rbfdot",kpar="automatic",C=25,cross=10, prob.model=TRUE)
svm.output3
svm.predict3 <- predict(svm.output3,test.data2, type='votes')
happypred3 <-data.frame(svm.predict3[,1])
happypred3[happypred3 >= .8] <- 1
happypred3[happypred3 < .8] <- 0


confusion.matrix3<- data.frame(test.data2[,12],happypred3)
matrix3 <- table(confusion.matrix3)
matrix3

#extra code - PLZ IGNORE
install.packages("caret")
library(caret)
matrix.con <- confusionMatrix(test.data[,12],happypred1$new)
matrix.con
