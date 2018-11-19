iris_data <- read.csv(file='iris.csv',stringsAsFactors = FALSE)
View(iris_data)

sum(!complete.cases(iris_data))

str(iris_data)
summary(iris_data)

iris_data$Species <- as.factor(iris_data$Species)
hist(iris_data$SepalLengthCm,col="Blue" )

train_indices <- sample(1:nrow(iris_data), replace=F, size=nrow(iris_data)*0.7)

iris_train <- iris_data[train_indices,]
iris_test <- iris_data[-train_indices,]

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

svm_model <- train(Species~., data = iris_train, method ='svmLinear')
predict_iris <- predict(svm_model,newdata = iris_test)
confusionMatrix(predict_iris, iris_test$Species)


install.packages("pROC")
library(pROC)

auc(multiclass.roc(as.numeric(iris_test$Species),as.numeric(predict_iris)))

install.packages("")
