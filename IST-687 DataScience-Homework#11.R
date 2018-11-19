#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#11-  Wielding the tm package and counting words
#Date of assignment due Wednesday,November 22, 2018
#Date in which the assignment is submitted is November 22, 2018.

install.packages("tm")
library(tm)

install.packages("wordcloud")
library(wordcloud)

install.packages("RJSONIO")
library(RJSONIO)

dataset.name <- "hotelSurveySherison.json" #storing the json file in a variable
hotelSurveyOut <- fromJSON(dataset.name, simplify = TRUE, nullValue = NA) #transfering the json data into R and replacing null values with NA
hotelSurvey <- data.frame(hotelSurveyOut) #converting into a data frame
View(hotelSurvey)
str(hotelSurvey)

pos <- "positive-words.txt"
View(pos)

p <- scan(pos,character(0),sep = "\n")
View(p)
summary(p)

neg <- "negative-words.txt"
View(neg)

n <- scan(neg, character(0),sep = "\n")
View(n)
summary(n)


p <- p[-c(1:34)]
n <- n[-c(1:34)]

createWordCounts <- function(vfreetext){
  words.vec <- VectorSource(vfreetext)
  words.corpus <- Corpus(words.vec)
  words.corpus <- tm_map(words.corpus,content_transformer(tolower))
  words.corpus <- tm_map(words.corpus,removePunctuation)
  words.corpus <- tm_map(words.corpus,removeNumbers)
  words.corpus <- tm_map(words.corpus,removeWords,stopwords("english"))
  
  tdm <- TermDocumentMatrix(words.corpus)
  tdm
  
  m <- as.matrix(tdm)
  wordCounts <- rowSums(m)
  wordCounts <- sort(wordCounts,decreasing = TRUE)
  
  return(wordCounts)
}

wordCounts <- createWordCounts(hotelSurvey$freeText)
View(wordCounts)  





getmatched <- function(wordCounts,PorN){
  words <- names(wordCounts)
  
  matched <- match(words,PorN,nomatch=0)
  return(matched)
}
  matchedP <- getmatched(wordCounts,p)
  matchedN <- getmatched(wordCounts,n)
View(matchedP)
View(matchedN)


CalcsPosNeg <- function(wordCounts,matchedP,matchedN){
  
  pTotal <- sum(wordCounts[which(matchedP != 0)])
  nTotal <-sum(wordCounts[which(matchedN != 0)])
  
  
  
}



genWordCloud <- function(wordCounts){
  
  cloudFrame <- data.frame(word = names(wordCounts), freq=wordCounts)
  
  wordcloud(names(wordCounts),wordCounts,min.freq=2,max.words = 30,rot.per = 0.35,colors=brewer.pal(8,"Dark2"))
}

genWordCloud(wordCounts)


genBarChart <- function(wordCounts,matched){
  
  sortedWords <- sort(wordCounts[matched > 1])
  barplot(sortedWords,las=2,cex.names = 0.75)
}

genBarChart(wordCounts,matchedP)
genBarChart(wordCounts,matchedN)

hotelSurveyLow <- hotelSurvey[hotelSurvey$overallCustSat<7,]
wordCounts <- createWordCounts(hotelSurveyLow$freeText)
genWordCloud(wordCounts)



hotelSurveyHigh <- hotelSurvey[hotelSurvey$overallCustSat>=7,]
wordCounts <- createWordCounts(hotelSurveyHigh$freeText)
genWordCloud(wordCounts)
