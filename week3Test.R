#Test Week 4 
library(Matrix)
library(markovchain)
source("exploration.R")


subsetData<-function(){
  set.seed(10)
  conInput<-file("../final/en_US/en_US.twitter.txt", "r") 
  totalLines<-readLines(conInput) 
  close(conInput)
  conInput2<-file("../final/en_US/en_US.blogs.txt", "r") 
  totalLines2<-readLines(conInput) 
  close(conInput2)
  conInput3<-file("../final/en_US/en_US.news.txt", "r") 
  totalLines3<-readLines(conInput3) 
  close(conInput3)
  totalLines<-c(totalLines,totalLines2,totalLines3)
  selected<-rbinom(length(totalLines),1,0.05)
  
  fileConn<-file("subsetEnglish.txt")
  writeLines(totalLines[selected==1], fileConn)
  close(fileConn)
}




buildN2Mchain<-function(){

clean<-initializeTextMining()

#WFdata<-computeWordFrequencyDist(clean)

testMatrix<-getNWDistribMatrix(clean,2)

test<-strsplit(testMatrix$`gram-words`, " ")
firstW<-lapply(test, function(x) x[1])
secondW<-lapply(test, function(x) x[2])
testMatrix$first<-firstW
testMatrix$second<-secondW

words<-unique(c(unique(firstW), unique(secondW)))
hashtable<-data.frame(as.character(words),seq(1,length(words),1))
colnames(hashtable)<-c("word", "id")
rownames(hashtable)<-hashtable$word
testMatrix$i<-hashtable[as.character(testMatrix$first),2]
testMatrix$j<-hashtable[as.character(testMatrix$second),2]

#occurrenciesFirst<-aggregate(occurrencies ~ as.character(first), data = testMatrix, sum)
#colnames(occurrenciesFirst)<-c("word","occFirst")

#hashtable<-merge(hashtable,occurrenciesFirst,by.x = "word", by.y = "word")


#occurrences have to be put in a way that the sum of a line is 1 sice is conditional probability
##############> sum(testMatrix[testMatrix$first=="a",1])

testMatrix<-merge(hashtable, testMatrix, by.x = "word", by.y = "first")
totalOcc<-(testMatrix$occurrencies)
testMatrix$prob<-testMatrix$occurrencies/testMatrix$occFirst

pippo<-sparseMatrix(testMatrix$i, testMatrix$j, x=testMatrix$prob)

gramsMchain <- new("markovchain", states = as.character(hashtable$word), byrow = T, 
                   transitionMatrix = as.matrix(pippo), name = "Word 2-gram transition matrix")
gramsMchain
}




buildN3Mchain<-function(){
  
  clean<-initializeTextMining()
  
  #WFdata<-computeWordFrequencyDist(clean)
  
  testMatrix<-getNWDistribMatrix(clean,3)
  
  test<-strsplit(testMatrix$`gram-words`, " ")
  firstW<-lapply(test, function(x) c(x[1], x[2]))
  secondW<-lapply(test, function(x) x[3])
  testMatrix$first<-firstW
  testMatrix$second<-secondW
  
  words<-unique(c(unique(firstW), unique(secondW)))
  hashtable<-data.frame(as.character(words),seq(1,length(words),1))
  colnames(hashtable)<-c("word", "id")
  rownames(hashtable)<-hashtable$word
  testMatrix$i<-hashtable[as.character(testMatrix$first),2]
  testMatrix$j<-hashtable[as.character(testMatrix$second),2]
  
  occurrenciesFirst<-aggregate(occurrencies ~ as.character(first), data = testMatrix, sum)
  colnames(occurrenciesFirst)<-c("word","occFirst")
  
  hashtable<-merge(hashtable,occurrenciesFirst,by.x = "word", by.y = "word")
  
  
  #occurrences have to be put in a way that the sum of a line is 1 sice is conditional probability
  ##############> sum(testMatrix[testMatrix$first=="a",1])
  
  testMatrix<-merge(hashtable, testMatrix, by.x = "word", by.y = "first")
  totalOcc<-(testMatrix$occurrencies)
  testMatrix$prob<-testMatrix$occurrencies/testMatrix$occFirst
  
  pippo<-sparseMatrix(testMatrix$i, testMatrix$j, x=testMatrix$prob)
  
  gramsMchain3 <- new("markovchain", states = as.character(hashtable$word), byrow = T, 
                     transitionMatrix = matrix(pippo), name = "Word 3-gram transition matrix")
  gramsMchain3
}




