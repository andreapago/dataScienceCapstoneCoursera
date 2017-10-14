library(tm)


initializeTextMining<-function(){
fileConn<-file("subsetEnglish.txt")
totalLines<-readLines(fileConn)

clean<-removeNumbers(totalLines)
clean<-removePunctuation(clean)
clean
}


computeWordFrequencyDist<-function(cleanData){
freqTerms<-termFreq(cleanData)
#saveRDS(freqTerms,"freqTermsSmallTweet.RDS")
matrixTerms<-as.matrix(freqTerms)
dt<-data.frame(matrixTerms)
dt$words<-rownames(dt)
colnames(dt)<-c("occurrencies","word")
plot(freqTerms)
#hist(dt$occurrencies,breaks = 100)
return(dt)
}

computeWordFrequencyDistMatrix<-function(cleanData){
  freqTerms<-termFreq(cleanData)
  #saveRDS(freqTerms,"freqTermsSmallTweet.RDS")
  matrixTerms<-as.matrix(freqTerms)
  dt<-data.frame(matrixTerms)
  dt$words<-rownames(dt)
  colnames(dt)<-c("occurrencies","word")
 # plot(freqTerms)
  #hist(dt$occurrencies,breaks = 100)
  matrixTerms
}




BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


NgramTokenizer <- function(x,n){
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
}



##2-3 grams

get2W3WDistrib<-function(clean){

freqTerms<-readRDS("freqTermsSmallTweet.RDS")

tdm2 <- termFreq(clean[1:1000], control = list(tokenize = BigramTokenizer))
tdm3 <- termFreq(clean[1:1000], control = list(tokenize = TrigramTokenizer))

matrixTerms<-as.matrix(tdm2)
dt<-data.frame(matrixTerms)
dt$words<-rownames(dt)
colnames(dt)<-c("occurrencies","word")
#plot(freqTerms)
hist(dt$occurrencies)

matrixTerms<-as.matrix(tdm3)
dt<-data.frame(matrixTerms)
dt$words<-rownames(dt)
colnames(dt)<-c("occurrencies","word")
#plot(freqTerms)
hist(dt$occurrencies)

}



getNWDistrib<-function(clean, n){
  
  if (n==2){
  tdmN <- termFreq(clean, control = list(tokenize = BigramTokenizer))
  } else if (n==3){
    tdmN <- termFreq(clean, control = list(tokenize = TrigramTokenizer))
    
  }
 
  matrixTerms<-as.matrix(tdmN)
  dt<-data.frame(matrixTerms)
  dt$words<-rownames(dt)
  colnames(dt)<-c("occurrencies","gram-words")
  #hist(dt$occurrencies)
  dt
}


getNWDistribMatrix<-function(clean, n){
  
  if (n==2){
    tdmN <- termFreq(clean, control = list(tokenize = BigramTokenizer))
  } else if (n==3){
    tdmN <- termFreq(clean, control = list(tokenize = TrigramTokenizer))
    
  }
  
  matrixTerms<-as.matrix(tdmN)
  dt<-data.frame(matrixTerms)
  dt$words<-rownames(dt)
  colnames(dt)<-c("occurrencies","gram-words")
  #hist(dt$occurrencies)
  dt
}











coverLang<-function(cleanData){
  subset <-clean[1:30000]
  WF<-computeWordFrequencyDist(subset)
  WFordered<-WF[order(WF$occurrencies),]
  totalWords<-sum(WF$occurrencies)
  len<-totalWords
  wordsToLookTo<-round(len*.5)
  #WFordered[1:wordsToLookTo]
  
  testtt<-WFordered[cumsum(WFordered$occurrencies)>=wordsToLookTo,]
  print(1-(length(testtt[,2])/len))
 
  
}


# The first step in building a predictive model for text is understanding the distribution and relationship between the words, tokens, and phrases in the text. The goal of this task is to understand the basic relationships you observe in the data and prepare to build your first linguistic models.
# 
# Tasks to accomplish
# 
# Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
# Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.
# 
# Questions to consider
# 
# Some words are more frequent than others - what are the distributions of word frequencies?
# What are the frequencies of 2-grams and 3-grams in the dataset?
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# How do you evaluate how many of the words come from foreign languages?
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?
# 
