###########

# Tasks to accomplish
# 
# Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.
# 
# Questions to consider
# 
#     How can you efficiently store an n-gram model (think Markov Chains)? DONE BELOW
#     How can you use the knowledge about word frequencies to make your model smaller and more efficient? I WOULD JUST COMPUTE THE PROBABILITY FOR THE MOST FREQUENT WORDS, LIKE THE MOST 200 USED (E.G., 20% OF THE WORDS COVER THE 80% OCCURRENCIES OR SO)
#     How many parameters do you need (i.e. how big is n in your n-gram model)? I THINK TO USE N=2, IT SHOULD BE ENOUGH AND THE APPROACH COULD BE APPLIED RECOURSIVELY
#     Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
# How do you evaluate whether your model is any good?
# How can you use backoff models to estimate the probability of unobserved n-grams?

###########


#https://www.jstatsoft.org/article/view/v072i03


library(PST)
source("exploration.R")

clean<-initializeTextMining()

WFdata<-computeWordFrequencyDist(clean[1:1000])

testMatrix<-getNWDistribMatrix(clean[1:1000],2)

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

occurrenciesFirst<-aggregate(occurrencies ~ as.character(first), data = matrix2, sum)
colnames(occurrenciesFirst)<-c("word","occFirst")

hashtable<-merge(hashtable,occurrenciesFirst,by.x = "word", by.y = "word")


#occurrences have to be put in a way that the sum of a line is 1 sice is conditional probability
##############> sum(testMatrix[testMatrix$first=="a",1])

testMatrix<-merge(hashtable, testMatrix, by.x = "word", by.y = "first")
totalOcc<-(testMatrix$occurrencies)
testMatrix$prob<-testMatrix$occurrencies/testMatrix$occFirst

pippo<-sparseMatrix(testMatrix$i, testMatrix$j, x=testMatrix$prob)

gramsMchain <- new("markovchain", states = as.character(hashtable$word), byrow = T, 
                transitionMatrix = as.matrix(pippo), name = "Word 2-gram transition matrix")





}
