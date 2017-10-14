library(RWeka)
library(tm)
library(parallel)
library(data.table)
library(stringr)




initExperiment<-function(){

input<-file("subsetEnglish10.txt")
lines<-readLines(input)
close(input)
cpus<-7

cl<-makeCluster(cpus)

clusterEvalQ(cl, c(library(tm)))

clean<-parLapply(cl, lines, removeNumbers)
clean<-parLapply(cl, clean, removePunctuation)
clean<-parLapply(cl, clean, stripWhitespace)


stopCluster(cl)
#orinalClean<-clean


#clean<-clean[1:5]



clean
}


printMessage<-function(){
  message("test")
}


makeGramProbMatrix<-function(textClean){

message("starting computation of Grams...")

  cpus<-7
  
  cl<-makeCluster(cpus)
  
  clusterEvalQ(cl, c(library(tm)))
  
clusterExport(cl, c("UnigramTokenizer","BigramTokenizer","TrigramTokenizer","QuadrigramTokenizer", "PentagramTokenizer"))
  
  
clean<-textClean
#test<-parLapply(cl, clean, getNWDistrib, 2)
test<-parLapply(cl, clean, getNWDistrib, 4)
nMinOne<-parLapply(cl, clean, getNWDistrib,3)
nMinTwo<-parLapply(cl, clean, getNWDistrib,2)
unigram<-parLapply(cl,clean,getNWDistrib,1)

DT<-rbindlist(test)
DT<-data.table(DT)
setkey(DT,gramWords)
DTall<-aggregate(occurrencies~gramWords,data=DT,FUN=sum)

DT2<-rbindlist(nMinOne)
DT2<-data.table(DT2)
setkey(DT2,gramWords)
DTallNMinOne<-aggregate(occurrencies~gramWords,data=DT2,FUN=sum)
DTall<-data.table(DTall)
DTallNMinOne<-data.table(DTallNMinOne)


DT3<-rbindlist(nMinTwo)
DT3<-data.table(DT3)
setkey(DT3,gramWords)
DTallNMinTwo<-aggregate(occurrencies~gramWords,data=DT3,FUN=sum)
DTallNMinTwo<-data.table(DTallNMinTwo)


DTUnigram<-rbindlist(unigram)
DTUnigram<-data.table(DTUnigram)
setkey(DTUnigram,gramWords)
DTUnigram<-aggregate(occurrencies~gramWords,data=DTUnigram,FUN=sum)
DTAllUnigram<-data.table(DTUnigram)



rm(DT,DT2,textClean,test, DT3, nMinTwo, nMinOne,unigram)

gc()
#DTall<-DTall[DTall$occurrencies>1,]


message("separating last word from extracted grams ")
#test<-strsplit(DTall$`gram-words`, " ")
#strsplit("UK, USA, Germany", ",\\s*(?=[^,]+$)", perl=TRUE)

DTall<-splitNgramSubset(DTall,DTallNMinOne,cl)
DTtwo<-splitNgramSubset(DTallNMinOne,DTallNMinTwo,cl)
DTthree<-splitNgramSubset(DTallNMinTwo,DTAllUnigram,cl)

# split<-parLapply(cl,DTall$gramWords, function(x) strsplit(x, ' (?=[^ ]+$)', perl=TRUE))#x[1])
# firstW<-parLapply(cl,split, function(x) unlist(x)[1])
# secondW<-parLapply(cl,split, function(x) unlist(x)[2])
# DTall[,first:=unlist(firstW)]
# DTall[,second:=unlist(secondW)]
# 
# setkey(DTallNMinOne,gramWords)
# setkey(DTall,first)
# 
# DTall<-DTallNMinOne[DTall,]


#DTallNMinOne<-DTallNMinOne[DTallNMinOne$occurrencies>1,]


rm(DTallNMinOne, DTallNMinTwo)
gc()
stopCluster(cl)

#words<-unique(c(unique(firstW), unique(secondW)))
#hashtable<-data.frame(as.character(words),seq(1,length(words),1))
#colnames(hashtable)<-c("word", "id")
#rownames(hashtable)<-hashtable$word
#DTall$i<-hashtable[as.character(DTall$first),2]
#DTall$j<-hashtable[as.character(DTall$second),2]


message("computing probabilities")
#occurrenciesFirst<-aggregate(occurrencies ~ as.character(first), data = DTall, sum)
#colnames(occurrenciesFirst)<-c("word","occFirst")

#hashtable<-merge(hashtable,occurrenciesFirst,by.x = "word", by.y = "word")

#DTall<-merge(hashtable, DTall, by.x = "word", by.y = "first")
#totalOcc<-(DTall$occurrencies)
gamma = 0.5
DTall[,prob:=i.occurrencies/occurrencies]
DTtwo[,prob:=i.occurrencies/occurrencies]
DTthree[,prob:=i.occurrencies/occurrencies]
DTall[,occMinGamma:=i.occurrencies-gamma]
DTtwo[,occMinGamma:=i.occurrencies-gamma]
DTthree[,occMinGamma:=i.occurrencies-gamma]
DTall[,probObsCorr:=occMinGamma/occurrencies]
DTtwo[,probObsCorr:=occMinGamma/occurrencies]
DTthree[,probObsCorr:=occMinGamma/occurrencies]
# DTalltest<-aggregate(probObsCorr~gramWords,data=DTall,FUN=sum)
# DTtwotest<-aggregate(probObsCorr~gramWords,data=DTtwo,FUN=sum)
# 
# DTalltest<-data.table(DTalltest)
# setkey(DTalltest,gramWords)
# DTalltest[,alpha:=1-probObsCorr]
# 
# DTtwotest<-data.table(DTtwotest)
# setkey(DTtwotest,gramWords)
# DTtwotest[,alpha:=1-probObsCorr]


#setkey(DTall,gramWords)

#DTalltest[DTall,]


#setkey(DTtwo,gramWords)

#DTtwotest[DTtwo,]

#print(10-11)

#DTall<-data.table(DTall)
return(list(DTall,DTtwo,DTthree))

}


#listGrams 1:4grams 2:3grmas 3:2grams
probabilityWord<-function(listGrmas, wordPreceeding){ #wordPreceeding can be a more words (i.e., gram)
  availableGrams<-dim(listGrmas)
  numSplits<-length(unlist(str_split(wordPreceeding, " ")))
  dime<-0
  active<-4-numSplits #max supported is 4-grams at the moment
  while(dime==0){
    seen<-listGrmas[[active]][gramWords==wordPreceeding]
    dime<-dim(seen)[1]
    active<-active+1
    wordPreceeding<-unlist(str_split(wordPreceeding," ",n=2))[2]
  }
    #trailWord<-unlist(str_split(wordPreceeding," ",n=2))[2]
  if(!is.na(wordPreceeding)){
    unseen<-listGrmas[[active]][gramWords==wordPreceeding]
    merged<-computeUnseenprob(seen,unseen)
  }
  else{
    seen[,finalProb:=prob]
    merged<-seen
  }
    selectedWord<-sample(x = merged$second, size=15, replace = T, prob=merged$finalProb)
    selectedWord
    

}


computeUnseenprob<-function(seen, unseen){ #}, alphaDT){
  
  # split<-unlist(strsplit(wordPreceeding, ' (?=[^ ]+$)', perl=TRUE))#x[1]))
  # w1<-split[-1]
  # w2<-split[-2]
  # alphaVal<-alphaDT[gramWords==w2,3]
  # occurrenciesw2<-probDT[gramWords==w2,2][1]
  # occurrenciesw1<-probDT[gramWords==w1,2][1]
  # prob<-alphaVal*occurrenciesw1/occurrenciesw2
  # prob
  
setkey(seen,second)
setkey(unseen,second)
merged<-seen[unseen,]

merged$i.occurrencies[is.na(merged$i.occurrencies)]<-0
merged$probObsCorr[is.na(merged$probObsCorr)]<-0
leftDiscProb<-1-sum(merged$probObsCorr)
merged[,test:=i.i.occurrencies-i.occurrencies,]
if (sum(merged[,test])==0){
merged$test=1}
merged[,probUnseen:=leftDiscProb*test/sum(test)]
merged[,finalProb:=probObsCorr+probUnseen]

merged

  
  
  
  
}
#pippo<-sparseMatrix($i, testMatrix$j, x=testMatrix$prob)




splitNgramSubset<-function(DTall,DTallNMinOne,cl){
  split<-parLapply(cl,DTall$gramWords, function(x) strsplit(x, ' (?=[^ ]+$)', perl=TRUE))#x[1])
  firstW<-parLapply(cl,split, function(x) unlist(x)[1])
  secondW<-parLapply(cl,split, function(x) unlist(x)[2])
  DTall[,first:=unlist(firstW)]
  DTall[,second:=unlist(secondW)]
  
  setkey(DTallNMinOne,gramWords)
  setkey(DTall,first)
  
  DTall<-DTallNMinOne[DTall,]
  
  DTall
  
}




#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#QuadrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))


#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

assignmentTestWeek3<-function(ngram,n1gram){
# print(paste("1:",probabilityWord(ngram,n1gram,"case of")))
# print(paste("2:",probabilityWord(ngram,n1gram,"mean the")))
# print(paste("3:",probabilityWord(ngram,n1gram,"me the")))
# print(paste("4:",probabilityWord(ngram,n1gram,"but the")))
# print(paste("5:",probabilityWord(ngram,n1gram,"at the")))
# print(paste("6:",probabilityWord(ngram,n1gram,"on my")))
# print(paste("7:",probabilityWord(ngram,n1gram,"quite some")))
# print(paste("8:",probabilityWord(ngram,n1gram,"his little")))
# print(paste("9:",probabilityWord(ngram,n1gram,"during the")))
# print(paste("10:",probabilityWord(ngram,n1gram,"must be")))


print(paste("1:",probabilityWord(ngram,n1gram,"a case of")))
print(paste("2:",probabilityWord(ngram,n1gram,"would mean the")))
print(paste("3:",probabilityWord(ngram,n1gram,"make me the")))
print(paste("4:",probabilityWord(ngram,n1gram,"struggling but the")))
print(paste("5:",probabilityWord(ngram,n1gram,"date at the")))
print(paste("6:",probabilityWord(ngram,n1gram,"be on my")))
print(paste("7:",probabilityWord(ngram,n1gram,"in quite some")))
print(paste("8:",probabilityWord(ngram,n1gram,"with his little")))
print(paste("9:",probabilityWord(ngram,n1gram,"faith during the")))
print(paste("10:",probabilityWord(ngram,n1gram,"you must be")))


}




getNWDistrib<-function(clean, n){
  
  if (n==2){
    tdmN <- termFreq(clean, control = list(tokenize = BigramTokenizer, tolower=TRUE))
  } else if (n==3){
    tdmN <- termFreq(clean, control = list(tokenize = TrigramTokenizer, tolower=TRUE))
    
  }
  else if (n==4){
    tdmN <- termFreq(clean, control = list(tokenize = QuadrigramTokenizer, tolower=TRUE))
    
  }
  else if (n==5){
    tdmN <- termFreq(clean, control = list(tokenize = PentagramTokenizer,  tolower=TRUE))
    
  }
  else if (n==1){
    tdmN <- termFreq(clean, control = list(tokenize = UnigramTokenizer,  tolower=TRUE, stopwords=FALSE, wordLengths= c(1,Inf)))
    
  }
  
  
  matrixTerms<-as.matrix(tdmN)
  dt<-data.frame(matrixTerms)
  dt$words<-rownames(dt)
  colnames(dt)<-c("occurrencies","gramWords")
  #hist(dt$occurrencies)
  dt
}


getNWDistribN<-function(clean, n){
  
 
    tdmN <- termFreq(clean, control = list(tokenize = NgramTokenizerAA(clean,n)))
    
  
  
  matrixTerms<-as.matrix(tdmN)
  dt<-data.frame(matrixTerms)
  dt$words<-rownames(dt)
  colnames(dt)<-c("occurrencies","gramWords")
  #hist(dt$occurrencies)
  dt
}






UnigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)


BigramTokenizer <-
 function(x)
 unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

TrigramTokenizer <-
function(x)
unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


QuadrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

PentagramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)




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
  selected<-rbinom(length(totalLines),1,0.1)
  
  fileConn<-file("subsetEnglish10.txt")
  writeLines(totalLines[selected==1], fileConn)
  close(fileConn)
}
