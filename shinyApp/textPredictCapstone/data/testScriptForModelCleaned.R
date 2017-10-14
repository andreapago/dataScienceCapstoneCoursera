library(RWeka)
library(tm)
library(parallel)
library(data.table)
library(stringr)




initExperiment<-function(){

input<-file("subsetEnglish0125.txt")
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

rm(DT,DT2,textClean,test, DT3, nMinTwo, nMinOne,unigram, clean)

gc()


message("separating last word from extracted grams ")

DTall<-splitNgramSubset(DTall,DTallNMinOne,cl)
DTtwo<-splitNgramSubset(DTallNMinOne,DTallNMinTwo,cl)
DTthree<-splitNgramSubset(DTallNMinTwo,DTAllUnigram,cl)

rm(DTallNMinOne, DTallNMinTwo, DTAllUnigram)
gc()
stopCluster(cl)


message("computing probabilities")

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


return(list(DTall,DTtwo,DTthree))

}





makeGramProbMatrixGeneric<-function(textClean, gramYouWant){
  
  message("starting computation of Grams...")
  
  cpus<-4
  
  cl<-makeCluster(cpus)
  
  clusterEvalQ(cl, c(library(tm)))
  
  clusterExport(cl, c("UnigramTokenizer","BigramTokenizer","TrigramTokenizer","QuadrigramTokenizer", "PentagramTokenizer", "EsagramTokenizer"))
  
  
  clean<-textClean
  #test<-parLapply(cl, clean, getNWDistrib, 2)
  test<-parLapply(cl, clean, getNWDistrib, gramYouWant)
  nMinOne<-parLapply(cl, clean, getNWDistrib,gramYouWant-1)
  
  
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
  
  
  
  rm(DT,DT2,textClean,test, nMinOne, clean)
  
  gc()
  
  
  message("separating last word from extracted grams ")
  
  DTall<-splitNgramSubset(DTall,DTallNMinOne,cl)
  
  rm(DTallNMinOne)
  gc()
  stopCluster(cl)
  
  
  message("computing probabilities")
  
  gamma = 0.5
  DTall[,prob:=i.occurrencies/occurrencies]
  
  DTall[,occMinGamma:=i.occurrencies-gamma]
  
  DTall[,probObsCorr:=occMinGamma/occurrencies]
  
  
  
  return(DTall)
  
}















#listGrams 1:6grams 2:5grmas 3:4grams 4:3grams 5:2grmas
probabilityWord<-function(listGrmas, wordPreceeding){ #wordPreceeding can be a more words (i.e., gram)
  wordPreceeding<-trimws(tolower(stripWhitespace(removePunctuation(removeNumbers(wordPreceeding)))))
  availableGrams<-dim(listGrmas)
  numSplits<-length(unlist(str_split(wordPreceeding, " ")))
  dime<-0
  active<-6-numSplits #max supported is 6-grams at the moment
  if(active < 1){
    active<-1
    wordPreceeding<-word(wordPreceeding,numSplits-4,numSplits)#in order to feed just the last 5 words at max
  }
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
    selectedWord<-sample(x = merged$second, size=1, replace = T, prob=merged$finalProb)
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



assignmentTestWeek3<-function(listGrams){
# print(paste("1:",probabilityWord(listGrams,"case of")))
# print(paste("2:",probabilityWord(listGrams,"mean the")))
# print(paste("3:",probabilityWord(listGrams,"me the")))
# print(paste("4:",probabilityWord(listGrams,"but the")))
# print(paste("5:",probabilityWord(listGrams,"at the")))
# print(paste("6:",probabilityWord(listGrams,"on my")))
# print(paste("7:",probabilityWord(listGrams,"quite some")))
# print(paste("8:",probabilityWord(listGrams,"his little")))
# print(paste("9:",probabilityWord(listGrams,"during the")))
# print(paste("10:",probabilityWord(listGrams,"must be")))


print(paste("1:",probabilityWord(listGrams,"a case of")))
print(paste("2:",probabilityWord(listGrams,"would mean the")))
print(paste("3:",probabilityWord(listGrams,"make me the")))
print(paste("4:",probabilityWord(listGrams,"struggling but the")))
print(paste("5:",probabilityWord(listGrams,"date at the")))
print(paste("6:",probabilityWord(listGrams,"be on my")))
print(paste("7:",probabilityWord(listGrams,"in quite some")))
print(paste("8:",probabilityWord(listGrams,"with his little")))
print(paste("9:",probabilityWord(listGrams,"faith during the")))
print(paste("10:",probabilityWord(listGrams,"you must be")))


}


assignmentTestWeek4<-function(listGrams){
  print(paste("1:",probabilityWord(listGrams,"live and I'd")))
  print(paste("2:",probabilityWord(listGrams,"me about his")))
  print(paste("3:",probabilityWord(listGrams,"arctic monkeys this")))
  print(paste("4:",probabilityWord(listGrams,"helps reduce your")))
  print(paste("5:",probabilityWord(listGrams,"to take a")))
  print(paste("6:",probabilityWord(listGrams,"to settle the")))
  print(paste("7:",probabilityWord(listGrams,"groceries in each")))
  print(paste("8:",probabilityWord(listGrams,"bottom to the")))
  print(paste("9:",probabilityWord(listGrams,"bruises from playing")))
  print(paste("10:",probabilityWord(listGrams,"of Adam Sandler's")))
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
  else if (n==6){
    tdmN <- termFreq(clean, control = list(tokenize = EsagramTokenizer,  tolower=TRUE, stopwords=FALSE, wordLengths= c(1,Inf)))
    
  }
  
  
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

EsagramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 6), paste, collapse = " "), use.names = FALSE)




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
  selected<-rbinom(length(totalLines),1,0.0125)
  
  fileConn<-file("subsetEnglish0125.txt")
  writeLines(totalLines[selected==1], fileConn)
  close(fileConn)
}
