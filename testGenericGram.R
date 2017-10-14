

makeGramProbMatrixGeneric<-function(textClean, gramYouWant){
  
  message("starting computation of Grams...")
  
  cpus<-4
  
  cl<-makeCluster(cpus)
  
  clusterEvalQ(cl, c(library(tm)))
  
  clusterExport(cl, c("UnigramTokenizer","BigramTokenizer","TrigramTokenizer","QuadrigramTokenizer", "PentagramTokenizer"))
  
  
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
  
  rm(DTallNMinOne, DTallNMinTwo, DTAllUnigram)
  gc()
  stopCluster(cl)
  
  
  message("computing probabilities")
  
  gamma = 0.5
  DTall[,prob:=i.occurrencies/occurrencies]
  
  DTall[,occMinGamma:=i.occurrencies-gamma]
 
  DTall[,probObsCorr:=occMinGamma/occurrencies]
 
  
  
  return(DTall)
  
}
