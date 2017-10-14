
source("testScriptForModelCleaned.R")
# 
# conInput<-file("subsetEnglish0025TestSet.txt", "r") 
# totalLines<-readLines(conInput) 
# close(conInput)
# 
# dataDir<-"~/Documents/coursera/Course10-Capstone/Rproject/grams/grams005/"
# 
# 
# sixGrams<-readRDS(paste0(dataDir,"sixGram.RDS"))
# fiveGrams<-readRDS(paste0(dataDir,"fiveGram.RDS"))
# fourGrams<-readRDS(paste0(dataDir,"fourGram.RDS"))
# threeGrams<-readRDS(paste0(dataDir,"threeGram.RDS"))
# twoGrams<-readRDS(paste0(dataDir,"twoGram.RDS"))




set.seed(22)


getRightWrong<-function(line){
  
  cleanedLine<-trimws(tolower(stripWhitespace(removePunctuation(removeNumbers(line)))))
  numWords<-length(unlist(str_split(cleanedLine, " ")))
  numWords
  begin<-sample(1:(numWords-1),1)
  possibleSubLine<-numWords-begin-1
  end<-sample(0:possibleSubLine,1)
  toPredict<-word(cleanedLine,begin,end+begin)  
  selectedWord<-probabilityWord(list(sixGrams,fiveGrams,fourGrams,threeGrams,twoGrams),toPredict)
  trueWord<-word(cleanedLine, begin+end+1)
  if(is.na(trueWord)){
    return(NA)
  }
  if(selectedWord==trueWord){
    1
  } else{
    0
  }
  
  
}


goodBad<-sapply(totalLines, getRightWrong,USE.NAMES = F)

good<-sum(goodBad,na.rm = T)

nas<-sum(is.na(goodBad))

overallAccuracy<-good/length(goodBad)
print(overallAccuracy)

correctedAcc<-good/(length(goodBad)-nas)
print(correctedAcc)
