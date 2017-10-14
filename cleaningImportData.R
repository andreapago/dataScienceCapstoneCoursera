
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
selected<-rbinom(.05*length(totalLines),length(totalLines),.5)

fileConn<-file("subsetEnglish.txt")
writeLines(totalLines[selected], fileConn)
close(fileConn)
}


contw<-file("../final/en_US/en_US.twitter.txt", "r") 
conbl<-file("../final/en_US/en_US.blogs.txt", "r") 
connw<-file("../final/en_US/en_US.news.txt", "r") 

linesTW<-readLines(contw)
linesBl<-readLines(conbl)
linesNw<-readLines(connw)

maxBl<-max(sapply(as.list(linesBl),nchar))
maxTW<-max(sapply(as.list(linesTW),nchar))
maxNw<-max(sapply(as.list(linesNw),nchar))

lsum<-sum(as.numeric(sapply("love",grepl,as.list(linesTW))))
hsum<-sum(as.numeric(sapply("hate",grepl,as.list(linesTW))))


sapply("biostats",grep,as.list(linesTW))



sum(as.numeric(sapply("A computer once beat me at chess, but it was no match for me at kickboxing",grepl,as.list(linesTW))))
