corr <- function(directory, treshold = 1) {
  filenames <- list.files(directory,pattern=".csv",full.names=TRUE)
  corrlist <- c()
  for (i in 1:length(filenames)) {
    compare<-complete(directory,i)
    if (compare[[2]]>treshold) {
      corrdata<-read.csv(filenames[i])
      corrlist<-c(corrlist,cor(corrdata$sulfate,corrdata$nitrate,use="na.or.complete"))
    }
  }
  corrlist
}