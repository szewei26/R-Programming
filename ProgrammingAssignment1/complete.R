complete <- function(directory, id=1:332) {
  obscase <- data.frame()
  filenames <- list.files(directory,pattern=".csv",full.names=TRUE)
  for(i in id) {
    mydata <- read.csv(filenames[i])
    count=NROW(na.omit(mydata))
    obscase <- rbind(obscase,c(i,count))
  }
  names(obscase) <- c("id","nobs")
  obscase
}