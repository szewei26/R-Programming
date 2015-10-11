pollutantmean <- function(directory, pollutant, id = 1:332) {
  mydata <- data.frame()
  filenames <- list.files(directory,pattern=".csv",full.names=TRUE)
  for(i in id) mydata <- rbind(mydata,read.csv(filenames[i]))
  mean(mydata[[pollutant]],na.rm=TRUE)
}