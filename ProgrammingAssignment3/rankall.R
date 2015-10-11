rankall <- function(outcome, num = "best") {
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data<-data[c(2,7,11,17,23)]
  validoutcome<-c("heart attack","heart failure","pneumonia")
  if(!outcome %in% validoutcome) stop("Invalid Outcome")
  outcome<-gsub(" ",".",outcome)
  states<-unique(data$State)
  states<-states[order(states)]
  sorteddata<-c("hospital"=character(),"state"=character())
  for(st in states) {
    newdata<-data[data$State==st & data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)] !="Not Available",]
    newdata[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)] <- as.data.frame(sapply(newdata[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)], as.numeric))
    newdata<-newdata[order(newdata$Hospital.Name,decreasing=FALSE),]
    newdata<-newdata[order(newdata[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)]),]
    if(num=="best") {
      num<-1
    } else if(num=="worst") {
      num<-nrow(newdata)
    }
    sorteddata<-rbind(sorteddata,c(newdata[num,]$Hospital.Name,st))
  }
  as.data.frame(sorteddata)
  colnames(sorteddata)<-c("hospital","state")
  as.data.frame(sorteddata)
}
