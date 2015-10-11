rankhospital <- function(state, outcome, num = "best") {
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data<-data[c(2,7,11,17,23)]
  validstate<-unique(data[,2]) 
  if(!state %in% validstate) stop("Invalid State")
  validoutcome<-c("heart attack","heart failure","pneumonia")
  if(!outcome %in% validoutcome) stop("Invalid Outcome")
  outcome<-gsub(" ",".",outcome)
  data<-data[data$State==state & data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)] !="Not Available",]
  data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)] <- as.data.frame(sapply(data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)], as.numeric))
  data<-data[order(data$Hospital.Name,decreasing=FALSE),]
  data<-data[order(data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)]),]
  if(num=="best") {
    num<-1
  } else if(num=="worst") {
    num<-nrow(data)
  } 
  data[num,]$Hospital.Name
}
