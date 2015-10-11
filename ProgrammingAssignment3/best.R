best <- function(state, outcome) {
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data<-data[c(2,7,11,17,23)]
  validstate<-unique(data[,2]) 
  if(!state %in% validstate) stop("Invalid State")
  validoutcome<-c("heart attack","heart failure","pneumonia")
  if(!outcome %in% validoutcome) stop("Invalid Outcome")
  outcome<-gsub(" ",".",outcome)
  data<-data[data$State==state & data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)] !="Not Available",]
  lowestrow<-which.min(data[,grep(outcome,names(data),ignore.case=TRUE,value=TRUE)])
  data[lowestrow,]$Hospital.Name
}