best <- function(state, outcome){
  res<-"Invalid outcome"
  oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  st <- oc$State
  if(state %in% st == FALSE){
    stop("Invalid State")
  }
  else{
    sub <- subset(oc,State==state)
    if(nrow(sub)>0){
          if (outcome=="heart attack") {
            sort <- sub[order(sub[,11],sub[,2]),]
            res <- sort[1,2]
          }
          if (outcome=="heart failure") {
            sort <- sub[order(sub[,17],sub[,2]),]
            res <- sort[1,2]
          }
          if (outcome=="pneumonia") {
            sort <- sub[order(sub[,23],sub[,2]),]
            res <- sort[1,2]
          }
    }
  return(res)
  }
}