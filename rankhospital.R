rankhospital <- function(state, outcome, num = "best"){
  hosp <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  st <- hosp$State
  oc <- c("heart attack", "heart failure", "pneumonia")
  print(oc)
  res <-"no outcome"
  worstcase <- 0
  
  if (state %in% st == FALSE) stop ("Invalid State")
  else if (outcome %in% oc == FALSE) stop ("invalid Outcome")
  else {
        sub <- subset(hosp,State==state)
        print(nrow(sub))
        if(nrow(sub)>0){
              if (outcome=="heart attack") {
                sort <- sub[order(as.numeric(sub[,11]),sub[,2]),]
                worstcase <-sub[which.max(as.numeric(sub[,11])),] 
                
              }
              if (outcome=="heart failure") {
                sort <- sub[order(as.numeric(sub[,17]),sub[,2]),]
                worstcase <-sub[which.max(as.numeric(sub[,17])),] 
                ##res <- sort[num,2]          
              }
              if (outcome=="pneumonia") {
                sort <- sub[order(as.numeric(sub[,23]),sub[,2]),]
                worstcase <-sub[which.max(as.numeric(sub[,23])),] 
                ##res <- sort[num,2]
              }
        }
        if(num=="best"){
          res <-sort[1,2]
          
        }else if(num=="worst"){
          res <-worstcase[1,2]
        }else{
          res <- sort[num,2]
        }
        return(res)
    }
}