rankall <- function(outcome, num = "best") {

  oc <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  oc <-oc[order(oc$State),]

  ## assign the index to look at the outcome and sort according to outcome
  li_outcome <- c("heart attack", "heart failure", "pneumonia")
  if (outcome %in% li_outcome){
    if(outcome=="heart attack") ind <-11    
    if (outcome=="heart failure") ind <-17
    if (outcome=="pneumonia") ind <-23
  }else {
    stop("Invalid outcome")
  }
  
  ## determine the ranking of the outcome if its worst or best and assign the ranking number
  li_rank <-c("best","worst")
  if (num %in% li_rank){
    if(num %in% "best") num <-1
    if(num %in% "worst") num <-0
  }

  vHos <- vector()                          #initialise an empty vector to house hospital
  vhos2 <- vector()                         #to put in the hospital as we traverse through the list of states
  
  st <- unique(oc$State)                    #to retrieve the list of states to go through

  for (i in 1:length(st)){
    sub<-subset(oc,State==st[i])            #to get a filtered list based on state
    sort <- sub[order(as.numeric(sub[,ind]),sub[,2]),] #sort according to the outcome,hospital
    if(num==0){                             #if the ranking is worst case, then get the max value
      worstcase <-sub[which.max(as.numeric(sub[,ind])),] 
      vhos2 <-worstcase[1,2]
    }else{
      vhos2 <-sort[num,2]
    }
    vHos <-c(vHos,vhos2)
  }
  #pack the vectors into the data.frame object and give the column name
  df <- data.frame(vHos,st)
  colnames(df) <-c("hospital","state")
  return(df)

}

