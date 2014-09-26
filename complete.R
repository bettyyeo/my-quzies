complete <- function(directory="specdata", id = 1:332) {
  v2<-vector()
  v1<-vector()
  for (i in id){
      f1 <- sprintf("%03d.csv", i)  
      mydata <-read.csv(paste(directory,f1,sep="/"))
      sub<-subset(mydata,!is.na(sulfate)&!is.na(nitrate))
      noOfrec = nrow(sub)
        
      id1<-sub["ID"]
      v1<-(c(v1,id1[1,1]))
      
      v2<-c(v2,noOfrec)
      
  }
  return(data.frame(id=v1,nobs=v2))
  
  return
}