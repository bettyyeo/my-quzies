pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  files<-dir(directory)
  V3 <- vector() ## container of values of the pollutant
  
  for (i in id){
      filedir <- paste(directory, sep="/",files[i])
      myData <- read.csv(filedir)
      V1 <- myData[,pollutant]
      V2 <- V1[!is.na(V1)]
      
      ##collect all the not null pollutant in a vector
      V3 <- c(V3,V2)
      ##print (paste(files[i], " : " , length(V3)))
      
  }
  return(mean(V3))
}

