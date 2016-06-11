complete <- function(directory, id = 1:332) {
  myid <- id
  for(i in 1:length(id)) {
    if(id[i]<10) {
      myid[i] <- paste("00",id[i], sep="")
    }else if(id[i]<100) {
      myid[i] <- paste("0",id[i], sep="")
    }
  }
  myoutput <- matrix(data=NA, nrow = length(id), ncol = 2)
  dimnames(myoutput) <- list(1:length(id), c("id","nobs"))
  j = 0
  for (i in myid) {
    mydata <- read.csv(paste("./", directory, "/", i, ".csv", sep = ""))
    cleandata <- na.omit(mydata)
      j = j + 1
      myoutput[j, 1] <- as.numeric(i)
      myoutput[j, 2] <- nrow(cleandata)    
  }
  print(myoutput)
}