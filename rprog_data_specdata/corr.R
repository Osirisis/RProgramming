corr <- function(directory, threshold = 0) {
  #Read list of files
  myfiles <- list.files(paste("./", directory, sep = ""))
  #Initialize output vector of correlations
  mycorr <- c()
  #for each file in list of files
  j = 0
  for (i in myfiles) {
    mydata <- read.csv(paste("./", directory, "/", i, sep = ""))
    cleandata <- na.omit(mydata)
    if (nrow(cleandata) > threshold) {
      j = j + 1
      mycorr[j] <- signif(cor(cleandata$sulfate, cleandata$nitrate), digits = 4)
    }
  }
  mycorr
}