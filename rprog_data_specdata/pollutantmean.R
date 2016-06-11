pollutantmean <- function(directory, pollutant, id = 1:332) {
  myid <- id
  for(i in 1:length(id)) {
    if(id[i]<10) {
      myid[i] <- paste("00",id[i], sep="")
    }else if(id[i]<100) {
      myid[i] <- paste("0",id[i], sep="")
    }
  }

  # http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
  mydata = do.call(rbind, lapply(paste(getwd(), "/", directory, "/", myid, sep="", ".csv"), function(x) read.csv(x, stringsAsFactors = FALSE)))
  print(mean(mydata[[pollutant]], na.rm = TRUE), digits = 4)
}