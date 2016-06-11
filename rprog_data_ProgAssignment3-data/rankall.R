rankall <- function(outcome, num = "best") {
      ## Read outcome data
      outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
      outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
      mycol <- outcomes[outcome]
      
      
      ## Check that state and outcome are valid
      if (!outcome %in% names(outcomes)) {
        stop("invalid outcome")
      }
      
      ## Convert numeric columns to numbers
      outcomeFile[,mycol] <- as.numeric(outcomeFile[,mycol])
      
      ## Return hospital name in that state with lowest 30-day death
      outcomeFile <- outcomeFile[order(outcomeFile$State, outcomeFile[mycol], outcomeFile$Hospital.Name), ]
      outcomeFile <- subset(outcomeFile, select = c(State, mycol, Hospital.Name))
      ##colnames(outcomeFile) <- c("State", "", "Hospital")
      
      ## Remove all NA rows
      outcomeFile <- outcomeFile[complete.cases(outcomeFile),]

      ## Split the data by state
      outcomeFile <- split(outcomeFile, outcomeFile$State)
      
      ## Determine if the data is best, worst, or a rank. Then generate a list of hospitals.
      if (num == "best") {
        mylist <- unlist(lapply(names(outcomeFile), function(x) outcomeFile[[x]][1,3]))
      }
      else if (num == "worst") {
        mylist <- unlist(lapply(names(outcomeFile), function(x) tail(outcomeFile[[x]][,3], 1)))
      }
      else {
        mylist <- unlist(lapply(names(outcomeFile), function(x) outcomeFile[[x]][num,3]))
      }

      ## Cleanup data for required output  
      mylist <- data.frame(mylist)
      colnames(mylist) <- c("hospital")
      mylist$state <- names(outcomeFile)

      ## Fix row names so they can be referenced by state
      rownames(mylist) <- names(outcomeFile)
      
      ## Return data frame
      mylist
}