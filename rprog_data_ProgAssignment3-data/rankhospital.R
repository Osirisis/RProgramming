rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
    mycol <- outcomes[outcome]
    
    
    ## Check that state and outcome are valid
    if (!state %in% outcomeFile[,7]) {
      stop("invalid state")
    }
    
    if (!outcome %in% names(outcomes)) {
      stop("invalid outcome")
    }
    
    ## Convert numeric columns to numbers
    outcomeFile[,mycol] <- as.numeric(outcomeFile[,mycol])
    
    ## Return hospital name in that state with lowest 30-day death
    outcomeFile <- outcomeFile[order(outcomeFile$State, outcomeFile[mycol], outcomeFile$Hospital.Name), ]
    outcomeFile <- subset(outcomeFile, select = c(State, mycol, Hospital.Name))
    outcomeFile <- outcomeFile[outcomeFile$State == state, ]
    
    ## Remove all NA rows
    outcomeFile <- outcomeFile[complete.cases(outcomeFile),]
    
    ## rate
    if (num == "best") {
      outcomeFile <- outcomeFile[1,]  
    }
    else if (num == "worst") {
      outcomeFile <- tail(outcomeFile, 1)  
    }
    else {
      outcomeFile <- outcomeFile[num,]
    }
    print(outcomeFile$Hospital.Name)
}