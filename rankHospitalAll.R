rankall <- function(outcome, num="best") {

## Read outcome data  
  
csvData <- read.csv("outcome-of-care-measures.csv", sep = ",")

## Checks that state and outcome are valid 

 validStates <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI","ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY") 
 validOutcomes <- c("heart attack", "heart failure", "pneumonia") 

if (!is.element(outcome,validOutcomes)) stop("invalid Outcome")


  headerName <- NULL 
 
  if (outcome == "heart attack") headerName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  else if (outcome == "heart failure") headerName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  else headerName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
       
      hospitals <- c() 
      states <- c() 
 
 
      
## For each state, find the hospital of the given rank

      for (state in validStates) { 
             rankedHosp <- c() 
                 data <- csvData[csvData$State == state,]         
                 sortedData <- data[order(as.numeric(as.character(data[,headerName])), as.character(data[,"Hospital.Name"])),] 
                 sortedData <- sortedData[!sortedData[,headerName] == "Not Available",] 
                 if (num == "best") { 
                       rankedHosp <- best(state, outcome) 
                   } else if (num == "worst") { 
                      rankedHosp <- as.character(tail(sortedData[,"Hospital.Name"], n = 1)) 
                   } else { 
                     rankedHosp <- as.character(sortedData[,"Hospital.Name"][num]) 
                   } 
                 hospitals <- c(hospitals, rankedHosp) 
      } 
      
      ## Return a data frame with the hospital names and the 
      ## (abbreviated) state name
      
           result <- data.frame(hospitals, validStates) 
           colnames(result) <- c("hospital", "state") 
           return(result) 
 } 

