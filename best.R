## Function to find the best hospital in a state in terms of
## lowest 30-day mortality rate for the specified outcome

best <- function(state, outcome) {
        ## Read outcome data
        
        importdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        if(!state %in% importdata[,7]) {
             stop("invalid state")   
        } 
        
        if(!outcome=="heart attack" && !outcome=="heart failure" && !outcome=="pneumonia"  ) {
                stop("invalid outcome")   
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        importdata <- importdata[importdata[,7]==state,]
        
        if(outcome=="heart attack"){
                importdata[,11] <- suppressWarnings(as.numeric(importdata[,11]))
                x <- importdata[importdata[,11] == min(importdata[,11],na.rm=TRUE),2]
                bad <- is.na(x)
                y <- sort(x[!bad], decreasing = FALSE)
                y[1]
        } else if(outcome=="heart failure"){
                importdata[,17] <- suppressWarnings(as.numeric(importdata[,17]))
                x <- importdata[importdata[,17] == min(importdata[,17],na.rm=TRUE),2]
                bad <- is.na(x)
                y <- sort(x[!bad], decreasing = FALSE)
                y[1]
        } else if(outcome=="pneumonia"){
                importdata[,23] <- suppressWarnings(as.numeric(importdata[,23]))
                x <- importdata[importdata[,23] == min(importdata[,23],na.rm=TRUE),2]
                bad <- is.na(x)
                y <- sort(x[!bad], decreasing = FALSE)
                y[1]
        }
}