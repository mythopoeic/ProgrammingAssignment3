## Function of three arguments to return a vector of the Hospital with the 
## specified ranking

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        importdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        if(!state %in% importdata[,7]) {
                stop("invalid state")   
        } 
        
        if(!outcome=="heart attack" && !outcome=="heart failure" && !outcome=="pneumonia"  ) {
                stop("invalid outcome")   
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        importdata <- importdata[importdata[,7]==state,]
        
        if(outcome=="heart attack"){
                importdata[,11] <- suppressWarnings(as.numeric(importdata[,11]))
                x<- importdata[order(importdata[,11], importdata[,2], na.last = NA),]

        } else if(outcome=="heart failure"){
                importdata[,17] <- suppressWarnings(as.numeric(importdata[,17]))
                x<- importdata[order(importdata[,17], importdata[,2], na.last = NA),]

        } else if(outcome=="pneumonia"){
                importdata[,23] <- suppressWarnings(as.numeric(importdata[,23]))
                x<- importdata[order(importdata[,23], importdata[,2], na.last = NA),]

        }
        
        if(num =="best"){
                num<-1
                x[num,2]
        }else if (num =="worst"){
                num <- nrow(x)
                x[num,2]
        }else if (num > nrow(x)){
                return(NA)
        } else{
                x[num,2]
        }
}