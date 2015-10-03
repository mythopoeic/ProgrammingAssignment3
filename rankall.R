## Function with two arguments that returns a data frame of all states
## and the hospital in each state with the specified ranking

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        importdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        
        if(!outcome=="heart attack" && !outcome=="heart failure" && !outcome=="pneumonia"  ) {
                stop("invalid outcome")   
        }
        
        ## For each state, find the hospital of the given rank
        
        states <- unique(importdata[,7])
        states <- sort(states)
        output <- vector()
        if (num=="best" | num=="worst"){
                pnum<-1
        }else {
                pnum<-num
        }
        
        if(outcome=="heart attack"){
                sortcol<-11
        } else if(outcome=="heart failure"){
                sortcol<-17
        } else if(outcome=="pneumonia"){
                sortcol<-23
        }
        importdata[,sortcol] <- suppressWarnings(as.numeric(importdata[,sortcol]))
        
        for (i in 1:length(states)){
                statedata <- importdata[grep(states[i], importdata[,7]),]
                
                if(num=="worst"){
                        orderdata <- statedata[order(statedata[,sortcol],statedata[,2],decreasing = TRUE),]
                }else{
                        orderdata <- statedata[order(statedata[,sortcol],statedata[,2]),]   
                }
                
                output <- append (output, as.character(orderdata[pnum,2]))
                output <- append (output, as.character(states[i]))
                
        }
        
        output <- as.data.frame(matrix(output,length(states),2, byrow = TRUE))
        colnames(output) <- c("hospital", "state")
        rownames(output) <- states
        return(output)
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}