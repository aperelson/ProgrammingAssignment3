rankhospital <- function(state, outcome, num = "best") { 
    
    ## Read outcome data
    outcomeofcare <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    
    ## Check that state and outcome are valid
    if (state %in% unique(outcomeofcare$State) == FALSE) {
        stop("invalid state")
    }
    
    if (is.element(outcome, c('heart attack', 'heart failure', 'pneumonia')) == FALSE) {
        stop("invalid condition")
    }
    
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
    ## Get all the hospitals in the state:
    hospinstate <- outcomeofcare[outcomeofcare$State==state,]
    
    ## Choose which criteria to get the lowest of:
    if (outcome == 'heart attack') {
        sortingcolumn <- 11
    }
    else if (outcome == 'heart failure') {
        sortingcolumn <- 17
    }
    else if (outcome == 'pneumonia') {
        sortingcolumn <- 23
    }
    
    namecolumn <- grep("Hospital.Name", colnames(hospinstate))
    
    #Convert column to numbers for sorting and suppress warnings:
    hospinstate[sortingcolumn] = 
        suppressWarnings(as.numeric(hospinstate[sortingcolumn][hospinstate[sortingcolumn] != 'No']))
    
    #Order by both the condition and the hospital name:    
    topone <- hospinstate[order(hospinstate[sortingcolumn],hospinstate[namecolumn]),]
    topone$Hospital.Name[num]
            
}