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
    ## And convert column to numbers for sorting and suppress warnings:
    if (outcome == 'heart attack') {
        sortingcolumn <- which(colnames(hospinstate)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
        
        hospinstate[sortingcolumn] = 
            suppressWarnings(as.numeric(hospinstate[sortingcolumn][hospinstate[sortingcolumn] != 'No']))
    }
    else if (outcome == 'heart failure') {
        sortingcolumn <- which(colnames(hospinstate)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
        
        hospinstate[sortingcolumn] = 
            suppressWarnings(as.numeric(hospinstate[sortingcolumn][hospinstate[sortingcolumn] != 'No']))
    }
    else if (outcome == 'pneumonia') {
        sortingcolumn <- which(colnames(hospinstate)=="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        
        hospinstate[sortingcolumn] = 
            suppressWarnings(as.numeric(hospinstate[sortingcolumn][hospinstate[sortingcolumn] != 'No']))
    }
    
    namecolumn <- which(colnames(hospinstate)=="Hospital.Name")

    ## Order the data frame: 
    hospinstate <- hospinstate[order(hospinstate[sortingcolumn], hospinstate[namecolumn]),]
    
    ## Extract only the not NA rows:
    hospinstate <- hospinstate[!is.na(hospinstate[sortingcolumn]),]
    
    ## Determine which record to fetch:
    if (num == 'best') {
        mainone <- head(hospinstate, 1)        
    }
    else if (num == 'worst') {
        mainone <- tail(hospinstate, 1)        
    }
    else if ((is.numeric(num)) & (num >= 1) & (num <= nrow(hospinstate))) {
        mainone <- hospinstate[num,]
    }
    else {
        return(NA)
    }
        
    mainone$Hospital.Name
}
