best <- function(state, outcome) { 
    
    ## Read outcome data
    outcomeofcare <- read.csv("outcome-of-care-measures.csv", 
                              colClasses = "character") 
    
    ## Check that state and outcome are valid
    if (state %in% unique(outcomeofcare$State) == FALSE) {
        stop("Invalid State")
    }

    if (is.element(outcome, c('heart attack', 'heart failure',
                                    'pneumonia')) == FALSE) {
        stop("Invalid Condition")
    }
    
    ## Return hospital name in that state with lowest 30-day death ## rate
    
    ## Get all the hospitals in the state:
    hospinstate <- outcomeofcare[outcomeofcare$State==state,]
    
    ## Choose which criteria to get the lowest of:
    if (outcome == 'heart attack') {
        sortingcolumn <- 
            grep("Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                 colnames(hospinstate))
    }
    else if (outcome == 'heart failure') {
        sortingcolumn <- 
        grep("Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
             colnames(hospinstate))
    }
    else if (outcome == 'pneumonia') {
        sortingcolumn <- 
        grep("Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", 
             colnames(hospinstate))
    }
    
    
    hospinstate[sortingcolumn] = 
        suppressWarnings(as.numeric(hospinstate[sortingcolumn][hospinstate[sortingcolumn] != 'No']))
    
    topone <- head(hospinstate[order(hospinstate[sortingcolumn]),],1)
    
    topone$Hospital.Name
}
