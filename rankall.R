rankall <- function(outcome, num = "best") { 
    
    ## Read outcome data
    outcomeofcare <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
	
	##Set up return data frame:
	returnDF <- data.frame(hospital = character(0), state = character(0))
    
    ## Check that outcome is valid
    if (is.element(outcome, c('heart attack', 'heart failure', 'pneumonia')) == FALSE) {
        stop("invalid condition")
    }
    
    ## For each state, find the hospital of the given rank
	for (i in 1:nrow(unique(outcomeofcare$State))) {
		rowState <- outcomeofcare$State[i,]
		
		## Get all the hospitals in the state:
		hospinstate <- outcomeofcare[outcomeofcare$State==rowState$State,]
		
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
			selectedHospName <- mainone$Hospital.Name
		}
		else if (num == 'worst') {
			mainone <- tail(hospinstate, 1)        
			selectedHospName <- mainone$Hospital.Name
		}
		else if ((is.numeric(num)) & (num >= 1) & (num <= nrow(hospinstate))) {
			mainone <- hospinstate[num,]
			selectedHospName <- mainone$Hospital.Name
		}
		else {
			selectedHospName <- NA
		}
			
		#Found the record, now add it to the data frame to be returned:
		newrow = c(selectedHospName, rowState)
		returnDF <- rbind(returnDF, newrow)		
	}
    
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    returnDF
}
