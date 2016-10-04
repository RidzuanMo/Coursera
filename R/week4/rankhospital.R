rankhospital <- function(state, outcome, num = "best") {
    ##
    ## define type of accepted outcome
    ##
    
    type_of_outcome <- c( "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                          "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                          "pneumonia"= "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
    
    ##
    ## Read outcome data
    ##
    
    raw_data <- read.csv(file.path("data","outcome-of-care-measures.csv"), na.string = "Not Available", colClasses = "character")
    
    ##
    ## Check that state and outcome are valid
    ##
    
    if(is.na(type_of_outcome[outcome])){
        ## Stop if invalid outcome
        stop("invalid outcome")
    }
    
    ## get subset of data based on State and omit incomplete cases
    hospital <- na.omit(subset(raw_data, State==state, select=c("Hospital.Name", type_of_outcome[outcome])))
    
    ## rename header
    names(hospital) <- c("Hospital.Name", "Rate")
    
    if(nrow(hospital)==0){
        ## Stop if state does not exist
        stop("Invalid State")
    }
    
    ##
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ##
    
    # Sort hospital base on Rate
    ranking <- hospital[order( as.numeric(hospital$Rate), hospital$Hospital.Name), ]
    
    # Assign rank to sorted data frame
    ranking$rank <- 1:nrow(ranking)
    
    if(num=="best") {
        ## Return the best hospital
        return(ranking[1, "Hospital.Name"])
    }
    
    if(num=="worst") {
        ## Return the worst hospital
        return(ranking[nrow(ranking), "Hospital.Name"])
    }
    
    if(num > nrow(ranking)) {
        ## Return the NA if the rank is not valid
        return(NA)
    }
    
    ## Return hospital based on rank
    ranking[ranking$rank == num ,"Hospital.Name"]
}