best <- function(state, outcome) {
    ##
    ## define type of accepted outcome
    ##
    
    type_of_outcome <- c( "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                          "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
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
    
    names(hospital) <- c("Hospital.Name", "outcome")
    
    if(nrow(hospital)==0){
        ## Stop if state does not exist
        stop("Invalid State")
    }
    
    ##
    ## Return hospital name in that state with lowest
    ## 30-days death rate
    ##

    ## Ordered complete case data
    ordered_data <- hospital[order(as.numeric(hospital$outcome), hospital$Hospital.Name), ]
    
    ## return the top most data
    ordered_data[1, "Hospital.Name"]
}