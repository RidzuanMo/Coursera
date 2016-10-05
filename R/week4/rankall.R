rankall <- function(outcome, num = "best") {
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
    
    ##
    ## For each state, find the hospital of the given rank
    ##
    
    hospitals <- na.omit(subset(raw_data, select=c("State","Hospital.Name", type_of_outcome[outcome])))
    names(hospitals) <- c("State", "Hospital.Name", "Rate")
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # Split data.frame based on State
    states <- split(hospitals, hospitals$State)
    
    # initialize data.frame to hold output
    allHospitalByRanking <- data.frame()
    
    # Loop through all States in dataset
    for(i in 1:length(states)) {
        hospitals_in_state <- states[[i]]
        
        # Sort according to Rate
        hospitals_rank <- hospitals_in_state[order(as.numeric(hospitals_in_state$Rate), 
                                                   hospitals_in_state$Hospital.Name), ]
        
        if(num=="best") { 
            ranking <- 1 
        }else if(num=="worst") { 
            ranking <- nrow(hospitals_rank) 
        }else {
            ranking <- num
        }
        
        # Row bind the finding to output data.frame
        allHospitalByRanking <- rbind(allHospitalByRanking, 
                                      data.frame(
                                          Hospital = hospitals_rank[ranking,"Hospital.Name"],
                                          State = hospitals_in_state[1,"State"])
        )
    }
    
    allHospitalByRanking
}