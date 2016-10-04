best <- function(state, outcome) {
    ## define type of accepted outcome
    type_of_outcome <- c( "heart attack" = "Heart.Attack", "heart failure"="Heart.Failure", "pneumonia"= "Pneumonia")
    
    ## Read outcome data
    raw_data <- read.csv(file.path("data","outcome-of-care-measures.csv"), na.string = "Not Available", colClasses = "character")
    
    ## Check that state and outcome are valid
    state_data <- subset(raw_data, State==state, 
                         select=c(Hospital.Name,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    names(state_data) <- c("Hospital.Name", "Heart.Attack", "Heart.Failure","Pneumonia")
    
    if(nrow(state_data)==0){
        stop("Invalid State")
    }
    
    if(is.na(type_of_outcome[outcome])){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest
    ## 30-days death rate
    
    ordered_data = NULL
    
    ## Selected complete case for each outcome
    complete_case_data <- state_data[!is.na(state_data[type_of_outcome[outcome]]), ]

    ## Ordered complete case data
    ordered_data <- complete_case_data[order(as.numeric(complete_case_data[[type_of_outcome[outcome]]]), 
                                             complete_case_data["Hospital.Name"]), ]
    
    ordered_data[1, "Hospital.Name"]
}