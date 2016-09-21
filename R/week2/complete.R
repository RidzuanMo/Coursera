complete <- function(directory, id=1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files.
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    data_frame <- data.frame()
    
    # read datasets
    for (i in id) {
        # read data from csv
        filename <- file.path(".",directory,paste(sprintf("%03d",i),".csv",sep=""))
        data <- read.csv(filename[1])
        
        data_frame <- rbind(data_frame, data.frame(id=i, nobs=sum(complete.cases(data))))
    }
    
    data_frame
}