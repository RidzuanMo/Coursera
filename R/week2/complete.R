complete <- function(directory, id=1:332) {
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