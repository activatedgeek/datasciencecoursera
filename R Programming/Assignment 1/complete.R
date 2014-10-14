complete <- function(directory, id = 1:332) {
    files <- list.files(directory)
    data_csv <- data.frame(id = numeric(0), nobs = numeric(0))
    row <- 1
    for(i in id){
        data <- read.csv(paste(directory,files[i],sep="/"))
        data_csv[row,] <- c(i, nrow(data[complete.cases(data),]))
        row <- row + 1
    }
    data_csv
}