pollutantmean <- function(directory, pollutant, id = 1:332){
    files <- list.files(directory)                          #Get all files in directory
    data_csv <- data.frame()                                #Bind all files data into one variable
    for(i in id){
        #print(i)
        data <- read.csv(paste(directory,files[i],sep="/"))
        data_csv <- rbind(data_csv, data)
    }
    mean(data_csv[,pollutant], na.rm=TRUE)                  #Remove NA's and mean
}