rankall <- function(outcome, num = "best"){
    col <- c(11,17,23)
    names(col) <- c("heart attack", "heart failure", "pneumonia")
    # Error handling
    if(!(outcome %in% names(col)))
        stop("invalid outcome", call. = T)
    data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
    data[,col[outcome]] <- as.numeric(levels(data[,col[outcome]]))[data[,col[outcome]]]
    
    hospital <- c()
    if(identical(num,"best")){
        num = 1
        
        for(level in levels(data[,7])){
            trimmed <- data[data[,7] == level,][,]
            trimmed <- trimmed[order(trimmed[,col[outcome]], trimmed[,2]),]
            hospital <- c(hospital, as.character(trimmed[num,2]))
        }
    }
    else if(identical(num,"worst")){
        num = 1
        
        for(level in levels(data[,7])){
            trimmed <- data[data[,7] == level,][,]
            trimmed <- trimmed[order(-trimmed[,col[outcome]], trimmed[,2]),]
            hospital <- c(hospital, as.character(trimmed[num,2]))
        }
    }
    else{
        for(level in levels(data[,7])){
            trimmed <- data[data[,7] == level,][,]
            trimmed <- trimmed[order(trimmed[,col[outcome]], trimmed[,2]),]
            hospital <- c(hospital, as.character(trimmed[num,2]))
        }
    }
    state <- levels(data[,7])
    data.frame(hospital,state)
}