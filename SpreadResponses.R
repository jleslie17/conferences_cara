require(plyr)
# Function to make a vector of possible answers for each variable
getAllAnswers <- function(x){
        allAnswers <- unique(c(
                unlist(strsplit(as.character(x), ", "))))
        return(allAnswers)
}

getAnswerLevels <- function(df){
        choices <- character()
        for(item in 1:length(names(df))){
                choices <- c(choices, getAllAnswers(df[,item]))
        }
        return(unique(choices))
}

SpreadResponses <- function(df){
        answerLevels <- getAnswerLevels(df)
        df$id <- 1:nrow(df)
        DF <- ddply(df, .(id), function(x)
                table(factor(unlist(strsplit(paste(x, sep = ', '), 
                                             ", ")), levels = answerLevels)))
        for(i in 1:nrow(DF)){
                for(j in 2:ncol(DF)){
                        if(DF[i,j] >1){
                                DF[i,j] = 1
                        }}}
        DF <- DF[,-1]
        return(DF)
}

#test <- SpreadResponses(DF2)
#identical(test, DF)
