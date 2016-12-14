# This is a function to put underscores between words in all
# variables. The second line puts a space back in the case of 
# preceeding comma.

AddUnderscores <- function(x){
        for(i in names(x)){
                x[,i] <- gsub(" ", "_", x[,i])
                x[,i] <- gsub(",_", ", ", x[,i])
        }
        return(x)
}

