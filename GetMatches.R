# This function takes a list of lists, corresponding to possible matches for
# each delegate. It returns a dataframe with delegate names as columns
# with each row corresponding to a different match. The lengths of the columns 
# will be different, corresponding to different numbers of suitable matches per 
# delegate. 
# 
# It takes the arguments of L (the list generated from the matrix matching) and 
# Data, which is the original dataset. Data should have the first four columns of:
# names(Data)[c(4:6, 8)] <- [1] "Title"      "First.Name" "Surname"    "Company"  

GetMatches <- function(L, Data){
        
        # Put list into a dataframe
        CompaniesToMeet <- data.frame(matrix(0, nrow = length(L), ncol = length(L)))
        for(i in 1:length(L)){
                for(j in 1:length(L[[i]])){
                        CompaniesToMeet[j,i] <- L[[i]][j]
                }
        }
        
        # Trim off the blank rows
        CompaniesToMeet <- CompaniesToMeet[rowSums(CompaniesToMeet) > 0,]
        
        # Info on the Delegates
        DelegatesToMeet <- data.frame(matrix(as.character(''), nrow = nrow(CompaniesToMeet),
                                             ncol = ncol(CompaniesToMeet)), stringsAsFactors = F)
        
        names(DelegatesToMeet) <- 1:ncol(CompaniesToMeet)
        Delegates <- Data[,c(4:6, 8)]
        # Put the Delegates and affilations into a character vector
        DelegatesList <- character()
        for(i in 1:nrow(Delegates)){
                DelegatesList[i] <- paste(Delegates[i,2], #removed title
                                          Delegates[i,3],
                                          ',',
                                          Delegates[i,4])
                DelegatesList[i] <- gsub(' , ', ', ', DelegatesList[i])
        }
        
        # Substitutes column numbers with delegate names
        for(i in 1:length(names(DelegatesToMeet))){
                names(DelegatesToMeet)[i] <- paste(Delegates[i,2], #removed title
                                                   Delegates[i,3])
        }
        
        # Grab the names of deligate targets and fill into dataframe
        for(i in 1:ncol(CompaniesToMeet)){
                for(j in 1:length(CompaniesToMeet[,i])){
                        if(CompaniesToMeet[j,i] > 0){
                                Person <- CompaniesToMeet[j,i]
                                DelegatesToMeet[j,i] <- DelegatesList[Person]
                        }
                }
        }
        return(DelegatesToMeet)
}