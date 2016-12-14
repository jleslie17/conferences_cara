library(xlsx)

GetCompanyMatchesOutput <- function(Data, DelegatesToMeet){
        Delegates <- Data[,c(5,6,9)] #FirstName, Surname, email
        CompanyMatchesOutput <- data.frame(matrix(ncol = 4, nrow = nrow(Delegates)))
        names(CompanyMatchesOutput) <- c('FirstName', 'Surname', 'email', 'Matches')
        CompanyMatchesOutput$FirstName <- Delegates$First.Name
        CompanyMatchesOutput$Surname <- Delegates$Surname
        CompanyMatchesOutput$email <- Delegates$Company.Email
        
        for(i in 1:nrow(CompanyMatchesOutput)){
                Matches <- DelegatesToMeet[,i]
                Matches <- Matches[Matches != '']
                if(length(Matches) != 0){
                        CompanyMatchesOutput$Matches[i] <- paste(Matches, collapse = '\n')
                }
        }
        return(CompanyMatchesOutput)
}
