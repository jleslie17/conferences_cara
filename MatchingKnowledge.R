# Use this script to match attendees based on knowledge, '3.1'

source('AddUnderscores.R')
source('SpreadResponses.R')
library(lsa)
source('GetMatches.R')
source('CompanyMatchesOutput.R')

path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
FileMS2 <- 'Milestone2utf8.csv'
File130916 <- 'SSL_Reg_13.09.16.csv'
File190916 <- 'Registration19Sept.csv'


Data <- read.csv(paste(datadir, File190916, sep = ''),
                 header = T, na.strings = '')

Data <- Data[rowSums(is.na(Data)) != ncol(Data),]

df <- Data[,21:33]
names(df)

targets <- df[,1:6]
users <- df[,8:13]
names(targets)
names(users)

# Put underscores between words, but not between answers
targets <- AddUnderscores(targets)
#Spread responses
tarSpread <- SpreadResponses(targets)
names(tarSpread)
# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '.*please.*select.*', 'NEED.*INFO',
           'NEED_INFO', '---_please_select_---', 'n/a', '-', '.', 'none',
           'X', 'None', 'N/a', 'No_interest', '%', 'n.a', 'tbc', 'n.a.', 'NA')

#drops <- c('Other', 'na', 'N/A', '---_please_select_---')
tarSpread <- tarSpread[, !names(tarSpread) %in% drops]
colSums(tarSpread)
names(tarSpread)

CombineSimilarColumns <- function(df, a, b){
        # Takes entries from column b and puts them into col a, then removes
        # col b. Use this when two columns have slightly different names but
        # tell the same information.
        df[[a]][df[[b]] == 1] <- 1
        df <- df[, names(df) != b]
        return(df)
}
# Clean up tarSpread
names(tarSpread)[names(tarSpread) == 'wireless_connectivity'] <- 'Wireless_connectivity'




users <- AddUnderscores(users)
usersSpread <- SpreadResponses(users)
usersSpread <- usersSpread[, !names(usersSpread) %in% drops]
# Tidy these due to bad formatting:
names(usersSpread)
usersSpread <- usersSpread[,sort(names(usersSpread))]
tarSpread <- tarSpread[,sort(names(tarSpread))]


names(usersSpread)
a <- 'Robotics_and_AI'
b <- 'Robotics_and_AI_'
table(usersSpread[[a]])
table(usersSpread[[b]])
usersSpread <- CombineSimilarColumns(usersSpread, a, b)
table(usersSpread[[a]])
table(usersSpread[[b]])
names(usersSpread)

# usersSpread$Robotics_and_AI[usersSpread$Robotics_and_AI_ == 1] <- 1
# usersSpread <- usersSpread[,names(usersSpread) != 'Robotics_and_AI_']
#africa <- 'Smart_technology_African_cities'
# names(tarSpread)[34] <- africa
# names(usersSpread)[25] <- africa

a <- 'information_security'
b <- 'inforamtion_security'
table(usersSpread[[a]])
table(usersSpread[[b]])
usersSpread <- CombineSimilarColumns(usersSpread, a, b)
table(usersSpread[[a]])
table(usersSpread[[b]])
names(usersSpread)

a <- 'software_development'
b <- 'Software'
table(usersSpread[[a]])
table(usersSpread[[b]])
usersSpread <- CombineSimilarColumns(usersSpread, a, b)
table(usersSpread[[a]])
table(usersSpread[[b]])
names(usersSpread)


# Now remove the columns in users that do not appear in targets
names(tarSpread)
names(usersSpread)
names(usersSpread)[!names(usersSpread) %in% names(tarSpread)]
usersTidy <- usersSpread[,names(usersSpread) %in% names(tarSpread)]
names(tarSpread)
names(tarSpread)[!names(tarSpread) %in% names(usersTidy)]
tarTidy <- tarSpread[,names(tarSpread) %in% names(usersTidy)]

# Reorder the columns to match
# index <- integer()
# for(name in names(tarTidy)){
#         index <- c(index, which(names(usersTidy) == name))
# }
# 
# names(usersTidy)[index]
# names(tarTidy)
# 
# identical(names(usersTidy)[index], names(tarTidy))
# 
# usersTidy <- usersTidy[, index]

# make empty matrix for for values to be added
for(i in 1:length(names(usersTidy))){
        print(names(usersTidy)[i])
        print(names(tarTidy)[i])
}

m <- matrix(0, nrow = nrow(usersSpread), ncol = nrow(tarSpread))
musers <- as.matrix(usersTidy)
mtargets <- as.matrix(tarTidy)

distanceToTargets <- function(user, targets = mtargets){
        distances <- numeric()
        for(i in 1:nrow(targets)){
                distances[i] <- cosine(user, targets[i,])
        }
        #This makes the outcome binary:
        distances[is.na(distances)] <- 0
        distances[distances > 0] <- 1
        return(distances)
}


for(i in 1:nrow(musers)){
        m[i,] <- distanceToTargets(musers[i,])
}

mSum <- m + t(m)

RankDistanceToTargets <- function(user, targets){
        # Calculates cosine distance between one user profile and each of the
        # targets in L. Returns only 25 best matches for the user. NOT the 
        # 25 best reciprocal fits!
        tempDF <- data.frame(target = numeric(),
                             distance = numeric())
        distances <- numeric()
        for(j in 1:length(targets)){
                tempDF[j,1] <- targets[j]
                tempDF[j,2] <- cosine(musers[user,], mtargets[targets[j],])
        }
        #This gives na the value of 0:
        tempDF$distance[is.na(tempDF$distance)] <- 0
        # Fills in the values up to position 25:
        for(i in 1:25){
                if(is.na(tempDF$distance[i])){
                        tempDF[i,] <- 0
                }
        }
        return(tempDF[order(tempDF$distance, decreasing = T)[1:25],1])
}

L <- list()
for(i in 1:nrow(mSum)){
        x <- which(mSum[i,] > 1, arr.ind = T) #finds recipr matches
        y <- x[x != i] #removes self
        if(length(y) == 0){ #sets to 0 if no data
                y <- 0
                L[[i]] <- y
        } else {
                trimmed <- RankDistanceToTargets(i, y)
                L[[i]] <- trimmed
        }
}

# x <- which(mSum[1468,] > 1, arr.ind = T)
# y <- x[x != 1468]
# targets <- y
# user <- 1468
# 
# test <- y
# for(i in 1:25){
#         if(is.na(test[i])){
#                 test[i] <- 0
#         }
# }
# 
# RankDistanceToTargets[1468, y]

# L is a list of matches
# Here there is a list of matches. Now get that into suitable output
# This puts the list of names of matches column-wise. Each column is
# a delgate, with the rows being the matches. Not a great output.
DelegatesToMeet <- GetMatches(L, Data)

# This puts the data into a four-column dataframe, with the list of matches 
# as one \n-separated string in the fourth column.
KnowledgeMatchesOutput <- GetCompanyMatchesOutput(Data, DelegatesToMeet)

write.xlsx(KnowledgeMatchesOutput, 'KnowledgeMatchesTop25_M3.xlsx', row.names = F)
