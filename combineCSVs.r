######### Necessary packages ##############
library(plyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(lattice)
library(scales)
###########################################

############# Read in the Data ############
years <- list.files(pattern = ".csv")
incomeRaw <- data.frame()

# lapply
#incomeRaw <- do.call(rbind, lapply(years, read.csv))
## Best way is to use read.table, but I'll have to build a function
loadFile <- function(x) {
        df <- read.table(file = x, header = TRUE, na.strings = "NA", sep = ",", stringsAsFactors = FALSE)
}
incomeRaw <- do.call(rbind, lapply(years, loadFile))
###########################################

############ Have a look ##################
head(incomeRaw)
str(incomeRaw)
incomeRaw[, 3:5] <- sapply(incomeRaw[, 3:5], as.numeric)
colnames(incomeRaw)
summary(incomeRaw$AvgValue)
incomeRaw$Source <- NULL
incomeRaw$Origin <- NULL
###########################################

############ Add in the 'Party' variable ###################################
for (i in 1:nrow(incomeRaw)) {
        if((incomeRaw$chamber[i] == "E") | (incomeRaw$chamber[i] == "J")) {
                incomeRaw$Party[i] = NA
        } else if (grepl(pattern = "R\\-", incomeRaw$Name[i]) == TRUE) {
                incomeRaw$Party[i] = "R"
        } else if (grepl(pattern = "D\\-", incomeRaw$Name[i]) == TRUE) {
                incomeRaw$Party[i] = "D"
        } else if (grepl(pattern = "I\\-", incomeRaw$Name[i]) == TRUE) {
                incomeRaw$Party[i] = "I"
        } else {
                incomeRaw$Party[i] = NA
        }
}
##############################################################################

############ Split off names in 'Name' and add seperate variable for 'State' ###############################
incomeRaw <- data.frame(incomeRaw, colsplit(incomeRaw$Name, pattern = " \\(", names = c("Names", "State")))
head(incomeRaw)

# Next step will be to drop the Party ID and cleanup state remnants 
incomeRaw$State <- sub(pattern = "[D, R, I]\\-", replacement = "", x = incomeRaw$State)
incomeRaw$State <- sub(pattern = ")", replacement = "", x = incomeRaw$State)

# Drop columns Source and Name as we no longer need that data
incomeRaw$Name <- NULL


# Rename Names column back to nicer sounding Name
colnames(incomeRaw)[8] <- "Name"
############################################################################################################

####### Drop the executive and court instances #############################################################
congressIncomes <- subset(incomeRaw, incomeRaw$chamber != "E" & incomeRaw$chamber != "J" & incomeRaw$chamber != "")
head(congressIncomes)
############################################################################################################

############ Melt and recast the data frame by year #############################

# need to figure out the order, but this is the general form for recasting
congressIncomes$MinValue <- NULL
congressIncomes$MaxValue <- NULL
md <- melt(congressIncomes, id = c("cid", "year"))
head(md)
## recast by cid and year...makes the cid repeat for the variable column but it
## displays the data by year, which is what we want
newCast <- dcast(md, cid + variable ~ year)
head(newCast)


#################################################################################


############### Summary Plots ###################################################
incomesByYear <- aggregate(AvgValue ~ year, data = congressIncomes, FUN = mean)
ggplot(incomesByYear, aes(y = AvgValue, x = as.factor(year))) + geom_bar(stat = "identity") + 
        labs(title = "Average Congressional Net Worth by Year", x = "Year") + 
        scale_y_continuous(name = "Average Net Worth", labels = comma)
# Let's try to bin them by year and then party
partyByYear <- aggregate(AvgValue ~ year + Party, data = congressIncomes, FUN = mean)
ggplot(partyByYear, aes(y = AvgValue, x = as.factor(year), fill = factor(Party)), color = factor(Party)) +
        labs(title = "Average Individual Congressional Net Worth by Party by Year", x = "Year", fill = "Party") +
        scale_y_continuous(name = "Average Individual Net Worth", labels = comma) + geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_manual(values = c("blue", "#009900", "red"))

# Add line geom in to see the outliers
ggplot(data = congressIncomes, aes(x = year, y = AvgValue, group = cid)) +
        scale_y_continuous(name = "Average Individual Net Worth", labels = comma) +
        labs(title = "Average Individual Cognressional Net Worth Across Time") +
        geom_line()

#################################################################################

######################## Select on certain individuals and map across time ######
johnKerry <- congressIncomes[which(congressIncomes$Name == "John Kerry"), ]

qplot(x = as.factor(year), y = AvgValue, data = johnKerry) +
        labs(title = "John Kerry's Net Worth", x = "Year") +
        scale_y_continuous(name = "Average Net Worth", labels = comma)

#################################################################################









