######### Necessary packages ##############
library(plyr)
library(data.table)
library(reshape2)

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
colnames(incomeRaw)[8] <- "State"
############################################################################################################

####### Drop the executive and court instances #############################################################
congressIncomes <- subset(incomeRaw, incomeRaw$chamber != "E" & incomeRaw$chamber != "C")
head(congressIncomes)
############################################################################################################




# let's make some mutha fuckin plots.....but later.
library(ggplot2)
library(lattice)
year2004 <- subset(congressIncomes, year == "2004")
year2005 <- subset(congressIncomes, year == "2005")
year2006 <- subset(congressIncomes, year == "2006")

xyplot(AvgValue ~ Name, data = year2004)
# ggplot will probably be better for this...this one looks like crap but it definately shows there are some outliers and the scale is a mess becuase ggplot2 is mean about that,
# as asthetically pleasing as ggplot can be
plot1 <- ggplot(year2004, aes(y = AvgValue, x = Name), xlab = "Member of Congress", ylab = "Average Value") + geom_point() 
plot1
