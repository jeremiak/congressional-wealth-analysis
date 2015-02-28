######### Necessary packages ##############
library(plyr)
library(data.table)
library(reshape2)

###########################################


############# Read in the Data ############
years <- list.files(pattern = ".csv")
congressIncomes <- data.frame()

# lapply
congressIncomes <- do.call(rbind, lapply(years, read.csv))

# the difference in system.time() for the two methods is a measly .048 seconds, however lapply seems to be dealing with the numerics better for some reason
###########################################

############ Have a look ##################
#head(congressIncomes)
#str(congressIncomes)
congressIncomes[, 3:5] <- sapply(congressIncomes[, 3:5], as.numeric)
colnames(congressIncomes)
#head(congressIncomes)
summary(congressIncomes$AvgValue)
###########################################


# So some things to consider: I think the "Name" variable needs to be split. Name should be a seperate case, Party affiliation should be a seperate case,
# and so should their home state. This would allow for another few layers of analysis (visualizing by state and party). Not sure what the "Source" column is.
# Looks like all NAs, so I'd drop it all together.  My thought on plotting this would be to start global and then go local. Visualize the global distribution of wealth by party,
# then by state, then by district (is that what the cid variable is? I couldn't tell...a quick google search didn't turn up any documentation for this one).


############## Add in the 'Party' variable #####################################
congressIncomes$Name <- as.character(congressIncomes$Name)
for (i in 1:nrow(congressIncomes)) {
  if (grepl(pattern = "D\\-", congressIncomes$Name[i]) == TRUE) {
    congressIncomes$Party[i] = "D"
  } else if (grepl(pattern = "R\\-", congressIncomes$Name[i]) == TRUE) {
    congressIncomes$Party[i] = "R"
  } else {
    congressIncomes$Party[i] = "I"
  }
}

# remove rows that represent the exeutive and judiciary
for (i in 1:nrow(congressIncomes)) {
  if (!is.na(congressIncomes$chamber[i])) {
    if ((congressIncomes$chamber[i] == "E") | (congressIncomes$chamber[i] == "J")) {
      congressIncomes[-(i),]
    }
  }
}
################################################################################




######################## Split off names in 'Name' and add seperate variable for 'State' #############################################################
congressIncomes <- data.frame(congressIncomes, colsplit(congressIncomes$Name, pattern = " \\(", names = c("Names", "State")))
head(congressIncomes)

# Next step will be to drop the Party ID and cleanup state remnants 
congressIncomes$State <- sub(pattern = "[D, R, I]\\-", replacement = "", x = congressIncomes$State)
congressIncomes$State <- sub(pattern = ")", replacement = "", x = congressIncomes$State)

# Drop columns Source and Name as we no longer need that data
congressIncomes$Name <- NULL
congressIncomes$Source <- NULL

# Rename Names column back to nicer sounding Name
colnames(congressIncomes)[9] <- "Name"

states <- unique(congressIncomes$State)


######################################################################################################################################################

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
