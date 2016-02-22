library(read.table)


incomes <- list.files(pattern = ".txt")
test <- read.table(incomes[1], header = FALSE, sep = ",", quote = "|")




test2 <- read.table(incomes[2], header = FALSE, sep = ",", quote = "|")
test3 <- read.table(incomes[2], header = FALSE, sep = ",", quote = "|")
