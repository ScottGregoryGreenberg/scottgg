# Read into R the data in the files "cheese1.csv" and "cheese2.csv".  These
# datasets contain the per capita consumption of different cheeses from 1995 to
# 2014. The two data sets cover the same years, although the names for the
# variable holding the year differ.  Merge the data together into a single
# data.frame in which all the consumption data for a given year is one
# observation.  Name your merged data.frame "cheese".  The dimension of this
# data.frame should be 20 rows and 7 columns.

cheese1 <- read.csv(file="https://dahl.byu.edu/223/2018a2/hw/01/files/cheese1.csv",head=TRUE,sep=",")
names(cheese1)[1] <- "Time"
cheese2 <- read.csv(file="https://dahl.byu.edu/223/2018a2/hw/01/files/cheese2.csv",head=TRUE,sep=",")
cheese <- merge(cheese1, cheese2, by="Time")
# Which kind of cheese has the highest average consumption per capita over all
# years?
cm <- colMeans(cheese[2:7], na.rm = TRUE)
# cheddar
# Which year saw the largest total consumption per capita of all cheeses?
ys <- rowSums(subset(cheese, select = c(2:7)), na.rm = TRUE)
yswy <- data.frame(cheese[,1],ys)
#2007