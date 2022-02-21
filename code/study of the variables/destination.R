#Destination
table_destination <- table(cab_rides$destination)
print(table_destination)
prop.table(table_destination)
barplot(table_destination, main = "Distribution of race destinations", xlab = "Number of races", col = "orange")

#Description of destination
by(cab_rides[,c(1, 5)], cab_rides[,"destination"], FUN = summary)

boxplot(cab_rides$distance ~ cab_rides$destination, col = "yellow", main = paste("Boxplot for Distance according to the destination"), cex.axis= 0.6, xlab = "Destination", ylab = "Distance")
