library(XML)
library(dplyr)
ApplePrices <- data.frame(readHTMLTable("http://www.joburgmarket.co.za/dailyprices.php?commodity=90&containerall=2"))
GrapePrices <- data.frame(readHTMLTable("http://www.joburgmarket.co.za/dailyprices.php?commodity=33&containerall=2"))
MangoPrices <- data.frame(readHTMLTable("http://www.joburgmarket.co.za/dailyprices.php?commodity=45&containerall=2"))
OrangePrices <- data.frame(readHTMLTable("http://www.joburgmarket.co.za/dailyprices.php?commodity=64&containerall=2"))

#add the date
ApplePrices <- mutate(ApplePrices,Sys.Date())
GrapePrices <- mutate(GrapePrices,Sys.Date())
MangoPrices <- mutate(MangoPrices,Sys.Date())
OrangePrices <- mutate(OrangePrices,Sys.Date())

colnames(ApplePrices) <- c("Container", "UnitMass", "ProductName", "TotalValueSold", "TotalQuantitySold", "TotalKgSold", "AveragePrice", "HighestPrice", "AveragePriceperKg", "HighestPriceperKg", "Date")
colnames(GrapePrices) <- c("Container", "UnitMass", "ProductName", "TotalValueSold", "TotalQuantitySold", "TotalKgSold", "AveragePrice", "HighestPrice", "AveragePriceperKg", "HighestPriceperKg", "Date")
colnames(MangoPrices) <- c("Container", "UnitMass", "ProductName", "TotalValueSold", "TotalQuantitySold", "TotalKgSold", "AveragePrice", "HighestPrice", "AveragePriceperKg", "HighestPriceperKg", "Date")
colnames(OrangePrices) <- c("Container", "UnitMass", "ProductName", "TotalValueSold", "TotalQuantitySold", "TotalKgSold", "AveragePrice", "HighestPrice", "AveragePriceperKg", "HighestPriceperKg", "Date")

#take away the comma used for thousands separator
ApplePrices[,4] <- gsub(",", "", ApplePrices[,4])
GrapePrices[,4] <- gsub(",", "", GrapePrices[,4])
MangoPrices[,4] <- gsub(",", "", MangoPrices[,4])
OrangePrices[,4] <- gsub(",", "", OrangePrices[,4])

#take away the R use for the Rand symbol
#take away some unnecessary columns that I can calculate myself
ApplePrices[,c(2,4,7:10)] <- as.numeric(gsub("R", "", as.matrix(ApplePrices[,c(2,4,7:10)])))
ApplePrices <- select(ApplePrices, -AveragePriceperKg, -HighestPriceperKg, -UnitMass)
GrapePrices[,c(2,4,7:10)] <- as.numeric(gsub("R", "", as.matrix(GrapePrices[,c(2,4,7:10)])))
GrapePrices <- select(GrapePrices, -AveragePriceperKg, -HighestPriceperKg, -UnitMass)
MangoPrices[,c(2,4,7:10)] <- as.numeric(gsub("R", "", as.matrix(MangoPrices[,c(2,4,7:10)])))
MangoPrices <- select(MangoPrices, -AveragePriceperKg, -HighestPriceperKg, -UnitMass)
OrangePrices[,c(2,4,7:10)] <- as.numeric(gsub("R", "", as.matrix(OrangePrices[,c(2,4,7:10)])))
OrangePrices <- select(OrangePrices, -AveragePriceperKg, -HighestPriceperKg, -UnitMass)

#only consider the 18.5 Kg boxes Cl 1 and take out the Cl2 products that are 18.5 Kg boxes
#Consider 9Kg boxe & 8 x 1.5 Kg boxe
ApplePrices <- ApplePrices %>% filter(TotalValueSold>0) %>% filter(!grepl(",CL 2,\\*,|CL 2,S|CL 2,XS", ProductName)) %>% filter(grepl("18.50KG|9KG|1.5", Container))
GrapePrices <- GrapePrices %>% filter(TotalValueSold>0)
#only consider the 4 Kg tray
MangoPrices <- MangoPrices %>% filter(TotalValueSold>0) %>% filter(grepl("4KG", Container))
OrangePrices <- OrangePrices %>% filter(TotalValueSold>0)

ApplePrices
GrapePrices
MangoPrices
OrangePrices

#write the df into a file, without writing row names and colum names.
write.table(ApplePrices, file="JoburgApples.csv", sep=",", col.names=F, row.names=F, append=TRUE)
write.table(GrapePrices, file="JoburgGrape.csv", sep=",", col.names=F, row.names=F, append=TRUE)
write.table(MangoPrices, file="JoburgMango.csv", sep=",", col.names=F, row.names=F, append=TRUE)
write.table(OrangePrices, file="JoburgOrange.csv", sep=",", col.names=F, row.names=F, append=TRUE)
