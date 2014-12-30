library(XML)
library(dplyr)
prices <- data.frame(readHTMLTable("http://www.joburgmarket.co.za/dailyprices.php?commodity=90&containerall=2"))
prices <- mutate(prices,Sys.Date())
colnames(prices) <- c("Container", "UnitMass", "ProductName", "TotalValueSold", "TotalQuantitySold", "TotalKgSold", "AveragePrice", "HighestPrice", "AveragePriceperKg", "HighestPriceperKg", "Date")

#take away the comma used for thousands separator
prices[,4] <- gsub(",", "", prices[,4])

#take away the R use for the Rand symbol
prices[,c(2,4,7:10)] <- as.numeric(gsub("R", "", as.matrix(prices[,c(2,4,7:10)])))
prices <- select(prices, -AveragePriceperKg, -HighestPriceperKg, -UnitMass)
prices <- prices %>% filter(TotalValueSold>0) %>% filter(!grepl("CL 2", ProductName), Container == "18.50KG CARTON" ) %>% arrange(TotalQuantitySold)

prices


write.table(prices, file="JoburgApples.csv", sep=",", col.names=F, row.names=F, append=TRUE)
