#load the data
library(ggplot2)
library(dplyr)
apples <- read.csv("JoburgApples.csv")

#To take away the comma-asterisks. Use of double backlash as the asterisk is a metacharacter.
#use of gsub instead of sub (so consider every instance)
apples[,2] <- gsub(",\\*", "", apples[,2])

#also check for dates, as this is a pain !! 
apples[,8] <- as.Date(apples[,8], format="%Y-%m-%d")

#Notice the | inside the quotation marks, this is the OR operator - also no space before or after it!  
#Filtering the varieties that are count 120, 135..
#The dollar sign at the end of 120... is to find all rows n ProductName that end by 120 or 135
data <- apples %>% filter(grepl("120$|135$", ProductName), tvaluesold >= 10000, date > "2014-10-1")
grs120 <- as.data.frame(data %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny Smith", Count = "120-135"))
cpp120 <- as.data.frame(data %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink", Count = "120-135"))
top120 <- as.data.frame(data %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red", Count = "120-135"))
ski120 <- as.data.frame(data %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking", Count = "120-135"))
gld120 <- as.data.frame(data %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious", Count = "120-135"))

df120 <- rbind(grs120, cpp120, top120, gld120)

#now for the count 70 to 90
data2 <- apples %>% filter(grepl("70$|80$|90$", ProductName), tvaluesold >= 10000)
grs70 <- as.data.frame(data2 %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny Smith", Count = "070-90"))
cpp70 <- as.data.frame(data2 %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink", Count = "070-90"))
top70 <- as.data.frame(data2 %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red", Count = "070-90"))
ski70 <- as.data.frame(data2 %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking", Count = "070-90"))
gld70 <- as.data.frame(data2 %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious", Count = "070-90"))

df70 <- rbind(grs70, cpp70, top70, gld70)

#now for the count 100 to 110
data3 <- apples %>% filter(grepl("100$|110$", ProductName), tvaluesold >= 10000)
grs100 <- as.data.frame(data3 %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny Smith", Count = "100-110"))
cpp100 <- as.data.frame(data3 %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink", Count = "100-110"))
top100 <- as.data.frame(data3 %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red", Count = "100-110"))
ski100 <- as.data.frame(data3 %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking", Count = "100-110"))
gld100 <- as.data.frame(data3 %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious", Count = "100-110"))

df100 <- rbind(grs100, cpp100, top100, gld100)

apples <- rbind(df70, df100, df120)


#Now we graph the data.
p1 <- ggplot(apples, aes(date, AvgPrice, color=Variety)) + geom_line() + facet_grid(Count ~ .) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Apples price over time", x="Dates", y="Average price (in Zar)")
p2 <- ggplot(apples, aes(date, AvgPrice, color=Count)) + geom_line() + facet_grid(Variety ~ .) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Apples price over time", x="Dates", y="Average price (in Zar)")
coor <- coord_cartesian(ylim = c(150, 250))
