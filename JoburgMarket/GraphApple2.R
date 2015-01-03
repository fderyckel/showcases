#load the data
library(ggplot2)
library(dplyr)
apples <- read.csv("JoburgApples.csv")

#To take away the asterisks. Use of double backlash as the asterisk is a metacharacter.
apples[,2] <- gsub(",\\*", "", apples[,2])

#also check for dates, as this is a pain !! 
apples[,8] <- as.Date(apples[,8], format="%Y-%m-%d")

#Notice the | inside the quotation marks, this is the OR operator - also no space before or after it!  
#Filtering the varieties that are count 120, 135..
#The dollar sign at the end of 120... is to find all rows n ProductName that end by 120 or 135
data <- apples %>% filter(grepl("120$|135$", ProductName), tvaluesold >= 10000, date > "2014-10-1")
grs120 <- as.data.frame(data %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny 120-135"))
cpp120 <- as.data.frame(data %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink 120-135"))
top120 <- as.data.frame(data %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red 120-135"))
brae120 <- as.data.frame(data %>% filter(grepl("BRAEBURN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Braeburn 120-135"))
fuj120 <- as.data.frame(data %>% filter(grepl("FUJI", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Fuji 120-135"))
ski120 <- as.data.frame(data %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking 120-135"))
gld120 <- as.data.frame(data %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious 120-135"))
df120 <- rbind(grs120, cpp120, top120, brae120, fuj120, ski120, gld120)

#now for the count 70 to 90
data2 <- apples %>% filter(grepl("70$|80$|90$", ProductName), tvaluesold >= 10000)
grs70 <- as.data.frame(data2 %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny 070-90"))
cpp70 <- as.data.frame(data2 %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink 070-90"))
top70 <- as.data.frame(data2 %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red 070-90"))
brae70 <- as.data.frame(data2 %>% filter(grepl("BRAEBURN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Braeburn 070-90"))
fuj70 <- as.data.frame(data2 %>% filter(grepl("FUJI", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Fuji 070-90"))
ski70 <- as.data.frame(data2 %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking 070-90"))
gld70 <- as.data.frame(data2 %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious 070-90"))
df70 <- rbind(grs70, cpp70, top70, brae70, fuj70, ski70, gld70)

#now for the count 100 to 110
data3 <- apples %>% filter(grepl("100$|110$", ProductName), tvaluesold >= 10000)
grs100 <- as.data.frame(data3 %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny 100-110"))
cpp100 <- as.data.frame(data3 %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink 100-110"))
top100 <- as.data.frame(data3 %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red 100-110"))
brae100 <- as.data.frame(data3 %>% filter(grepl("BRAEBURN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Braeburn 100-110"))
fuj100 <- as.data.frame(data3 %>% filter(grepl("FUJI", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Fuji 100-110"))
ski100 <- as.data.frame(data3 %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking 100-110"))
gld100 <- as.data.frame(data3 %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious 100-110"))
df100 <- rbind(grs100, cpp100, top100, brae100, fuj100, ski100, gld100)

#now for the count 150 to 165 & 180
data4 <- apples %>% filter(grepl("150$|165$|180$", ProductName), tvaluesold >= 10000)
grs150 <- as.data.frame(data4 %>% filter(grepl("GRANNY", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Granny 150-180"))
cpp150 <- as.data.frame(data4 %>% filter(grepl("CRIPPS PINK", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Cripps Pink 150-180"))
top150 <- as.data.frame(data4 %>% filter(grepl("TOPRED", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Top Red 150-180"))
brae150 <- as.data.frame(data4 %>% filter(grepl("BRAEBURN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Braeburn 150-180"))
fuj150 <- as.data.frame(data4 %>% filter(grepl("FUJI", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Fuji 150-180"))
ski150 <- as.data.frame(data4 %>% filter(grepl("STARKING", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Starking 150-180"))
gld150 <- as.data.frame(data4 %>% filter(grepl("GOLDEN", ProductName)) %>% group_by(date) %>% summarise(AvgPrice = mean(avgprice)) %>% mutate(Variety= "Golden Delicious 150-180"))
df150 <- rbind(grs150, cpp150, top150, brae150, fuj150, ski150, gld150)

#create the df for each of the variety
dfGRS <- rbind(grs70, grs100, grs120, grs150)
dfCPP <- rbind(cpp70, cpp100, cpp120, cpp150)
dfTOP <- rbind(top70, top100, top120, top150)
dfGLD <- rbind(gld70, gld100, gld120, gld150)
dfSKI <- rbind(ski70, ski100, ski120, ski150)

##TODO: do  dftop, dfgld, dfski, dfgrs

#Now we graph the data.
p120 <- ggplot(df120, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for count 120-135 (in Zar)")
p70 <- ggplot(df70, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for count 70-90 (in Zar)")
p100 <- ggplot(df100, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for counts 100-110 (in Zar)")
pcpp <- ggplot(dfCPP, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for Cripps Pink (in Zar)")
pgrs <- ggplot(dfGRS, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for Granny Smith (in Zar)")
pski <- ggplot(dfSKI, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for Starking (in Zar)") 
ptop <- ggplot(dfTOP, aes(date, AvgPrice, group=Variety, color=Variety)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + labs(title="Apples price over time", x="Dates", y="Average price for Topred (in Zar)")


