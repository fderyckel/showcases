#this script intends to add a column variety and anothre one count
#the purpose is to be able to better use the facettign / wrapping of ggplot2

library(dplyr)

apples <- read.csv("JoburgApples.csv")

apples[,2] <- gsub(",\\*", "", apples[,2])
apples[,8] <- as.Date(apples[,8], format="%Y-%m-%d")

grs <- apples %>% filter(grepl("GRANNY", ProductName), tvaluesold >10000) %>% mutate(Variety = "Granny Smith")
cpp <- apples %>% filter(grepl("PINK", ProductName), tvaluesold >10000) %>% mutate(Variety = "Cripps Pink")
top <- apples %>% filter(grepl("TOP", ProductName), tvaluesold >10000) %>% mutate(Variety = "Top Red")
gld <- apples %>% filter(grepl("GOLDEN", ProductName), tvaluesold >10000) %>% mutate(Variety = "Golden Delicious")
ski <- apples %>% filter(grepl("STARKING", ProductName), tvaluesold >10000) %>% mutate(Variety = "Starking")
bra <- apples %>% filter(grepl("BRAEBURN", ProductName), tvaluesold >10000) %>% mutate(Variety = "Braeburn")

apples <- rbind(grs, cpp, top, gld, ski, bra)

s70 <- apples %>% filter(grepl("70$", ProductName)) %>% mutate(Count = 70)
s80 <- apples %>% filter(grepl("80$", ProductName)) %>% mutate(Count = 80)
s90 <- apples %>% filter(grepl("90$", ProductName)) %>% mutate(Count = 90)
s100 <- apples %>% filter(grepl("100$", ProductName)) %>% mutate(Count = 100)
s110 <- apples %>% filter(grepl("110$", ProductName)) %>% mutate(Count = 110)
s120 <- apples %>% filter(grepl("120$", ProductName)) %>% mutate(Count = 120)
s135 <- apples %>% filter(grepl("135$", ProductName)) %>% mutate(Count = 135)
s150 <- apples %>% filter(grepl("150$", ProductName)) %>% mutate(Count = 150)
s165 <- apples %>% filter(grepl("165$", ProductName)) %>% mutate(Count = 165)

apples <- rbind(s70, s80, s90, s100, s110, s120, s135, s150, s165)
