#cleaning data for apples
library(dplyr)

data <- read.csv("~/Documents/R/Portfolio/JoburgMarket/dataRaw/JoburgApples.csv")
data[,8] <- as.Date(data[,8], format="%d/%m/%y")
data[,2] <- gsub(",\\*", "", data[,2])

variety = c("CRIPPS PINK", "CRIPPS RED", "TOPRED", 
            "GOLDEN DELICIOUS", "STARKING", "GRANNY SMITH", 
            "ROYAL GALA", "PANORAMA GOLDEN", "FUJI", "BRAEBURN")
count1 = c("80$|90$", "100$|110$", "120$|135$", "150$|165$|198$", "G6$|G7$|G8$", "M$|L$|XL$")
count2 = c("80-90", "100-110", "120-135", "150-165-198", "Econo", "Jumble")
df = data.frame(matrix(vector(), 0, 10, dimnames=list(c(), c(names(data), "Variety", "Count"))), 
                stringsAsFactors=F)

for (i in 1:length(variety)) {
  df1 <- data %>% filter(grepl(variety[i], ProductName)) %>% mutate(Variety = as.factor(variety[i]))
  for (i in 1:length(count1)){
    df2 <- df1 %>% filter(grepl(count1[i], ProductName)) %>% mutate(Count = as.factor(count2[i]))
    df <- rbind(df, df2)
  }
}

df <- df %>% 
  group_by(date, Variety, Count) %>% 
  summarise(Price = round(sum(avgprice * tquantitysold) / sum(tquantitysold)), 
            Value = sum(tvaluesold), 
            Quantity = sum(tquantitysold)) %>% 
  arrange(date, Variety, Count)

write.csv(df, "~/Documents/R/Portfolio/JoburgMarket/shinyApps/cleanApples.csv", row.names=F)
