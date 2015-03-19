#cleaning data for Oranges
library(dplyr)

data <- read.csv("~/Documents/R/Portfolio/JoburgMarket/dataRaw/Oranges.csv")
data[,8] <- as.Date(data[,8], format="%Y-%m-%d")
data[,2] <- gsub(",\\*", "", data[,2])

variety = c("NAVEL", "CARA CARA", "DELTA SEEDLESS", 
            "MIDKNIGHT", "VALENCIA")
count1 = c("CL 1,40$|CL 1,48$", "CL 1,60$|CL 1,56$", "CL 1,64$", "CL 1,72$", 
           "CL 1,88$", "CL 1,L$|CL 1,M$", "CL 1,S$|CL 1,XS$")
count2 = c("Cl1 40-48", "Cl1 56-60", "Cl1 64", "Cl1 72", "Cl1 88", "Cl1 M/L", "Cl1 S/XS")
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
  group_by(Date, Variety, Count, Container) %>% 
  summarise(Price = round(sum(AveragePrice * TotalQuantitySold) / sum(TotalQuantitySold)), 
            Value = sum(TotalValueSold), 
            Quantity = sum(TotalQuantitySold)) %>% 
  arrange(Date, Variety, Count)

write.csv(df, "~/Documents/R/Portfolio/JoburgMarket/shinyApps/cleanOranges.csv", row.names=F)
