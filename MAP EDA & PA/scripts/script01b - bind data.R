##short script to add at the end of the "script01 - read data.R" script.  
## after creating all the files, lets bind them together for some exploratory data analysis
## this scirpt can actually be added to the "script01 - read data.R" ... It can all be one big operation. 

sourcefile <- ("Documents/R/Portfolio/MAP EDA & PA/datarawish/")
path <- dir(sourcefile, pattern = "\\ raw.csv", full.names=T)
data <- read.csv(path[1])

for (i in 2:length(path)) {
  data2 <- read.csv(path[i])
  data <- rbind(data, data2)
}

write.table(data, file = "MAP data raw.csv", sep = ",", col.names = T, row.names = F)