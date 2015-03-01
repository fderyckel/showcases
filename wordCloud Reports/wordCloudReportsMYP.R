library(dplyr)
library(tm)
library(wordcloud)

#The first part of the script, will create the .txt file for each of the teachers in the .csv file

setwd("~/Documents/R/KG reports/")
data <- read.csv("Gr4CommentsEd.csv")

teacher <- levels(data$Teacher)

for (i in 1:length(teacher)) {
  teacherComments <- filter(data, Teacher == teacher[i])
  write.csv(teacherComments[,2], paste0("Comments/", teacher[i], ".txt"), row.names=F)
}

#now this second part is to create the word cloud
path <- dir("Comments/")

for (i in 1:length(path)) {
  text <- readLines(paste0("Comments/", path[i]))
  fdr <- Corpus(VectorSource(text))
  fdr <- tm_map(fdr, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
  fdr <- tm_map(fdr, content_transformer(tolower), mc.cores=1)
  fdr <- tm_map(fdr, removePunctuation, mc.cores=1)
  fdr <- tm_map(fdr, removeWords, c(stopwords("english"), "semester"), mc.cores=1)
  fdr <- tm_map(fdr, stripWhitespace, mc.cores=1)
  dtm <- DocumentTermMatrix(fdr)
  m <- as.matrix(dtm)
  v <- sort(colSums(m), decreasing = T)
  words <- names(v)
  d <- data.frame(word = words, freq=v)
  
  path[i] <- gsub("\\.txt", "", path[i])
  
  write.csv(d, paste0("tbl", path[i], ".csv"), row.names=F)
  
  png(paste0("wc", path[i], ".png"), width = 900, height=650)
  wordcloud(d$word, d$freq, scale = c(5, 0.5), min.freq = 15, random.order = F, colors = brewer.pal(8, "Dark2"))
  dev.off()
  
}




