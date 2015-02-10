#We assume, the .txt file is clean (that is no unwanted character such as . " ' $path etc.)
## this script, takes the raw .txt file, takes out the names of the students, format the dates,
## and rewrite as a .csv file with the term name as title
## It does this recursively for all the raw files in the sourcefile indicated in the directory.

#TO DO ... how to use R to clean these files of unwanted characters
##split the termName into Year and Semester ... easier for later arrange.

library(dplyr)

setwd("~/")
sourceFile <- "Documents/R/MAP Predictive Analytics/datacleaned/"
path <- dir(sourceFile, pattern = "\\.txt", full.names=T)

for (i in 1:length(path)) {
  ##read the .txt file.  
  map <- read.table(path[i], sep = "\t", header=T, fileEncoding="UTF-16", na.strings="")

  #we'll do some cleaning
  map[,13] = as.Date(map[,13], format = "%m/%d/%Y")
  map[,c(15, 37, 38)] <- as.integer(as.matrix(map[,c(15, 37, 38)]))

  #let's already write on disk this cleaner, raw-ish version into a .csv format
  # + just take out names (for privacy)
  filename = paste0(map[1,6], " raw.csv")
  map <- select(map, -StudentName)
  write.table(map, file= filename, sep=",", col.names=T, row.names=F)

  #take out all the columns of no interest
  data <- map %>% select(-RITtoReadingMin, -RITtoReadingMax, -StudentEthnic, -TestTypeName, -InstitutionName, -TestStartDate, -RITtoReadingScore)


  ##reconstruct a wide df
  #separate by Measurement Scale & rename the column
  math<- data %>% filter(MeasurementScaleName == "Mathematics") %>% 
    rename(mathTestName = TestName, mathRIT = TestRITScore, mathPercentile = TestPercentile, mathStdErr = TestStdErr, computation = GoalRITScore1, numberSense = GoalRITScore2, Geometry = GoalRITScore3, measurement = GoalRITScore4, statProb = GoalRITScore5, algebra = GoalRITScore6, problemSolving = GoalRITScore7) %>% 
    select(-MeasurementScaleName, -GoalName1, -GoalName2, -GoalName3, -GoalName4, -GoalName5, -GoalName6, -GoalName7) %>% 
    rename(computationAdj = GoalAdjective1, numberSenseAdj = GoalAdjective2, geometryAdj = GoalAdjective3, measurementAdj = GoalAdjective4, statProbAdj = GoalAdjective5, algebraAdj = GoalAdjective6, problemSolvingAdj = GoalAdjective7)

#we can take out more columns here as we'll bind these data later on.
  lang <- data %>% filter(MeasurementScaleName == "Language Usage") %>% 
    rename(langTestName = TestName, langRIT = TestRITScore, langPercentile = TestPercentile, langStdErr = TestStdErr, langRITScore1 = GoalRITScore1, langRITScore2 = GoalRITScore2, langRITScore3 = GoalRITScore3, langRITScore4 = GoalRITScore4) %>% 
    select(-MeasurementScaleName, -GoalName1, -GoalName2, -GoalName3, -GoalName4, -GoalName5, -GoalName6, -GoalName7, -GoalAdjective5, -GoalAdjective6, -GoalAdjective7, -GoalRITScore5, -GoalRITScore6, -GoalRITScore7) %>% 
    rename(langAdj1 = GoalAdjective1, langAdj2 = GoalAdjective2, langAdj3 = GoalAdjective3, langAdj4 = GoalAdjective4)

  read <- data %>% filter(MeasurementScaleName == "Reading") %>% 
    rename(readTestName = TestName, readRIT = TestRITScore, readPercentile = TestPercentile, readStdErr = TestStdErr, readRITScore1 = GoalRITScore1, readRITScore2 = GoalRITScore2, readRITScore3 = GoalRITScore3) %>% 
    select(-MeasurementScaleName, -GoalName1, -GoalName2, -GoalName3, -GoalName4, -GoalName5, -GoalName6, -GoalName7, -GoalAdjective4, -GoalAdjective5, -GoalAdjective6, -GoalAdjective7, -GoalRITScore4, -GoalRITScore5, -GoalRITScore6, -GoalRITScore7) %>% 
    rename(readAdj1 = GoalAdjective1, readAdj2 = GoalAdjective2, readAdj3 = GoalAdjective3)

  #join all these tables together
  map1 <- full_join(math, read)
  map2 <- full_join(map1, lang)
  map <- map2

  #save everything into a new .csv file
  filename = paste0(map[1,4], " wide.csv")
  write.table(map, file = filename, sep=",", col.names = T, row.names = F)
}
