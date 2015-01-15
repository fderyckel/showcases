##read the .txt file.  
## it is a tab separated values, and we have to make explicit the encoding
map <- read.table(source, sep = "\t", header=T, fileEncoding="UTF-16")

#check its structure & view the first few rows
str(data)
head(data)

#take out all the columns of no interest + take out names (for privacy)
library(dplyr)
data <- map %>% select(-RITtoReadingMin, -StudentName, -RITtoReadingMax, -TestName, -StudentEthnic, -TestTypeName, -InstitutionName, -TestStartDate, -RITtoReadingScore)

##reconstruct a wide df first
#separate by Measurement Scale & rename the column
math<- data %>% filter(MeasurementScaleName == "Mathematics") %>% 
  rename(mathRIT = TestRITScore, mathPercentile = TestPercentile, mathStdErr = TestStdErr, Computation = GoalRITScore1, NumberSense = GoalRITScore2, Geometry = GoalRITScore3, Measurement = GoalRITScore4, StatProb = GoalRITScore5, Algebra = GoalRITScore6, ProblemSolving = GoalRITScore7) %>% 
  select(-MeasurementScaleName, -GoalName1, -GoalName2, -GoalName3, -GoalName4, -GoalName5, -GoalName6, -GoalName7) %>% 
  rename(ComputationAdj = GoalAdjective1, NumberSenseAdj = GoalAdjective2, GeometryAdj = GoalAdjective3, MeasurementAdj = GoalAdjective4, StatProbAdj = GoalAdjective5, AlgebraAdj = GoalAdjective6, ProblemSolvingAdj = GoalAdjective7)

#we can take out more columns here as we'll bind these data later on.
lang <- data %>% filter(MeasurementScaleName == "Language Usage") %>% 
  rename(langRIT = TestRITScore, langPercentile = TestPercentile, langStdErr = TestStdErr, GramUs = GoalRITScore1, SpPunctCap = GoalRITScore2, ParaSent = GoalRITScore3, GenPurpAud = GoalRITScore4) %>% 
  select(-MeasurementScaleName, -GoalName1, -GoalName2, -GoalName3, -GoalName4, -GoalName5, -GoalName6, -GoalName7, -GoalAdjective5, -GoalAdjective6, -GoalAdjective7, -GoalRITScore5, -GoalRITScore6, -GoalRITScore7) %>% 
  rename(GramUsAdj = GoalAdjective1, SpPunctCapAdj = GoalAdjective2, ParaSentAdj = GoalAdjective3, GenPurpAudAdj = GoalAdjective4)

read <- data %>% filter(MeasurementScaleName == "Reading") %>% 
  rename(readRIT = TestRITScore, readPercentile = TestPercentile, readStdErr = TestStdErr, ReadProc = GoalRITScore1, LitTex = GoalRITScore2, NFictTex = GoalRITScore3) %>% 
  select(-MeasurementScaleName, -GoalName1, -GoalName2, -GoalName3, -GoalName4, -GoalName5, -GoalName6, -GoalName7, -GoalAdjective4, -GoalAdjective5, -GoalAdjective6, -GoalAdjective7, -GoalRITScore4, -GoalRITScore5, -GoalRITScore6, -GoalRITScore7) %>% 
  rename(ReadProcAdj = GoalAdjective1, LitTexAdj = GoalAdjective2, NFictTexAdj = GoalAdjective3)

#join all these tables together
map1 <- full_join(math, read)
map2 <- full_join(map1, lang)
map <- map2

#count how many missing values for each test
#or how many kids didn't do their test
sum(is.na(map$mathRIT))
sum(is.na(map$langRIT))
sum(is.na(map$readRIT))
