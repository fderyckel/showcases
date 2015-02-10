#Script to tidy the data to make the wide df from MWEA a long one
# in order to boxplot the strands

library(dplyr)
library(tidyr)

data <- read.csv("Documents/R/Portfolio/MAP EDA & PA/datarawish/MAP data raw.csv")

## Here we'll really just keep the columns that interest us.

data <- data %>% select(1, 2, 4, 5, 6, 7, 15, 18, 21, 24, 27, 30, 33)
maplong <- data %>% gather("Strands", "RIT Score", GoalRITScore1:GoalRITScore7, na.rm = TRUE)

