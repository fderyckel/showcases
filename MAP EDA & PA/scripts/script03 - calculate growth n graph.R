library(dplyr)

#this script calculate the growth from YoY and SoS.

#load the data.  
#data <- read.csv("sourcefile")

data <- data %>% group_by(StudentID) %>% filter(n() > 1) %>%
  arrange(StudentID, GradeName, TermName) %>%
  mutate(mathGrowth = mathRIT - lag(mathRIT))%>%
  select(StudentID, GradeName, TermName, mathRIT, mathGrowth) %>%
  arrange(GradeName, TermName, StudentID)

View(data)

ungroup(data)

data2 <- data %>% filter(TermName == "Fall 2014") %>% group_by(GradeName) %>% 
  summarize(meanMathGrowth = mean(mathGrowth, na.rm=T))

##TO DO
## add the lang & read
## add by strands


View(data2)

##TO DO
##graph (vertical) from grade 2 to 10, all growth for each strands
##graph (longitudinal) mean growth for a certain grade for several year in a row.