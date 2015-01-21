#we graph the class mean RIT for each test for each grade.
#we use the raw file first
data <- read.csv("location of .csv file")
gr1 <- data %>% group_by(MeasurementScaleName, GradeName) %>% summarize(MeanRIT = mean(TestRITScore))

p1r <- ggplot(gr1, aes(GradeName, MeanRIT, fill=factor(GradeName))) + geom_bar(stat = "identity") 
  + coord_cartesian(ylim = c(150, 250))
  + facet_grid(MeasurementScaleName ~ .)
  + labs(title = "Mean RIT score per test & grade level", x = "Grade level", y = "Mean RIT score")
  + guides(fill = FALSE)

p1p <- ggplot(data, aes(factor(GradeName), TestPercentile, fill=factor(GradeName)))
  + geom_boxplot(stat = "boxplot") + guides(fill=F) 
  + facet_grid(MeasurementScaleName ~ .)
  + labs(title = paste("Mean RIT score per test & grade level \n", data[1,5]), x = "Grade level", y = "Mean RIT score")


#now, we do one with verticall bar, faceting with grade level, axis is factor of testname
p2 <- gr1 %>% ggplot(aes(MeasurementScaleName, MeanRIT, fill = MeasurementScaleName)) + geom_bar(stat = "identity") 
  + coord_cartesian(ylim = c(150, 250))
  + theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(), legend.title=element_blank())
  + labs(title = "Mean RIT score per grade level & per test", x = "Test", y = "Mean RIT score")
  + facet_grid(. ~ GradeName)

######################################################################
# now let's do the same with box & whisker plot using medians grade.
# a lot more meaningfull to see the spread of skills in a class

p3 <- ggplot(data2, aes(factor(GradeName), TestPercentile, fill=factor(GradeName))) 
  + geom_boxplot(stat = "boxplot") + guides(fill=F) 
  + facet_grid(. ~ MeasurementScaleName)