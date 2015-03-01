# Server.R

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

mapdata <- read.csv("MAP data raw.csv")

shinyServer(function(input, output, session) {
  
  output$mapgraph <- renderPlot({
    
    if(input$gender) {
      if(input$ritscale) {
        if(input$boxplot) {
          #if gender V, ritsclae V and boxplot V
          graphRIT <- reactive (mapdata %>% 
                      filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                      group_by(GradeName))
          ggplot(graphRIT(), aes(as.factor(GradeName), TestRITScore, fill = as.factor(StudentGender))) +
            geom_boxplot() + geom_jitter(aes(fill=StudentGender)) +
            theme(legend.title = element_blank()) + 
            labs(x = "Grade Level", y = "Median RIT score + quartiles") + 
            theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
                          }
        else {
          #if gender V, ritscale V, but not boxplot
          graphRIT <- reactive (mapdata %>% 
                      filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                      group_by(GradeName, StudentGender) %>% 
                      summarise(meanRIT = mean(TestRITScore)))
          ggplot(graphRIT(), aes(as.factor(GradeName), meanRIT, fill = as.factor(StudentGender))) +
            geom_bar(stat="identity", position = "dodge") + 
            coord_cartesian(ylim = c(150, 250)) + 
            labs(x = "Grade Level", y = "Mean RIT Score") + 
            theme(legend.title = element_blank())
            }
      }
      else {
        if(input$boxplot) {
          #if gender V, ritscale notV, boxplot V
          graphRIT <- reactive (mapdata %>% 
                                  filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                                  group_by(GradeName))
          ggplot(graphRIT(), aes(as.factor(GradeName), TestPercentile, fill = as.factor(StudentGender))) +
            geom_boxplot() + 
            theme(legend.title = element_blank()) + 
            coord_cartesian(ylim = c(10, 100)) + 
            labs(x = "Grade Level", y = "Median RIT score + quartiles") + 
            theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
                          }
        else{
          #if gender V, ritscale not V, boxplot not V
          graphRIT <- reactive (mapdata %>% 
                      filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                      group_by(GradeName, StudentGender) %>% 
                      summarise(meanPer = mean(TestPercentile)))
          ggplot(graphRIT(), aes(as.factor(GradeName), meanPer, fill = as.factor(StudentGender))) +
            geom_bar(stat="identity", position = "dodge") + 
            labs(x = "Grade Level", y = "Mean RIT Percentile") + 
            theme(legend.title = element_blank()) +
            scale_y_continuous(breaks=seq(20, 80, 10)) +
            coord_cartesian(ylim = c(15, 85)) + 
            theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
            }
      }
    }
    
    else  {
      if(input$ritscale) {
        #if gender not V, ritscale V, boxplot V
        if(input$boxplot) {
          graphRIT <- reactive (mapdata %>% 
                      filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                      group_by(GradeName))
          ggplot(graphRIT(), aes(as.factor(GradeName), TestRITScore, fill = as.factor(GradeName))) +
            geom_boxplot() + 
            coord_cartesian(ylim = c(150, 280)) + 
            labs(x = "Grade Level", y = "Mean RIT Score") + 
            guides(fill = FALSE)
                          }
        else {
          #this is the else from no boxplot + RIT
          if(input$values){
            #if gender not V, ritscale V, boxplot not V BUT values V
            #that's RIT with values
            graphRIT <- reactive (mapdata %>% 
                        filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                        group_by(GradeName) %>% 
                        summarise(meanRIT = mean(TestRITScore)))
            ggplot(graphRIT(), aes(as.factor(GradeName), meanRIT, fill = as.factor(GradeName))) +
            geom_bar(stat="identity") + 
            stat_identity(geom="text", aes(label=round(meanRIT)), vjust=-1.5) + 
            coord_cartesian(ylim = c(150, 250)) + 
            labs(x = "Grade Level", y = "Mean RIT Score") + 
            guides(fill = FALSE) + 
            theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
                          }
          else{
            #if gender not V, ritscale V, boxplot not V AND values not V
            #finish no values, so that is just RIT
            graphRIT <- reactive (mapdata %>% 
                                    filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                                    group_by(GradeName) %>% 
                                    summarise(meanRIT = mean(TestRITScore)))
            ggplot(graphRIT(), aes(as.factor(GradeName), meanRIT, fill = as.factor(GradeName))) +
              geom_bar(stat="identity") + 
              coord_cartesian(ylim = c(150, 250)) + 
              labs(x = "Grade Level", y = "Mean RIT Score") + 
              guides(fill = FALSE) + 
              theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
          }
          #finishing RIT V here.
      }
    }
      else {
        #we starting no RIT (ie. percentile)
        if(input$boxplot) {
            graphRIT <- reactive (mapdata %>% 
                          filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                          group_by(GradeName))
            ggplot(graphRIT(), aes(as.factor(GradeName), TestPercentile, fill = as.factor(GradeName))) +
              geom_boxplot() +
              labs(x = "Grade Level", y = "Mean RIT Percentile") + 
              guides(fill = FALSE) + 
              scale_y_continuous(breaks=seq(20, 80, 10)) + 
              theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
                            }
          else {
            #the else of the no boxplot (just percentile)
            if(input$values) {
              #percentile (no RIT), yes value, no boxplot
              graphRIT <- reactive (mapdata %>% 
                                      filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                                      group_by(GradeName) %>% 
                                      summarise(meanper = mean(TestPercentile)))
              ggplot(graphRIT(), aes(as.factor(GradeName), meanper, fill = as.factor(GradeName))) +
                geom_bar(stat="identity") + 
                stat_identity(geom="text", aes(label=round(meanper)), vjust=-1.5) + 
                guides(fill=FALSE) + 
                coord_cartesian(ylim = c(15, 85)) + 
                labs(x = "Grade Level", y = "Mean RIT Percentile") + 
                theme(legend.title = element_blank())
                 }
            else{
              #this is the percentile (not RIT), no box plot, no values
              graphRIT <- reactive(mapdata %>% 
                                     filter(TermName == input$termname, MeasurementScaleName == input$testname) %>% 
                                     group_by(GradeName) %>% 
                                     summarise(meanPer = mean(TestPercentile)))
              ggplot(graphRIT(), aes(as.factor(GradeName), meanPer, fill = as.factor(GradeName))) +
                geom_bar(stat="identity") +
                labs(x = "Grade Level", y = "Mean RIT Percentile") + 
                guides(fill = FALSE) + 
                scale_y_continuous(breaks=seq(20, 80, 10)) + 
                coord_cartesian(ylim = c(15, 85)) + 
                theme(axis.title.x = element_text(size=14), axis.text.x = element_text(face="bold", size=12), axis.title.y = element_text(size=14), axis.text.y = element_text(face="bold", size=12), panel.grid.major.x = element_blank() )
            }
        }}
          }
  
  },
  height = function() {
    session$clientData$output_mapgraph_width
  }
  
  )
  
  
  output$mapboxplots <- renderPlot({
    graph2RIT <- reactive (mapdata %>% 
                             filter(TermName == input$termname2, MeasurementScaleName == input$testname2, GradeName == input$gradename2) %>%  
                             select(1, 2, 4, 5, 6, 7, 15, 18, 21, 24, 27, 30, 33) %>% 
                             gather("Strands", "RITScore", GoalRITScore1:GoalRITScore7, na.rm = TRUE)
                          )
      ggplot(graph2RIT(), aes(factor(Strands), RITScore)) + 
      geom_boxplot(aes(fill=factor(Strands))) + 
      labs(title = "Visualisation of strands for given input", x = "Goals", y = "RIT Score" )
  }, 
  height = function() {
    session$clientData$output_mapboxplots_width
  }
  )
  
})