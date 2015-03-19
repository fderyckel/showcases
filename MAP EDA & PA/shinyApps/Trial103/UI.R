#Shiny UI

library(shiny)

shinyUI(navbarPage("MAP results",
  
  tabPanel("Summaries",
           
          sidebarLayout(
            
            sidebarPanel(
              selectInput("testname",           
                          "Select the test to visualize",
                          levels(mapdata$MeasurementScaleName)),
              
              selectInput("termname",         
                           "Select the time the test was taken",
                           levels(mapdata$TermName)),
              hr(),
              checkboxInput("gender", "Gender differentiation"),
              checkboxInput("ritscale", "RIT scale on the y-axis"),
              helpText("By default, the scale is set to percentile"),
              checkboxInput("values", "See values for histogram"),
              hr(),
              checkboxInput("boxplot", "See distribution of abilities"),
              helpText("Tick the box, for a box & wisker plot. If you would prefer to see how students' ability is distributed."),
              
              hr(),
              helpText("Data provided by the NWEA website for AISL (from Fall 2009 to Fall 2014)")
              ),

          mainPanel(
            plotOutput("mapgraph", height = "auto"),
            tableOutput("table1")
            )
          )
        ),
  
  tabPanel("In depth",
           sidebarLayout(
             
             sidebarPanel(
               selectInput("testname2",           
                           "Select the test to visualize",
                           levels(mapdata$MeasurementScaleName)),
               selectInput("termname2",         
                           "Select the term the test was taken",
                           levels(mapdata$TermName)),
               selectInput("gradename2",
                           "What grade levels",
                           choices = c(3, 4, 5, 6, 7, 8 , 9, 10)),
               hr(),
               helpText("Data provided by the NWEA website for AISL (from Fall 2009 to Fall 2014)")
             ),
             
             mainPanel(
               plotOutput("mapboxplots", height="auto")
             )
           )
  ),
  tabPanel("Growth visualizations")
        )
)