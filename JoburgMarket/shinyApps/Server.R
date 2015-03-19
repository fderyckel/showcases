# Server.R
library(shiny)
library(googleVis)
library(dplyr)


shinyServer(function(input, output, session) {
  
  output$applesgraph <- renderGvis({
    dfa <- reactive({df1 %>% filter(Variety == input$productname & Count == input$count)})
    
    gvisComboChart(dfa(), xvar = "date", 
                   yvar = c("Price", "Quantity"), 
                   options=list(title=input$productname, seriesType="bars", 
                                series="[{type:'line', targetAxisIndex: 0, linewidth: 2}, 
                                {type:'bars', targetAxisIndex: 1}]", 
                                
                                vAxes="[{title: 'Price (in Zar)'}, 
                                {title: 'Volume (in boxes)'}]", 
                                
                                hAxes="[{title: 'Date'}]",
                                height=600))
  } 
    )
  
}
  )