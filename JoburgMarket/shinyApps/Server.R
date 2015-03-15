# server.R
library(shiny)
library(googleVis)
library(dplyr)

setwd("~/Documents/R/Portfolio/JoburgMarket/shinyApps/")

#apple part
apdata <- read.csv("JoburgApples.csv")
apdata[,8] <- as.Date(apdata[,8], format="%d/%m/%y")
apdata[,2] <- gsub(",\\*", "", data[,2])
apdata <- filter(apdata, UnitMass == "18.50KG CARTON")
apdata <- apdata[,c(1, 2, 4, 6, 8)]
apdata[,2] <- as.factor(apdata[,2]) 
apdata <- arrange(apdata, date)

shinyServer(function(input, output) {
  df <- reactive({apdata %>% filter(ProductName == input$productname)
                  })
  output$applesgraph <- renderGvis({
    gvisComboChart(df(), xvar = "date", yvar = c("avgprice", "tquantitysold"), options=list(title=input$productname, seriesType="bars", series="[{type:'line'}, {type:'bars'}]", height=600))
  }
    
    )
  
}
  )


#chart <- gvisComboChart(data5, xvar = "date", yvar = c("avgprice", "tquantitysold"), options=list(title="Starking Count 100", seriesType="bars", series="[{type:'line'}, {type:'bars'}]", height=600))



