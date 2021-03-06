#Shiny UI

library(shiny)

shinyUI(navbarPage("Fruits on the Joburg market",
                   
                   tabPanel("Apples",
                            sidebarLayout(
                              
                              sidebarPanel(
                                selectInput("productname",
                                            "Select your product",
                                            choices = levels(df1$Variety)
                                            ),
                                selectInput("count",
                                            "Select your size",
                                            choices = levels(df1$Count)),
                                hr(),
                                dateRangeInput("dateRange",
                                            "Select your dates",
                                            max = Sys.Date(),
                                            separator = "-", format = "dd/mm/yy",
                                            startview = "year", weekstart = 1),
                                helpText("Prices & Volumes started to be recorded in November 2014"),
                                hr(),
                                helpText("Only products in 18.5 Kg boxes have been considered")
                                ),
                              
                            mainPanel(
                              htmlOutput("applesgraph")
                              )
                            )
                            ),
                   tabPanel("Oranges"),
                   tabPanel("Mangoes"),
                   tabPanel("Grapes")
                   
                   ))