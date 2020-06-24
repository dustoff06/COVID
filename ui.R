library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID19 Spatial Maps and Regression Analysis by County"),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Map",
                         leafletOutput("map", width="150%")), 
                tabPanel("Res Map:  Deaths/Cap",
                         plotOutput("plot3")), 
                tabPanel("Res Map: Cases/Cap",
                         plotOutput("plot4")),
                tabPanel("Spatial Reg.: Deaths/Cap", 
                         verbatimTextOutput("summary")),
                tabPanel("Spatial Reg:  Cases/Cap",
                         verbatimTextOutput("summary2"))
            )
            
        )
    )
)
