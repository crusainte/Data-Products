library(shiny)
require(rCharts)

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Singapore Airport Arrivals by Country of Embarkation"
),
                   
hr(),   
            
            mainPanel(
                sliderInput("yearRange", label = h4("Input year range"), min = 1978, 
                            max = 2014, value = c(1978, 2014)),
                submitButton('Submit'),
                hr(),
                uiOutput("chartTitle"),
                showOutput("myChart", "nvd3"),
                tags$head(tags$style(HTML(".nv-axislabel {font: 15px Arial; font-weight:bold;}"))) # to style labels
            )
        
    )
)
