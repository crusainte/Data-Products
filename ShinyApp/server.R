library(shiny)
require(rCharts)

# Load data processing file
source("processData.R")

shinyServer(
    function(input, output) {
        output$inputValue <- renderPrint({input$year})
        
        output$myChart <- renderChart({

            plotResultsByYear(rankCountriesByYear(dataT,input$year),input$year)
        })
    }
)