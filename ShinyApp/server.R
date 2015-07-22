library(shiny)
require(rCharts)

# Load data processing file
source("processData.R")

shinyServer(
    function(input, output) {    
        output$myChart <- renderChart({
            plotResults(rankCountriesByYearRange(dataT,input$yearRange[1],input$yearRange[2]))
        })
        
        output$chartTitle <- renderUI({
            if(input$yearRange[1] != input$yearRange[2])
            {
                title <- paste("Top 10 Airport Arrival Countries between ",input$yearRange[1], " and ", input$yearRange[2])
            }
            else
            {
                title <- paste("Top 10 Airport Arrival Countries for ",input$yearRange[1])
            }
            
            h4(title)
        })
    }
)