library(shiny)
require(rCharts)

# Define UI for dataset viewer application
shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("Airport Arrivals in Singapore by Country of Embarkation"),
        
        sidebarPanel(
            numericInput('year', 'Enter the year (between 1978 and 2014)', 1978, min = 1978, max = 2014, step = 1),
            submitButton('Submit')
        ),
        mainPanel(
            h3('Results of prediction'),
            h4('You entered'),
            verbatimTextOutput("inputValue"),
            h4('Chart'),
            showOutput("myChart", "nvd3")
        )
    )
)
