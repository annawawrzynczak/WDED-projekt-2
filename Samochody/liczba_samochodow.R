library(readr)
library(dplyr)
library(plotly)
library(readxl)
library(shiny)

samochody_liniowy <- read_excel("Pojazdy_zarejestrowane_2021_19.xlsx", 
                                sheet = "Arkusz4")

samochody_liniowy$miesiąc_rok <- factor(samochody_liniowy$miesiąc_rok, levels = samochody_liniowy[["miesiąc_rok"]])
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Zanieczyszceznia w Polsce produkowane przez samochody"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(6, 
               
               checkboxGroupInput("wojewodztwo", "Jakie województwo cię interesuje?", colnames(samochody_liniowy[,2:17]))
        ),
        
        column(6,
               
        )
    ),
    fluidRow(
        column(6,
               plotlyOutput("pointPlot")
        ),
        column(6,
               
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$pointPlot <- renderPlotly({
        plot_ly(
            data = samochody_liniowy,
            x = ~miesiąc_rok,
            y = as.formula(paste0('~', input$wojewodztwo)),
            type = 'scatter', mode = 'lines'
        )%>% 
            layout(title = "Samochody zarejestrowane w poszczególnych miesiącach w Polsce",
                    xaxis = list(title = "Miesiąc"),
                    yaxis = list (title = "Liczba"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
