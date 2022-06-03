library(readr)
library(dplyr)
library(plotly)
library(readxl)
library(shiny)

samochody_liniowy <- read_excel("Zeszyt_pd.xlsx")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Zanieczyszceznia w Polsce produkowane przez samochody"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(6, 
               
               #checkboxGroupInput("wybierz_wojewodztwo", "Jakie województwo cię interesuje?", 
                #                  factor(unique(samochody_liniowy$wojewodztwo))),
               sliderInput("wybierz_rok", "Rok:", value = c(2015,2021), min = 2015, max = 2021, step =1)
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
        ggplotly(ggplot(samochody_liniowy[(samochody_liniowy$rok >= input$wybierz_rok[1] & samochody_liniowy$rok <= input$wybierz_rok[2]) 
                                         ,], 
                        aes(x = rok, y = suma_w_roku/1000, group = wojewodztwo)) + 
                                                   geom_line(aes(color = wojewodztwo))+
                                                   geom_point(aes(color = wojewodztwo))+
                     ggtitle("Liczba samochodów zarejestrowanych w Polsce") +
                     xlab("Rok")+
                     ylab("Liczba w tys.")
                 )
           
        
       
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
