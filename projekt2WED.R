library(dplyr)
library(readxl)
library(shiny)
library(ggplot2)
#data_excel <- read_excel("data_excel.xlsx", col_types = c("numeric", "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "text", "skip", "skip", "skip", "skip", "skip", "skip", "numeric", "skip", "skip", "skip", "text", "text", "skip", "skip",  "skip", "skip", "skip", "skip", "skip",  "skip", "skip", "skip", "skip"))
#colnames(data_excel)<-c("ID", "Country", "Mk", "Cn", "Ewltp", "Ft", "Fm")

#fuel_kinds <- data_excel %>% distinct(Ft) %>% select(Ft)
#Car_Brand <- data_excel %>% distinct(Mk) %>% select(Mk)
#data_grouped <- data_excel %>% group_by(Mk, Cn, Ft) %>% summarize(mean_Ewltp = mean(na.omit(Ewltp)))


ui <- shinyUI(fluidPage(
  titlePanel("Tytul"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Brand", "Marka", Car_Brand),
      checkboxInput("Petrol", "PETROL", FALSE),
      checkboxInput("Diesel", "DIESEL", FALSE),
      checkboxInput("Petrolelectric", "PETROL/ELECTRIC", FALSE),
      checkboxInput("Dieselelectric", "DIESEL/ELECTRIC", FALSE),
      checkboxInput("LPG", "LPG", FALSE),
      checkboxInput("NG_Biometane", "NG_BIOMETANE", FALSE),
      checkboxInput("NG", "NG", FALSE)
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
))
draw_plot <- function(data, Brand, Fuels){
  data<-data[is.element(data$Ft, Fuels), ] %>% filter(Mk==Brand)
  p<-ggplot(data, aes(x=data$Cn, y=data$mean_Ewltp, fill=Ft)) + geom_col(position='dodge',width=0.5, alpha=0.5) + coord_flip() + labs(y="Spalanie [g/km]", x="Model") + scale_fill_discrete(name = "Rodzaj Paliwa")
  p
}
server <- shinyServer(function(input, output) {
  
  output$Plot <- renderPlot({
    fuels <- c()
    if(input$Petrol) fuels<-append(fuels, 'PETROL')
    if(input$Diesel) fuels<-append(fuels, 'DIESEL')
    if(input$Petrolelectric) fuels<-append(fuels, 'PETROL/ELECTRIC')
    if(input$Dieselelectric) fuels<-append(fuels, 'DIESEL/ELECTRIC')
    if(input$LPG) fuels<-append(fuels, 'LPG')
    if(input$NG_Biometane) fuels<-append(fuels, 'NG-BIOMETHANE')
    if(input$NG) fuels<-append(fuels, 'NG')
    p<-draw_plot(data_grouped, input$Brand, fuels)
    p
    
  }, height=1000, width=1000)
  
})

shinyApp(ui, server)
