library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(bslib)

df <- zanieczyszczenie %>% 
  na.omit() %>% 
  rename(Spaliny = `Ewltp (g/km)`) %>% 
  filter(Spaliny > 0) 
nazwy_marek <- (unique(df$Mk))


ui <- fluidPage(
  textOutput('opiswykres2'),
  
  selectInput("paliwo", "Wybierz rodzaj napędu aut: ",
              choices = unique(as.character(df$Ft)),
              multiple = FALSE,
              selected = c((unique(as.character(df$Ft)))[1], (unique(as.character(df$Ft)))[2])
              
  ),
 # selection<- selectInput("marka", "Wybierz markę samochodów:",
 #              choices = nazwy_marek,
 #              multiple = FALSE,
 #              selected = c(nazwy_marek[1], nazwy_marek[3])
 #              ),
  
  uiOutput("secondSelection"),
  
  
  mainPanel(
    plotlyOutput("Plot1"),
    tableOutput("table")
  ),
 textOutput('wniosekwykres2')
  
        
)

server <- function(input, output) {

  output$opiswykres2 <- renderText({paste0("Firmy samochodowe przyczyniają się do globalnego ocieplenia. ",
                                           "Aby zmniejszyć emisję dwutlenku węgla, sprzedawane są auta wykorzystujące ",
                                           "różne rodzaje paliwa. ",
                                           "Poniższy wykres przedstawia zależności pomiędzy modelami",
                                           " samochodów zarejestrowanych w Polsce w 2020 roku a średnią",
                                           " ilością obliczoną w teście WLTP wyprodukowanych przez te auta spalin.")
  }) 
  
  output$secondSelection <- renderUI({
    selectInput("marka", "Wybierz markę samochodów:",
                choices = unique(df[df$Ft == input$paliwo,"Mk"])
                )

  })
  
  
  output$Plot1 <- renderPlotly({
    df %>% 
      filter( Ft %in% input$paliwo &
             Mk %in% input$marka
      ) %>%
      group_by(Cn) %>%
      summarize(srednia = mean(Spaliny)) %>% 
      plot_ly(
        x=~Cn,
        y=~srednia,
        type = "bar",
       #, color =~Ft,
      #  colors = rainbow(length(Ft)),
      hovertemplate = paste('Spaliny: %{y}<br>',
      'Model: %{x}<br><extra></extra>'),
      showlegend = FALSE
      )%>% 
      layout(xaxis = list(
                          title = paste("Modele aut od", ~input$marka)),
             yaxis =list(title = "Średnia ilość spalin w g/km"),
             title = " Średnia ilość spalin produkowana przez poszczególne modele aut")
    
  })
  output$table <- renderTable({
    
    df %>% 
      group_by(Ft,Mk,Cn) %>%
      summarize(srednia = mean(Spaliny)) %>% 
      arrange(desc(srednia)) %>% 
      rename("Model" = Cn, "Typ paliwa" = Ft,
             "Firma" = Mk, `Średnie spaliny w g/km` = srednia) %>% 
      head(5)
    
  })
  
  output$wniosekwykres2 <- renderText({paste0("Na podstawie wykresu można stwierdzić, że",
                            " niewielu klientów i firm samochodowych interesuje się autami hybrydowymi z przekładnią",
                        " beznynowo-elektryczną oraz pojazdami napędzanymi biometanem.",
                  " Można również zaobserwować, że wśród samochodów, które",
            " działają za pomocą tego samego rodzaju paliwa, wartości produkowanych spalin są sobie bliske dla różnych modeli.")
    
  })
   
}

shinyApp(ui , server)
