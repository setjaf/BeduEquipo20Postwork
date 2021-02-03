library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# El archivo momios.R y el postwork 3 se han corrido aparte y sus imágenes se
# han guardado en la carpeta www

ui <- fluidPage(
    tabsetPanel(
        tabPanel("Graficas goles",
                 titlePanel("Grafica de goles"), hr(),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("Opciones",
                                      label = c("Goles local-visitante"),
                                      choices = c("Local", "Visitante"))
                     ),
                     mainPanel(
                         plotOutput("Grafica")
                     )
                 )
        ),
        tabPanel("Graficas Postwork 3",
                 titlePanel("Graficas del postwork 3"), hr(),
                 img(src = "plots_pstwrk_3.png", height = 800, width = 700)
        ),
        tabPanel("Dataset",
                 titlePanel("match.data.csv"), hr(),
                 fluidRow(column(DT::dataTableOutput("df"),
                                 width = 12))
        ),
        tabPanel("Factores de ganancia",
                 titlePanel("Factores de Ganancia mínimo y máximo"), hr(),
                 img(src = "plot_1.png", height = 500, width = 700), hr(),
                 img(src = "plot_2.png", height = 500, width = 700)
        )
    )
)

server <- function(input, output) {
    output$df = DT::renderDataTable(
        DT::datatable({
            read.csv("match.data.csv")
        })
    )
    output$Grafica = renderPlot({
        aux = read.csv("match.data.csv")
        aux1 = c()
        if(input$Opciones == "Local"){
            aux1 = aux[,c("home.score", "away.team")]
            aux1 %>% ggplot(aes(x = home.score)) +
                geom_bar(fill = "#3adcc6") + facet_wrap("away.team") +
                scale_x_discrete("Número de goles como local", breaks = 0:10) +
                ylab("")
        } else{
            aux1 = aux[, c("away.score", "away.team")]
            aux1 %>% ggplot(aes(x = away.score)) +
                geom_bar(fill = '#0572ec') + facet_wrap("away.team") +
                scale_x_discrete("Número de goles como visitante",
                                 breaks = 0:10) + ylab("")
        }
    })}

shinyApp(ui = ui, server = server)
