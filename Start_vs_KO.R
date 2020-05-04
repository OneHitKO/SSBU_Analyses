# Setup 
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggrepel)

SSBU.data <- readRDS("SSBU.data")

# Define UI ----
ui <- fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("SSBU: Harder, Better, Stronger, Faster"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Identify the quickest KO moves for your favorite character!"),
            
            selectInput(inputId = "char", label = "Select your character",
                        choices = SSBU.data$Name,
                        selected = "Mario"),
            
            sliderInput(inputId = "max_frames", label = "Maximum frames to display", 
                        min = 10, max = 60, step = 10, value = 50),
            
            sliderInput(inputId = "max_perc", label = "Maximum kill percent",
                        min = 100, max = 850, step = 50, value = 200),
            
            helpText("Hover your cursor over a move for details."),
            
            width = 3
        ),
        
        mainPanel(
            plotOutput(outputId = "scatterplot", hover = "hover"),
            tableOutput(outputId = "details"),
            uiOutput("dynamic")
        )
    )
)

# Define server ----
server <- function(input, output) {
    output$scatterplot <-renderPlot({
        
        ggplot(data = subset(SSBU.data, Name == input$char), aes(x=Startup, y=Kill_Perc))+
            geom_jitter(aes(color = Type), size = 4)+
            labs(title = input$char, x = "Total Startup Frames", y = "Kill Percentage")+
            geom_text_repel(aes(label = Move), box.padding = 0.5, min.segment.length = 1)+
            ylim(0,input$max_perc)+
            xlim(0,input$max_frames)+
            theme_bw()
    },
    height = 400, width = 600)
    
    output$dynamic <- renderUI({
        req(input$hover)
        verbatimTextOutput("displayed.vals")
    })
    
    output$details <- renderTable({
        hover <- input$hover
        nearPoints(subset(SSBU.data, Name == input$char), input$hover) %>%
            transmute("Move" = Move, "Startup Frames" = Startup, "Kill %" = Kill_Perc)
    },
    align = "c")
}

# Run ----
shinyApp(ui = ui, server = server)
