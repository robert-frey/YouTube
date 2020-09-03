#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(RSQLite)
library(DBI)
library(shiny)
library(tidyverse)
library(GeomMLBStadiums)

db = dbConnect(SQLite(),"statcast_db.sqlite")

sc = dbGetQuery(conn = db, statement = "SELECT * FROM statcast_data_2020")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statcast Data in 2020"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
         selectInput("name","Select Batter",choices = unique(sc$player_name)),
         sliderInput("ev","Exit Velocity", value = 60, min = 0, max = 120)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("table"),
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sc_reactive <- reactive({
     ###WATCH YOUTUBE VIDEO FOR CODE###
    })

    output$table <- renderTable({
        head(sc_reactive,10)
    })
    
    output$plot <- renderPlot({
        ggplot(sc_reactive(),aes(x=hc_x_, y=hc_y_, color=events)) + 
            geom_spraychart(stadium_ids = "dodgers",
                            stadium_transform_coords = TRUE, 
                            stadium_segments = "all") + 
            theme_void() + 
            coord_fixed() +
            ggtitle(paste(input$name,"Spray Chart on Balls Greater Than or Equal to",
                          input$ev,"MPH"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
