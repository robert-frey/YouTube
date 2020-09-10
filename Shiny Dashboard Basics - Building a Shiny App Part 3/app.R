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

#install.packages('shinydashboard')
library(shinydashboard)
#install.packages('DT')
library(DT)

db = dbConnect(SQLite(),"statcast_db.sqlite")

sc = dbGetQuery(conn = db, statement = "SELECT * FROM statcast_data_2020")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",

    # Application title
    dashboardHeader(title = "Basic Statcast App"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(),
        
         #selectInput("name","Select Batter",choices = unique(sc$player_name)),
         #sliderInput("ev","Exit Velocity", value = 60, min = 0, max = 120)

        # Show a plot of the generated distribution
        dashboardBody(
           ###WATCH YOUTUBE VIDEO FOR CODE###
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sc_reactive <- reactive({
        sc %>% dplyr::filter(player_name == input$name,
                             launch_speed >= input$ev) %>% mlbam_xy_transformation()
    })

    output$table <- DT::renderDataTable({
        head(sc_reactive(),10)
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
