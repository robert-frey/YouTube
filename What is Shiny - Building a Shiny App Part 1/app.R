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

db = dbConnect(SQLite(),"statcast_db.sqlite")

sc = dbGetQuery(conn = db, statement = "SELECT * FROM statcast_data_2020")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statcast Data in 2020"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("ev",
                        "Exit Velocity:",
                        min = 1,
                        max = 120,
                        value = 60)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           ###WATCH YOUTUBE VIDEO FOR CODE####
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable({
    sc %>% dplyr::filter(launch_speed >= input$ev) %>% sample_n(10)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
