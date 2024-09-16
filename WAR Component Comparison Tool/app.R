#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
library(shiny)
#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("mlbplotR")
library(mlbplotR)
#install.packages("magick")
library(magick)
#install.packages("cowplot")
library(cowplot)
#install.packages("shinyjs")
library(shinyjs)

hs <- mlbplotR::load_headshots() %>% dplyr::select(savant_id, espn_headshot)
tms <- mlbplotR::load_mlb_teams() %>% dplyr::select(team_name = team_abbr, team_color, team_color2) %>%
  dplyr::mutate(team_name = case_when(team_name == "AZ" ~ "ARI",
                                      team_name == "SD" ~ "SDP",
                                      team_name == "SF" ~ "SFG",
                                      team_name == "KC" ~ "KCR",
                                      team_name == "CWS" ~ "CHW",
                                      TRUE ~ team_name),
                #Adjust Color Code for Mets because it doesn't like to plot with other blue teams
                team_color = ifelse(team_name == "NYM",team_color2,team_color))

bat <- baseballr::fg_bat_leaders(startseason = "2024", endseason = "2024")

war_bat <- bat %>% dplyr::filter(PA > 0) %>%
  dplyr::select(savant_id = xMLBAMID, player = PlayerName, team_name, Batting, BaseRunning, Fielding, Positional, League=wLeague, Replacement, RAR, WAR, WAROld) %>% 
  mutate(Fielding = ifelse(is.na(Fielding),0,Fielding),
                      sumWAR = Batting + BaseRunning + Fielding + Positional + League + Replacement) %>%
  mutate_at(vars(Batting,BaseRunning,Fielding,Positional,League,Replacement, WAR), ~round(.,1)) %>%
  left_join(.,hs,by=c("savant_id")) %>%
  left_join(., tms, by = "team_name") %>%
  #replace NA player_headshots with blank ones
  mutate(espn_headshot = ifelse(is.na(espn_headshot),"https://midfield.mlbstatic.com/v1/people/821938/spots/120?zoom=1.2",espn_headshot))


ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "icon", type = "image/jpg", size = "32x32", href = "https://www.freylytics.com/wp-content/uploads/go-x/u/30cbf355-a74f-4a45-bff7-53e6d70567e8/l51,t0,w1898,h1265/image-375x250.jpg")),
  
  # Application title
  titlePanel( title =  div(img(src="https://www.freylytics.com/wp-content/uploads/go-x/u/30cbf355-a74f-4a45-bff7-53e6d70567e8/l51,t0,w1898,h1265/image-375x250.jpg"), 'Batter WAR Component Comparison Tool'), windowTitle = "WAR Comparison" ), 
  
  fluidRow(
    column(4,
           selectInput("player_1","Select Player:", choices = sort(war_bat$player), selected = "Aaron Judge"),
           selectInput("player_2","Select Player:", choices = sort(war_bat$player), selected = "Bobby Witt Jr."),
           helpText("Created by Robert Frey, @RobertFrey40 on X for any questions/comments")),
    column(8,
           plotOutput("player_1_plot"),
           tableOutput("player_1_table"),
           plotOutput("player_2_plot"),
           tableOutput("player_2_table"))
  )
  
)

server <- function(input, output, session) {
  
  player_df1 <- reactive({
    
    df <- war_bat %>% dplyr::filter(player == input$player_1) %>%
      pivot_longer(c(Batting,BaseRunning,Fielding,Positional,League,Replacement), names_to = "Metric")
    
    df$Metric <- factor(df$Metric, levels = c("Batting", "BaseRunning", "Fielding", "Positional", "Replacement", "League"))
    
    df
    
  })
  
  player_df2 <- reactive({
    
    df <- war_bat %>% dplyr::filter(player == input$player_2) %>%
      pivot_longer(c(Batting,BaseRunning,Fielding,Positional,League,Replacement), names_to = "Metric")
    
    df$Metric <- factor(df$Metric, levels = c("Batting", "BaseRunning", "Fielding", "Positional", "Replacement", "League"))
    
    df
    
  })
  
  output$player_1_plot <- renderPlot({
    logo <- magick::image_read(player_df1()$espn_headshot[1])
    
    plot1 <- ggplot(player_df1(), aes(x = Metric, y = value, fill = team_color)) +
      geom_bar(stat = "identity") +
      geom_label(aes(label=paste(value,"Runs")), vjust=0.25, fill = "white") +
      scale_fill_manual(values=player_df1()$team_color) +
      ylim(min(c(player_df1()$value,player_df2()$value)), max(c(player_df1()$value, player_df2()$value))) +
      labs(caption = "Data: FanGraphs, Figure: @RobertFrey40") + 
      theme_classic() +
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            axis.text = element_text(size = 14),
            plot.caption = element_text(size = 12))
    
    ggplot() +
      draw_plot(plot1, x = 0, y = 0.15, width = 1, height = 0.85) +
      draw_image(logo, x = 0.4, y = 0.5, width = 0.25, height = 0.5) 
    
  })
  
  output$player_1_table <- renderTable({
    
    bat %>% dplyr::filter(PlayerName == input$player_1) %>%
      dplyr::select(player=PlayerName,pos=position,G,PA, `wRC+`=wRC_plus,WAR)
    
  })
  
  output$player_2_plot <- renderPlot({
    
    logo <- magick::image_read(player_df2()$espn_headshot[1])
    
    plot1 <- ggplot(player_df2(), aes(x = Metric, y = value, fill = team_color)) +
      geom_bar(stat = "identity") +
      geom_label(aes(label=paste(value,"Runs")), vjust=0.25, fill = "white", label.size = 0.75) +
      scale_fill_manual(values=player_df2()$team_color) +
      ylim(min(c(player_df1()$value,player_df2()$value)), max(c(player_df1()$value, player_df2()$value))) +
      labs(caption = "Data: FanGraphs, Figure: @RobertFrey40") + 
      theme_classic() +
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            axis.text = element_text(size = 14),
            plot.caption = element_text(size = 12))
    
    ggplot() +
      draw_plot(plot1, x = 0, y = 0.15, width = 1, height = 0.85) +
      draw_image(logo, x = 0.4, y = 0.5, width = 0.25, height = 0.5)
    
    
  })
  
  output$player_2_table <- renderTable({
    
    bat %>% dplyr::filter(PlayerName == input$player_2) %>%
      dplyr::select(player=PlayerName,pos=position,G,PA, `wRC+`=wRC_plus,WAR)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
