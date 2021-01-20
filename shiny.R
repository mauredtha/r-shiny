install.packages('shiny')
install.packages('babynames')
install.packages('dplyr')
install.packages('DT')
install.packages('shinythemes')
library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)
library(shinythemes)

# create the UI
ui <- fluidPage(
  titlePanel("My First Shiny App"),
  #theme = shinythemes::shinytheme('superhero'),
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel('Hello, Ich Bin Dery',
                 textInput('name','type your name'),
                 selectInput('sex', 'select sex', selected = 'F',
                             choices = c('M', 'F', 'X')),
                 sliderInput('year', 'Years', min = 1900, max = 2010, value = c(1900,1945))
                ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput('table_top_10_names')),
        tabPanel('DT', DT::DTOutput('dt_top_10_names')),
        tabPanel('Plot', plotOutput('plot_top_10_names')),
        tabPanel('Text', 
                 textOutput('greeting'),
                 plotOutput('trend')
                 )
      )
    )
  )
)

# create the server
server <- function(input, output, session){
  
  #Function to create data frame 10 top names by sex and year
  top_10_names <- function(){
    #Get top 10 names by sex and year
    top_10_names <- babynames %>%
      # MODIFY CODE BELOW : Filter for the selected sex
      filter(sex == input$sex) %>%
      #filter(year == 1900) %>%
      filter(year == input$year) %>%
      top_n(10, prop)
  }
  
  output$greeting <- renderText({paste('Hello', input$name)})
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    
    # Plot top 10 names by sex and year
    ggplot(top_10_names(), aes(x = name, y = prop)) + 
      geom_col(fill = "#263e63")
  })
  
  output$table_top_10_names <- renderTable({
    top_10_names()
  })
  
  output$dt_top_10_names <- DT::renderDT({
    top_10_names()
  })
  
}

# combine the UI and server
shinyApp(ui, server)