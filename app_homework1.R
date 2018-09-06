library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

pdf(NULL)

data("diamonds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Diamonds NavBar", 
             theme = shinytheme("sandstone"),
             tabPanel("Diamonds by Cuts",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cut_select",
                                      "Cuts:",
                                      choices = levels(diamonds$cut),
                                      multiple = TRUE,
                                      selectize = TRUE,
                                      selected = c("Fair", "Good", "Very Good", "Premium", "Ideal"))
                        ),
                        # Output plots
                        mainPanel(
                          fluidPage(plotlyOutput("cut_count_plot"), 
                                    plotlyOutput("cut_carat_plot"))
                                   )
                        ),
                      
                      h3("Table by Cut"),
                      DT::dataTableOutput("table_by_cut"), 
                      h3("Table by Carat"),
                      DT::dataTableOutput("table_by_carat")
                      
                      )
             ,
             # Carat - Cut Interaction Plot
             tabPanel("Diamonds by Cuts & Carats",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("carat_select",
                                      "Carats:",
                                      min = min(diamonds$carat, na.rm = T),
                                      max = max(diamonds$carat, na.rm = T),
                                      value = c(min(diamonds$carat, na.rm = T),max(diamonds$carat, na.rm = T)),
                                      step = 0.1
                             
                          )),
                        # Output plots
                        mainPanel(
                          fluidPage(plotlyOutput("carat_cut_price_plot")))
                        
                       
                      ),
                      h3("Table by Cut & Carat"),
                      DT::dataTableOutput("table_by_carat_cut"
                      
                      )
             )
             
             )
             )

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

summary_by_cut <- diamonds %>% group_by(cut) %>% summarise(count = n(), 
                                                           avg_carat = round(mean(carat),2),
                                                           min_carat = min(carat),
                                                           max_carat = max(carat),
                                                           mode_color = mode(color),
                                                           mode_clarity = mode(clarity),
                                                           avg_depth = round(mean(depth),2),
                                                           avg_table = round(mean(table),2),
                                                           avg_price = round(mean(price),2),
                                                           min_price = min(price),
                                                           max_price = max(price))

summary_by_carat <- diamonds %>% group_by(carat) %>% summarise(count = n(),
                                                               mode_cut = mode(cut),
                                                               mode_color = mode(color),
                                                               mode_clarity = mode(clarity),
                                                               avg_depth = round(mean(depth),2),
                                                               avg_table = round(mean(table),2),
                                                               avg_price = round(mean(price),2),
                                                               min_price = min(price),
                                                               max_price = max(price))
                                                               
summary_carat_cut <- diamonds %>% group_by(cut, carat) %>% summarise(count = n(),
                                                                     mode_color = mode(color),
                                                                     mode_clarity = mode(clarity),
                                                                     avg_depth = round(mean(depth),2),
                                                                     avg_table = round(mean(table),2),
                                                                     avg_price = round(mean(price),2),
                                                                     min_price = min(price),
                                                                     max_price = max(price))

# Define server logic
server <- function(input, output) {
  output$cut_count_plot <- renderPlotly({
    dat <- subset(summary_by_cut, cut %in% input$cut_select)
    ggplot(data = dat, aes(x = cut, y = count, fill = cut)) + geom_histogram(stat = "identity") +
      ggtitle("Diamond Count by Cut")
  })
  output$cut_carat_plot <- renderPlotly({
    dat <- subset(diamonds, cut %in% input$cut_select)
    ggplot(data = dat, aes(x = cut, y = carat, fill = cut)) + geom_violin() + 
      ggtitle("Diamond Carat by Cut")
  })
  output$carat_cut_price_plot <- renderPlotly({
    dat <- subset(diamonds, cut %in% input$cut_select & carat >= input$carat_select[1] & carat <= input$carat_select[2]) %>% 
      sample_n(500) #took a 500 point sample, so only a representation of diamond population
    ggplot(data = dat, aes(x = price, y = carat, fill = cut)) + geom_point() + 
      ggtitle("Diamond Price by Cut and Carat (Sample = 500)")
  })
  output$table_by_cut <- DT::renderDataTable({
    subset(summary_by_cut,
           select = c(cut, count, avg_carat, min_carat, max_carat, mode_color, mode_clarity, avg_depth, 
                      avg_table, avg_price, min_price, max_price))
  })
  output$table_by_carat <- DT::renderDataTable({
    subset(summary_by_carat, 
           select = c(carat, count, mode_cut, mode_color, mode_clarity, avg_depth, 
                      avg_table, avg_price, min_price, max_price))
  })
  output$table_by_carat_cut <- DT::renderDataTable({
    subset(summary_carat_cut,
           select = c(cut, carat, count, mode_color, mode_clarity, avg_depth, 
                      avg_table, avg_price, min_price, max_price))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)