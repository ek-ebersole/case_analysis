library(shiny)
library(shinythemes)
library(rpivotTable)
library(tidytext)
library(dplyr)
library(tidyr)
library(widyr)
library(ggplot2)
library(igraph)
library(ggraph)

# Load data
cases <- read.csv("C:/Users/keber/Dropbox/case_metadata.csv")
my_stopwords <- read.csv("C:/Users/keber/Dropbox/my_stopwords.csv")

# Prepare data
case_sum <- data.frame(id = cases$org_id, state = cases$court, summary = cases$summary)
case_sum <- data.frame(lapply(case_sum, as.character), stringsAsFactors = FALSE)
case_sum_cln <- case_sum %>% unnest_tokens(word, summary) %>% anti_join(stop_words)
case_sum_cln <- case_sum_cln %>% filter(!word %in% my_stopwords$stp_wrds)
case_sum_cln %>% count(word, sort = TRUE)
sum_pairs <- case_sum_cln %>% pairwise_count(word, id, sort = TRUE, upper = FALSE)
sum_cors <- case_sum_cln %>% group_by(word) %>% filter(n() >=5) %>% pairwise_cor(word, id, sort = TRUE, upper = FALSE)
tf <- case_sum_cln %>% count(state, word, sort = TRUE) %>% ungroup() %>% bind_tf_idf(word, state, n) %>% arrange(desc(tf_idf)) %>% mutate(word = factor(word, levels = rev(unique(word))))

# Define UI for application 
ui <- fluidPage(
  # Apply a theme; choose from 'darkly', 'flatly', 'superhero', 'journal', 'spacelab', etc
  # theme = shinytheme("flatly"),
  themeSelector(),
  
  # Application title
  titlePanel("Various R Demos", windowTitle = "Kyle's R Demo"),
   
  sidebarLayout(
    sidebarPanel(
      h2("A Shiny App"),
      p("This is a trial app using DC, Maryland, and Virginia court cases as data for trying new things in R."),
      br(),
      p("I have three main goals that I've been using Safari references for lately:"),
      p("  1. Transition to working in notebooks"),
      p("  2. Revisit some basic text analysis"),
      p("  3. Integrate interactive visualizations"),
      p("This particular exercise mostly addresses the last two, although I do have a notebook or two with related work."),
      br(),
      h3("References:"),
      p("* Julia Silge & David Robinson,", em("Text Mining with R,"), "O'Reilly, June 2017"),
      p("* Carson Sievert,", em("Interactive Data Visualization on the Web Using R,"), "O'Reilly, December 2017"),
      p("* Yihui Xie, J. J. Allaire & Garret Grolemund,", em("R Markdown: The Definitive Guide,"), "March 2019"),
      p("* Chris Beeley & Shitalkumar Sukhdeve,", em("Web Application Development with R Using Shiny,"), "Packt, September 2018"),
      br(),
      p("With luck, this is also on my github at:"), tags$u("https://github.com/ek-ebersole/case_analysis")
    ),
  
  
  # Show three tabs, one each for pivot, a graph, and a chart
  mainPanel(
    tabsetPanel(
        tabPanel("Pivot", rpivotTableOutput("myPivot")),
        tabPanel("Word Correlation", 
                 fluidRow(plotOutput("Corr"), 
                          wellPanel(numericInput(inputId = "n", "Correlation: Enter a value from 0  to 1", 
                                                 value = 0.5, min = 0, max = 1, step = 0.05)))),
        tabPanel("TF-IDF", 
                fluidRow(plotOutput("TFIDFplot"),
                         wellPanel(numericInput(inputId = "n1", "Enter number of Words to display",
                                                value = 10, min = 1, max = 100, step = 1))
      )
   )
))))

# Define server logic 
server <- function(input, output) {
  
  # Tab 1 pivot table
  output$myPivot <- renderRpivotTable({
      rpivotTable(data = cases)
   })
   
  # Tab 2 word correlation graph
  theData <- reactive({
    sum_cors %>% filter(correlation >= input$n)
    })
  
    output$Corr <- renderPlot({
      theData() %>% graph_from_data_frame() %>% ggraph(layout = "fr") + 
        geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_color = "cyan4") +
        geom_node_point(size = 5) + 
        geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
        theme_void()
    })
 
   # Tab 3 tf-idf graphs
   theTFdata <- reactive({
     tf_plot <- tf %>% group_by(state) %>% top_n(input$n1, tf_idf) %>%  ungroup() %>% mutate(word = reorder(word, tf_idf))
   })
   
 output$TFIDFplot <- renderPlot({
   theTFdata() %>% ggplot(aes(word, tf_idf, fill = state)) + geom_col(show.legend = FALSE) + 
     facet_wrap(~state, ncol = 3, scales = "free") + coord_flip()
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

