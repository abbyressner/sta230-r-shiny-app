
library(shiny)
library(ggplot2)
library(tidyr)
library(plotly)
library(leaflet)
library(bslib)
library(dplyr)

#data = read.csv("cleaned_data.csv")
inst_data = read.csv("cleaned_inst_data.csv")

######### STYLE #########



########## UI ###########
ui <- fluidPage(
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectizeInput("selectschools", 
                     "Select school(s):", 
                     choices = inst_data$name,
                     selected = c("Grinnell College", 
                                  "University of Iowa", 
                                  "Drake University"),
                     multiple = TRUE,
                     options = list(placeholder = "Type to search...",
                                    onInitialize = I('function() { this.setValue(""); }'),
                                    maxOptions = 5)
                     ),
      selectInput(),
      actionButton("school_changes", 
                   "Show Changes",
                   class = "btn-success")),
    mainPanel(
      plotOutput('plot')
    )
  )
)

# set up server function
server <- function(input, output, session){
  
  selectedSchools = reactive({
    if (input$school_changes == 0 || length(input$selectschools) == 0) {
      inst_data %>% 
        filter(name %in% c("Grinnell College", 
                           "University of Iowa", 
                           "Drake University"))
      } else {
        inst_data %>% 
          filter(name %in% input$selectschools)
        }
    })
  
  
  output$plot <- renderPlot({

    selected = isolate(selectedSchools())
    
    input$school_changes

    ggplot(selected, aes(x = name, y = adm_rate)) +
      geom_bar(stat = "identity") +
      labs(x = "Schools", y = "Acceptance Rate") +
      theme_minimal()
  })
  
  updateSelectizeInput(session, "selectschools", choices = inst_data$name, server = TRUE)
}

options(shiny.launch.browser = TRUE)

shinyApp(ui, server)
