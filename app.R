library(shiny)
library(ggplot2)
library(tidyr)
library(plotly)
library(bslib)
library(dplyr)

data <- read.csv("cleaned_data.csv")

default_sel_large <- c(
  "Grinnell College",
  "University of Iowa",
  "Drake University",
  "Iowa State University",
  "Coe College",
  "University of Wisconsin-Madison",
  "Santa Clara University",
  "Wesleyan University",
  "George Washington University",
  "Georgia Institute of Technology-Main Campus",
  "University of Minnesota-Twin Cities",
  "Oberlin College",
  "Fordham University",
  "Stony Brook University",
  "Colorado School of Mines",
  "University of Florida",
  "University of Miami",
  "Binghamton University",
  "Lafayette College",
  "Rose-Hulman Institute of Technology",
  "The University of Texas at Austin",
  "University of Massachusetts-Amherst",
  "North Carolina State University at Raleigh",
  "University of Connecticut",
  "University of Pittsburgh-Pittsburgh Campus",
  "Pepperdine University",
  "University of Georgia",
  "University of Denver",
  "Ohio State University-Main Campus",
  "University of Utah"
)

default_sel <- c(
  "Grinnell College",
  "Drake University",
  "Coe College",
  "Washington University in St Louis",
  "Occidental College"
)

shared_sidebar <- sidebar(
  selectizeInput(
    "selectschools",
    "Select school(s):",
    choices = data$name,
    selected = default_sel,
    multiple = TRUE,
    options = list(
      placeholder = "Type to search...",
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  numericInput("user_sat",
    "Your SAT Score:",
    value = 1240,
    min = 900,
    max = 1600,
    step = 10
  ),
  numericInput("user_act",
    "Your ACT Score:",
    value = 27,
    min = 18,
    max = 36
  ),
  actionButton("school_changes", "Show Changes", class = "btn-success")
)

###### UI ######
ui <- page_navbar(
  title = "College Scorecard Data Visualizer",
  theme = bs_theme(
    bootswatch = "litera",
    "h1-font-size" = "2.5rem"
  ),
  nav_panel(
    "Admissions",
    layout_sidebar(
      sidebar = shared_sidebar,
      layout_columns(
        card(
          card_header("Acceptance Rate"),
          card_body(plotlyOutput("barchart"))
        ),
        layout_columns(
          card(
            card_header("SAT Scores"),
            card_body(plotlyOutput("dumbbell_sat"))
          ),
          card(
            card_header("ACT Scores"),
            card_body(plotlyOutput("dumbbell_act"))
          ),
          col_widths = c(12, 12)
        )
      )
    )
  ),
  nav_panel(
    "Cost",
    layout_sidebar(
      sidebar = shared_sidebar,
      card(
        card_header("Net Price by Income Bracket"),
        card_body(plotlyOutput("income_cost_bar"))
      )
    )
  ),
  nav_panel(
    "Size/Selectivity",
    layout_sidebar(
      sidebar = shared_sidebar,
      navset_card_underline(
        nav("Plot", plotlyOutput("top_equit_schools")),
        nav("Table", dataTableOutput("equity_table")),
        title = "College Accessibility vs. Size and Selectivity"
      )
    )
  )
)


server <- function(input, output, session) {
  selectedSchools <- reactive({
    selection <- input$selectschools
    if (is.null(selection) || length(selection) == 0) {
      selection <- default_sel
    }
    data %>%
      filter(name %in% selection)
  })

  # bar chart (ggplot)
  output$barchart <- renderPlotly({
    input$school_changes

    selected <- isolate(selectedSchools()) %>%
      mutate(
        namesbyadm_rate = reorder(name, adm_rate),
        hover_text = paste0(
          name,
          "<br>Acceptance Rate: ",
          round(adm_rate * 100),
          "%"
        )
      )

    p <- ggplot(selected, aes(x = namesbyadm_rate, y = adm_rate, text = hover_text)) +
      geom_bar(stat = "identity", fill = alpha("#648fff", 0.7), color = "#648fff") +
      geom_text(aes(label = paste0(round(adm_rate * 100), "%")),
        nudge_y = 0
      ) +
      labs(x = "Schools", y = "Acceptance Rate") +
      theme_minimal() +
      theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))

    ggplotly(p, tooltip = "text")
  })

  # dumbbell-ish plot SAT (ggplot -> plotly)
  output$dumbbell_sat <- renderPlotly({
    input$school_changes

    selected <- isolate(selectedSchools()) %>%
      mutate(namesbyscores_desc = reorder(name, sat_p75))

    p <- ggplot(selected, aes(y = namesbyscores_desc)) +
      geom_segment(aes(x = sat_p25, xend = sat_p75, yend = namesbyscores_desc),
        color = alpha("#343a40", 0.2),
        linewidth = 5
      ) +
      geom_point(aes(x = sat_p25), color = "#00b6b9", size = 5) +
      geom_point(aes(x = sat_p75), color = "#648fff", size = 5) +
      geom_point(aes(x = sat_mid50), color = "#785ef0", size = 5) +
      geom_vline(
        xintercept = input$user_sat,
        linetype = "dashed",
        color = "slategrey",
        linewidth = 1
      ) +
      scale_x_continuous(limits = c(900, 1600), name = "Composite SAT Score") +
      labs(y = "") +
      theme_minimal() +
      theme(axis.text.y = element_text(face = "bold"))

    ggplotly(p) # converts ggplot to plotly
  })

  # dumbbell-ish plot ACT (ggplot -> plotly)
  output$dumbbell_act <- renderPlotly({
    input$school_changes

    selected <- isolate(selectedSchools()) %>%
      mutate(namesbyscores_desc = reorder(name, act_p75))

    p <- ggplot(selected, aes(y = namesbyscores_desc)) +
      geom_segment(aes(x = act_p25, xend = act_p75, yend = namesbyscores_desc),
        color = alpha("#343a40", 0.2),
        linewidth = 5
      ) +
      geom_point(aes(x = act_p25), color = "#00b6b9", size = 5) +
      geom_point(aes(x = act_p75), color = "#648fff", size = 5) +
      geom_point(aes(x = act_mid50), color = "#785ef0", size = 5) +
      geom_vline(
        xintercept = input$user_act,
        linetype = "dashed",
        color = "slategrey",
        linewidth = 1
      ) +
      scale_x_continuous(limits = c(16, 36), name = "ACT Score") +
      labs(y = "") +
      theme_minimal()

    ggplotly(p) # converts ggplot to plotly
  })


  output$top_equit_schools <- renderPlotly({
    input$school_changes

    selected <- isolate(selectedSchools()) %>%
      filter(!is.na(ratio), !is.na(adm_rate), !is.na(tuit_fee_out))

    plot_ly(
      data = selected,
      type = "scatter3d",
      mode = "markers",
      x = ~sat_avg,
      y = ~adm_rate,
      z = ~ug_enrollment,
      color = ~state,
      text = ~ paste(
        "School:", name,
        "<br>Average SAT:", sat_avg,
        "<br>Admit Rate:", round(adm_rate, 2),
        "<br># of undergrads: ", ug_enrollment
      ),
      hoverinfo = "text",
      marker = list(size = 5, opacity = 0.7)
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Average SAT Score"),
          yaxis = list(title = "Acceptance Rate"),
          zaxis = list(title = "Undergrad Enrollment")
        )
      )
  })

  output$income_cost_bar <- renderPlotly({
    input$school_changes

    selected <- isolate(selectedSchools())

    income_costs <- selected %>%
      select(name,
        `0-30k` = cost_avg_income_0_30k,
        `30-48k` = cost_avg_income_30_48k,
        `48-75k` = cost_avg_income_48_75k,
        `75-110k` = cost_avg_income_75_110k,
        `110k+` = cost_avg_income_110k_plus
      ) %>%
      pivot_longer(-name, names_to = "IncomeBracket", values_to = "Cost") %>%
      mutate(IncomeBracket = factor(IncomeBracket,
        levels = c("0-30k", "30-48k", "48-75k", "75-110k", "110k+")
      ))

    p <- ggplot(income_costs, aes(x = name, y = Cost, fill = IncomeBracket)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "0-30k" = "#edf8fb",
        "30-48k" = "#b2e2e2",
        "48-75k" = "#66c2a4",
        "75-110k" = "#2ca25f",
        "110k+" = "#006d2c"
      )) +
      labs(x = "School", y = "Average Net Price", fill = "Income Bracket") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
    ggplotly(p)
  })

  output$equity_table <- renderDataTable({
    selectedSchools() %>%
      filter(!is.na(ratio), !is.na(adm_rate), !is.na(tuit_fee_out)) %>%
      select(name, state, city, adm_rate, sat_avg, ug_enrollment, ratio) %>%
      arrange(desc(ratio))
  })

  updateSelectizeInput(
    session,
    "selectschools",
    choices = data$name,
    server = TRUE
  )
}

options(shiny.launch.browser = TRUE)

shinyApp(ui, server)
