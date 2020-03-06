library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  title = "Number of clinical trials started on biosimilars",
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "vedha_space.png"),
  ),
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    tags$div(
      style = "position: fixed;top: 250px; left: 0px; width: 100%;
      padding: 5px 0px 5px 0px; text-align: center; font-weight: bold;
      font-size: 300%; color: #ffffff; background-color:'transparent'; z-index: 105;",
      tags$img(src = "loading_icon.svg", height = "200px", width = "200px")
    )
  ),
  fluidRow(
    column(
      12,
      dateRangeInput(
        "filter_dates", label = "Filter Dates",
        start = "2015-01-01", end = "2020-03-01",
        min = "2010-01-01", max = "2020-03-01"
      )
    ),
    column(12, tags$a("Get the code and data", href = "https://github.com/vedhav/biosimilar_plot")),
    column(12, plotlyOutput("plot", height = "700px"))
  )
)

server <- function(input, output, session) {
  ct_data <- read_csv("ct_data.csv")
  approved_data <- read_csv("approved_data.csv")
  biosimilar_reference_generics_colors <- setNames(
    c("#B01D59", "#7F519A", "#EC2826", "#378C8C", "#6DC343", "#F8D352", "#2E6CB5", "#F3762E", "#1C3E8C"),
    c("adalimumab", "bevacizumab", "epoetin-alfa", "etanercept", "filgrastim",
      "infliximab", "pegfilgrastim", "rituximab", "trastuzumab")
  )
  observeEvent(input$filter_dates, {
    plot_ct_data <- ct_data %>% filter(month_year >= input$filter_dates[1] & month_year <= input$filter_dates[2])
    plot_approved_data <- approved_data %>% filter(month_year >= input$filter_dates[1] & month_year <= input$filter_dates[2])
    output$plot <- renderPlotly({
      plot_ly(
        plot_ct_data %>% arrange(month_year),
        x = ~month_year,
        y = ~ct_studies,
        color = ~reference_generic,
        colors = biosimilar_reference_generics_colors,
        type = 'scatter', mode = "lines",
        legendgroup = ~reference_generic,
        hoverinfo = 'text',
        text = ~hover_text
      ) %>%
        add_trace(
          data = plot_approved_data,
          x = ~month_year,
          y = ~ct_studies, marker = list(size = 20),
          type = 'scatter', mode = "markers",
          hoverinfo = 'text', text = ~hover_text,
          legendgroup = ~reference_generic, showlegend = FALSE
        ) %>%
        layout(
          xaxis = list(title = "Month", showgrid = FALSE),
          yaxis = list(title = "Number of CT studies", showgrid = FALSE)
        )
    })
  })
}

shinyApp(ui, server)