# UK Transport Zero-Emission 2040 — Shiny app
# MSc Data Analytics project, London Metropolitan University (2022)
# Author: Eduard de Jesús Romero Mercedes
#
# All data cleaning happens in prepare_data.R, which writes data/app_data.rds.
# The app only loads that file, so it starts fast and does no heavy work
# while the user interacts with it.

library(shiny)
library(shinyWidgets)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

d <- readRDS("data/app_data.rds")

years      <- seq(2012L, max(d$charge_point$year))
last_co2   <- max(d$co2$year)
year_date  <- function(y) as.Date(paste0(y, "-01-01"))

# Colour scale for CO2, fixed across years so colours are comparable
co2_bins <- unique(c(round(quantile(d$co2$mean_co2, probs = seq(0, 1, 0.2))), Inf))
co2_pal  <- colorBin("Blues", domain = d$co2$mean_co2, bins = co2_bins)

# Datasets offered in the "Plots" tab
tab_data <- list(
  "Electricity"      = d$electricity_tab,
  "Electric Vehicle" = d$vehicles_tab
)

panel_theme <- theme_bw(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank())

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- bootstrapPage(
  tags$head(includeCSS("estilos_.css")),
  navbarPage(
    "UK Transport Zero-emission 2040",
    tabPanel(
      "Map",
      div(
        class = "outer",
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          top = 75, left = 55, width = 450, fixed = TRUE,
          draggable = TRUE, height = "auto",
          sliderTextInput(
            "year", label = h5("Select the year"),
            choices = years, selected = max(years), grid = FALSE,
            animate = animationOptions(interval = 3000, loop = FALSE)
          ),
          plotOutput("plot_cp",          height = "130px"),
          plotOutput("plot_co2",         height = "230px"),
          plotOutput("plot_vehicles",    height = "230px"),
          plotOutput("plot_electricity", height = "230px")
        )
      )
    ),
    tabPanel(
      "Plots",
      selectInput("data", "Data", choices = names(tab_data)),
      selectInput("territory", "Territory", choices = NULL),
      selectInput("period", "Period", choices = NULL),
      tableOutput("table"),
      plotOutput("plot_tab")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  year     <- reactive({
    req(input$year)
    as.integer(input$year)
  })
  cut_date <- reactive(year_date(year()))

  # ---- Map -----------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = -2.5, lat = 54.5, zoom = 6) |>
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Charge points", "CO2 emission"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend("bottomright", pal = co2_pal, values = d$co2$mean_co2,
                title = "<small>Average transport CO2<br/>per local authority (kt CO2e)</small>")
  })

  observe({
    # Emissions data end in last_co2; later years show the latest available
    co2_year <- min(year(), last_co2)
    co2_now  <- d$uk |>
      inner_join(filter(d$co2, year == co2_year), by = "country")
    cp_now   <- filter(d$charge_point, year <= year())

    leafletProxy("map") |>
      clearGroup("CO2 emission") |>
      clearGroup("Charge points") |>
      addPolygons(
        data = co2_now, group = "CO2 emission",
        fillColor = ~co2_pal(mean_co2), fillOpacity = 0.5,
        stroke = TRUE, color = "white", weight = 1, smoothFactor = 0.5,
        label = ~sprintf("%s (%d): %.1f kt CO2e", country, year, mean_co2)
      ) |>
      addMarkers(
        data = cp_now, group = "Charge points",
        lat = ~latitude, lng = ~longitude,
        label = lapply(cp_now$label, htmltools::HTML),
        clusterOptions = markerClusterOptions()
      )
  })

  # ---- Floating panel plots ------------------------------------------------
  output$plot_cp <- renderPlot({
    d$charge_point_cum |>
      filter(year <= year()) |>
      ggplot(aes(year, total)) +
      geom_line(colour = "blue") +
      geom_point(size = 1, colour = "blue") +
      labs(x = NULL, y = "Total", title = "Charge points (cumulative)") +
      panel_theme
  })

  output$plot_co2 <- renderPlot({
    d$co2 |>
      filter(year <= year()) |>
      ggplot(aes(year, mean_co2, colour = country)) +
      geom_line() +
      geom_point(size = 1) +
      labs(x = NULL, y = "kt CO2e",
           title = "Transport CO2 emission (mean per local authority)") +
      panel_theme
  })

  output$plot_vehicles <- renderPlot({
    d$vehicles_gb |>
      filter(date <= cut_date(), total > 0) |>
      ggplot(aes(date, total, colour = types)) +
      geom_line() +
      geom_point(size = 1) +
      scale_y_log10() +
      labs(x = NULL, y = "Vehicles (log scale)",
           title = "Electric vehicles licensed in Great Britain") +
      panel_theme
  })

  output$plot_electricity <- renderPlot({
    d$electricity_gen |>
      filter(date <= cut_date()) |>
      ggplot(aes(date, gwh, colour = source)) +
      geom_line() +
      labs(x = NULL, y = "GWh", title = "Electricity generated by source") +
      panel_theme
  })

  # ---- Plots tab -----------------------------------------------------------
  selected_data <- reactive({
    req(input$data)
    tab_data[[input$data]]
  })

  observeEvent(selected_data(), {
    updateSelectInput(session, "territory",
                      choices = sort(unique(selected_data()$territory)))
  })

  by_territory <- reactive({
    req(input$territory)
    filter(selected_data(), territory == input$territory)
  })

  observeEvent(by_territory(), {
    updateSelectInput(session, "period", choices = unique(by_territory()$period))
  })

  by_period <- reactive({
    req(input$period)
    filter(by_territory(), period == input$period)
  })

  output$table <- renderTable({
    by_period() |>
      mutate(date = format(date, "%Y-%m-%d")) |>
      head(10)
  })

  output$plot_tab <- renderPlot({
    req(nrow(by_period()) > 0)
    if (input$data == "Electricity") {
      ggplot(by_period(), aes(date, total_consumption_gwh)) +
        geom_line(colour = "blue") +
        geom_point(colour = "blue") +
        labs(x = NULL, y = "Electricity consumed (GWh)", title = "Electricity") +
        theme_bw()
    } else {
      by_period() |>
        filter(total > 0) |>
        ggplot(aes(date, total, colour = types)) +
        geom_line() +
        geom_point() +
        scale_y_log10() +
        facet_wrap(~types, scales = "free") +
        labs(x = NULL, y = "Electric vehicles (log scale)", title = "Electric vehicles") +
        theme_bw() +
        theme(legend.position = "none")
    }
  })
}

shinyApp(ui, server)
