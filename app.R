library(shiny)
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)
library(janitor)
library(stringr)

aus_wines_wide <- readr::read_csv("AustralianWines.csv", show_col_types = FALSE) %>%
  clean_names()

aus_wines_ts <- aus_wines_wide %>%
  mutate(parsed_date = parse_date_time(month, orders = "b-y")) %>%
  mutate(across(-c(month, parsed_date), ~ suppressWarnings(as.numeric(.)))) %>%
  pivot_longer(
    cols      = -c(month, parsed_date),
    names_to  = "Varietal",
    values_to = "Sales"
  ) %>%
  mutate(
    Varietal = Varietal %>% str_replace_all("_", " ") %>% str_squish() %>% str_to_title(),
    Month = yearmonth(parsed_date),
    Date  = as.Date(parsed_date)
  ) %>%
  select(Month, Date, Varietal, Sales) %>%
  as_tsibble(index = Month, key = Varietal)

varietal_choices <- aus_wines_ts %>% distinct(Varietal) %>% arrange(Varietal) %>% pull(Varietal)

min_date <- min(aus_wines_ts$Date, na.rm = TRUE)
max_date <- max(aus_wines_ts$Date, na.rm = TRUE)

ui <- navbarPage(
  title = "australian wine forecast explorer",
  
  tabPanel(
    "Forecasts",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        selectInput(
          "varietals",
          "select varietal(s):",
          choices  = varietal_choices,
          selected = varietal_choices[1:2],
          multiple = TRUE
        ),
        
        dateRangeInput(
          "date_range",
          "date range:",
          start  = min_date,
          end    = max_date,
          min    = min_date,
          max    = max_date,
          format = "yyyy-mm"
        ),
        
        sliderInput(
          "train_end",
          "training end:",
          min   = min_date,
          max   = max_date,
          value = max_date - years(3),
          timeFormat = "%Y-%m"
        ),
        
        numericInput(
          "horizon",
          "forecast horizon (months):",
          value = 12,
          min   = 1,
          max   = 60
        )
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "main_tabs",
          
          tabPanel(
            "Overview",
            br(),
            plotOutput("overview_plot", height = "350px")
          ),
          
          tabPanel(
            "Forecasts",
            br(),
            plotOutput("forecast_plot", height = "400px"),
            br(),
            tableOutput("metrics_table")
          ),
          
          tabPanel(
            "Model Specs",
            br(),
            tableOutput("specs_table")
          ),
          
          tabPanel(
            "Seasonality",
            br(),
            plotOutput("season_plot", height = "400px")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "About",
    fluidPage(
      br(),
      h3("about this app"),
      p("interactive forecasting for australian wine by varietal.")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$date_range, {
    dr <- input$date_range
    if (input$train_end < dr[1] || input$train_end > dr[2]) {
      updateSliderInput(session, "train_end", value = dr[2] - years(1))
    }
  })
  
  filtered_ts <- reactive({
    req(input$varietals, input$date_range)
    aus_wines_ts %>%
      filter(
        Varietal %in% input$varietals,
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      )
  })
  
  training_ts <- reactive({
    req(filtered_ts())
    train_end_ym <- yearmonth(input$train_end)
    filtered_ts() %>% filter(Month <= train_end_ym)
  })
  
  validation_ts <- reactive({
    req(filtered_ts())
    train_end_ym <- yearmonth(input$train_end)
    filtered_ts() %>% filter(Month > train_end_ym)
  })
  
  models_fit <- reactive({
    req(training_ts())
    training_ts() %>%
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  forecasts_plot <- reactive({
    req(models_fit())
    models_fit() %>% forecast(h = input$horizon)
  })
  
  forecasts_valid <- reactive({
    req(models_fit(), validation_ts())
    n_valid <- nrow(validation_ts())
    if (n_valid <= 0) return(NULL)
    models_fit() %>% forecast(h = n_valid)
  })
  
  output$overview_plot <- renderPlot({
    dat <- filtered_ts()
    req(nrow(dat) > 0)
    autoplot(dat, Sales) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      theme_minimal()
  })
  
  output$forecast_plot <- renderPlot({
    dat <- filtered_ts()
    fc  <- forecasts_plot()
    req(nrow(dat) > 0)
    autoplot(fc, dat) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      theme_minimal()
  })
  
  output$season_plot <- renderPlot({
    dat <- filtered_ts()
    req(nrow(dat) > 0)
    dat %>%
      gg_subseries(Sales) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      theme_minimal()
  })
  
  output$metrics_table <- renderTable({
    req(models_fit())
    train_dat <- training_ts()
    valid_dat <- validation_ts()
    validate(need(nrow(valid_dat) > 0, "validation window empty"))
    
    acc_train <- accuracy(models_fit(), train_dat)
    fc_valid  <- forecasts_valid()
    req(fc_valid)
    acc_valid <- accuracy(fc_valid, valid_dat)
    
    bind_rows(acc_train, acc_valid) %>%
      select(.model, Varietal, .type, RMSE, MAE, MAPE)
  })
  
  output$specs_table <- renderTable({
    req(models_fit())
    specs <- models_fit() %>% glance()
    
    has_arima_cols <- all(c("p","d","q","P","D","Q","m") %in% names(specs))
    if (has_arima_cols) {
      specs <- specs %>%
        mutate(
          arima_order = ifelse(
            .model == "ARIMA",
            paste0(
              "ARIMA(",
              p, ",", d, ",", q, ")(",
              P, ",", D, ",", Q, ")[", m, "]"
            ),
            NA_character_
          )
        )
    } else specs$arima_order <- NA_character_
    
    specs <- specs %>%
      mutate(
        ets_form = ifelse(.model == "ETS", "ETS (auto)", NA_character_)
      )
    
    specs %>%
      select(.model, Varietal, ets_form, arima_order, AIC, BIC)
  })
}

shinyApp(ui = ui, server = server)