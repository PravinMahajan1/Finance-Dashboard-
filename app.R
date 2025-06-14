# Required libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(readr)
library(scales)
library(shinythemes)
library(bslib)
library(forecast)  # For enhanced forecasting

# Enhanced data processing function
process_expenses <- function(data) {
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  
  data <- data %>%
    mutate(Category = case_when(
      tolower(Category) %in% c("food", "groceries", "dining", "restaurant", "takeout") ~ "Food & Dining",
      tolower(Category) %in% c("rent", "mortgage", "utilities", "electricity", "water", "gas") ~ "Housing",
      tolower(Category) %in% c("transport", "gas", "bus", "uber", "taxi", "car") ~ "Transportation",
      tolower(Category) %in% c("entertainment", "movies", "games", "streaming", "music") ~ "Entertainment",
      tolower(Category) %in% c("health", "medical", "doctor", "pharmacy", "insurance") ~ "Healthcare",
      tolower(Category) %in% c("shopping", "clothes", "electronics", "amazon") ~ "Shopping",
      tolower(Category) %in% c("education", "books", "courses", "training") ~ "Education",
      tolower(Category) %in% c("savings", "investment", "retirement") ~ "Savings",
      TRUE ~ "Other"
    )) %>%
    mutate(
      Month = floor_date(Date, "month"),
      Week = floor_date(Date, "week"),
      Day = floor_date(Date, "day"),
      Weekday = wday(Date, label = TRUE),
      IsWeekend = Weekday %in% c("Sat", "Sun")
    )
  
  daily_summary <- data %>%
    group_by(Day, Category) %>%
    summarise(Total = sum(Amount, na.rm = TRUE), Count = n(), .groups = "drop")
  
  monthly_summary <- data %>%
    group_by(Month, Category) %>%
    summarise(Total = sum(Amount, na.rm = TRUE), Count = n(), AvgTransaction = mean(Amount, na.rm = TRUE), .groups = "drop")
  
  monthly_totals <- data %>%
    group_by(Month) %>%
    summarise(
      Total_Expenses = sum(Amount, na.rm = TRUE),
      Transaction_Count = n(),
      Avg_Daily_Spending = Total_Expenses / days_in_month(Month),
      .groups = "drop"
    )
  
  monthly_income <- 100000  # Default in INR
  monthly_totals$Budget_Used <- (monthly_totals$Total_Expenses / monthly_income) * 100
  monthly_totals$Savings <- monthly_income - monthly_totals$Total_Expenses
  monthly_totals$Savings_Rate <- (monthly_totals$Savings / monthly_income) * 100
  
  return(list(
    data = data,
    daily_summary = daily_summary,
    monthly_summary = monthly_summary,
    monthly_totals = monthly_totals,
    categories = unique(data$Category)
  ))
}

# Enhanced forecasting with ARIMA
forecast_savings <- function(monthly_totals, periods = 6) {
  if (nrow(monthly_totals) < 12) return(NULL)
  
  ts_data <- ts(monthly_totals$Savings, frequency = 12)
  arima_model <- auto.arima(ts_data)
  forecast_result <- forecast(arima_model, h = periods, level = c(80, 95))
  
  future_months <- seq(max(monthly_totals$Month) %m+% months(1), by = "month", length.out = periods)
  
  forecast_data <- data.frame(
    Month = future_months,
    Savings = as.numeric(forecast_result$mean),
    Lower_CI_80 = as.numeric(forecast_result$lower[,1]),
    Upper_CI_80 = as.numeric(forecast_result$upper[,1]),
    Lower_CI_95 = as.numeric(forecast_result$lower[,2]),
    Upper_CI_95 = as.numeric(forecast_result$upper[,2]),
    Type = "Forecast"
  )
  
  historical_data <- monthly_totals %>%
    select(Month, Savings) %>%
    mutate(
      Lower_CI_80 = Savings,
      Upper_CI_80 = Savings,
      Lower_CI_95 = Savings,
      Upper_CI_95 = Savings,
      Type = "Historical"
    )
  
  return(bind_rows(historical_data, forecast_data))
}

# Custom CSS
custom_css <- "
  .main-header .navbar { background-color: #2c3e50 !important; }
  .skin-blue .main-header .navbar .nav > li > a { color: #fff !important; }
  .content-wrapper { background-color: #f8f9fa !important; }
  .box { border-radius: 8px !important; box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important; border: none !important; }
  .info-box { border-radius: 8px !important; box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important; }
  .small-box { border-radius: 8px !important; box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important; }
  .btn-primary { background-color: #3498db !important; border-color: #3498db !important; border-radius: 6px !important; }
  .btn-success { background-color: #27ae60 !important; border-color: #27ae60 !important; border-radius: 6px !important; }
  .progress-bar { background-color: #3498db !important; }
  .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #3498db !important; }
"

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "FinanceTracker Pro",
    dropdownMenu(
      type = "notifications",
      headerText = "Profile",
      icon = icon("user"),
      menuItem("Profile Settings", tabName = "profile", icon = icon("user-cog")),
      menuItem("Clear Profile", icon = icon("sign-out-alt"), href = "#")
    )
  ),
  
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Category Charts", tabName = "category_charts", icon = icon("chart-pie")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("crystal-ball")),
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Profile", tabName = "profile", icon = icon("user"))
    ),
    div(
      style = "padding: 15px;",
      h4("Data Upload", style = "color: #fff; margin-bottom: 10px;"),
      fileInput("file", "Upload CSV File", accept = ".csv")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_expenses", width = 3),
          valueBoxOutput("monthly_avg", width = 3),
          valueBoxOutput("savings_rate", width = 3),
          valueBoxOutput("budget_status", width = 3)
        ),
        fluidRow(
          box(
            title = "Daily Expenses", status = "primary", solidHeader = TRUE,
            width = 8, height = 400,
            withSpinner(plotlyOutput("daily_expenses"))
          ),
          box(
            title = "Quick Insights", status = "info", solidHeader = TRUE,
            width = 4, height = 400,
            div(
              style = "padding: 10px;",
              h4("Top Spending Categories"),
              tableOutput("top_categories"),
              br(),
              h4("Spending Patterns"),
              textOutput("spending_insights")
            )
          )
        )
      ),
      tabItem(
        tabName = "analytics",
        fluidRow(
          box(
            title = "Filters & Controls", status = "primary", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3, selectInput("category_filter", "Category:", choices = NULL)),
              column(3, dateRangeInput("date_range", "Date Range:", start = Sys.Date() - months(6), end = Sys.Date())),
              column(3, numericInput("monthly_income", "Monthly Income (₹):", value = 100000, min = 0, step = 1000)),
              column(3, br(), actionButton("apply_filters", "Apply Filters", class = "btn-primary"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Category Analysis", status = "info", solidHeader = TRUE,
            width = 6, height = 450,
            withSpinner(plotlyOutput("category_analysis"))
          ),
          box(
            title = "Spending Patterns", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            withSpinner(plotlyOutput("pattern_analysis"))
          )
        )
      ),
      tabItem(
        tabName = "category_charts",
        fluidRow(
          box(
            title = "Category Distribution", status = "primary", solidHeader = TRUE,
            width = 6, height = 450,
            withSpinner(plotlyOutput("category_pie"))
          ),
          box(
            title = "Category Trends", status = "info", solidHeader = TRUE,
            width = 6, height = 450,
            withSpinner(plotlyOutput("category_trends"))
          )
        )
      ),
      tabItem(
        tabName = "forecasting",
        fluidRow(
          box(
            title = "Forecast Settings", status = "primary", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3, sliderInput("forecast_months", "Forecast Months:", min = 1, max = 24, value = 6)),
              column(3, numericInput("savings_goal", "Monthly Savings Goal (₹):", value = 20000, min = 0, step = 1000)),
              column(3, selectInput("forecast_model", "Forecast Model:", choices = c("ARIMA", "Linear", "ETS"))),
              column(3, br(), actionButton("generate_forecast", "Generate Forecast", class = "btn-success"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Savings Forecast", status = "info", solidHeader = TRUE,
            width = 8, height = 500,
            withSpinner(plotlyOutput("savings_forecast"))
          ),
          box(
            title = "Forecast Summary", status = "success", solidHeader = TRUE,
            width = 4, height = 500,
            div(
              style = "padding: 15px;",
              h4("Projected Outcomes"),
              tableOutput("forecast_summary"),
              br(),
              h4("Recommendations"),
              textOutput("recommendations")
            )
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Data Overview", status = "primary", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3, infoBoxOutput("total_transactions", width = NULL)),
              column(3, infoBoxOutput("date_range_info", width = NULL)),
              column(3, infoBoxOutput("categories_count", width = NULL)),
              column(3, infoBoxOutput("data_quality", width = NULL))
            )
          )
        ),
        fluidRow(
          box(
            title = "Transaction Data", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("full_data_table")
          )
        )
      ),
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Application Settings", status = "primary", solidHeader = TRUE,
            width = 6,
            h4("Financial Settings"),
            numericInput("default_income", "Default Monthly Income (₹):", value = 100000, min = 0, step = 1000),
            selectInput("currency", "Currency:", choices = c("INR" = "₹", "USD" = "$", "EUR" = "€", "GBP" = "£"), selected = "INR"),
            br(),
            h4("Display Settings"),
            checkboxInput("dark_mode", "Dark Mode", value = FALSE),
            checkboxInput("show_animations", "Show Animations", value = TRUE)
          ),
          box(
            title = "Data Export", status = "success", solidHeader = TRUE,
            width = 6,
            h4("Export Options"),
            downloadButton("download_summary", "Download Summary Report", class = "btn-info"),
            downloadButton("download_data", "Download Raw Data", class = "btn-warning"),
            downloadButton("download_charts", "Download Charts", class = "btn-success")
          )
        )
      ),
      tabItem(
        tabName = "profile",
        fluidRow(
          box(
            title = "User Profile", status = "primary", solidHeader = TRUE,
            width = 6,
            textInput("user_name", "Name:"),
            textInput("user_email", "Email:"),
            passwordInput("user_password", "Password:"),
            actionButton("save_profile", "Save Profile", class = "btn-success")
          ),
          box(
            title = "Profile Data", status = "info", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("profile_info")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(
    monthly_income = 100000,
    processed_data = NULL,
    user_profile = NULL
  )
  
  # Profile management (in-memory)
  observeEvent(input$save_profile, {
    values$user_profile <- list(
      name = input$user_name,
      email = input$user_email,
      password = input$user_password
    )
    showNotification("Profile saved in session!", type = "message")
  })
  
  output$profile_info <- renderPrint({
    if (is.null(values$user_profile)) {
      "No profile data available"
    } else {
      str(values$user_profile)
    }
  })
  
  # Data processing
  expense_data <- reactive({
    req(input$file)
    
    tryCatch({
      data <- read_csv(input$file$datapath, col_types = cols(
        Date = col_date(format = "%Y-%m-%d"),
        Category = col_character(),
        Amount = col_double(),
        Description = col_character()
      ))
      
      if (!all(c("Date", "Category", "Amount") %in% names(data))) {
        showNotification("CSV must contain Date, Category, and Amount columns", type = "error")
        return(NULL)
      }
      
      processed <- process_expenses(data)
      values$processed_data <- processed
      updateSelectInput(session, "category_filter", choices = c("All", processed$categories))
      
      return(processed)
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Update monthly income
  observeEvent(input$monthly_income, {
    values$monthly_income <- input$monthly_income
  })
  
  # Value Boxes
  output$total_expenses <- renderValueBox({
    data <- expense_data()
    total <- if (is.null(data)) 0 else sum(data$data$Amount, na.rm = TRUE)
    valueBox(
      value = paste0("₹", format(total, big.mark = ",")),
      subtitle = "Total Expenses",
      icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$monthly_avg <- renderValueBox({
    data <- expense_data()
    avg <- if (is.null(data)) 0 else mean(data$monthly_totals$Total_Expenses, na.rm = TRUE)
    valueBox(
      value = paste0("₹", format(round(avg), big.mark = ",")),
      subtitle = "Monthly Average",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$savings_rate <- renderValueBox({
    data <- expense_data()
    rate <- if (is.null(data)) 0 else mean(data$monthly_totals$Savings_Rate, na.rm = TRUE)
    valueBox(
      value = paste0(round(rate, 1), "%"),
      subtitle = "Avg Savings Rate",
      icon = icon("piggy-bank"),
      color = "yellow"
    )
  })
  
  output$budget_status <- renderValueBox({
    data <- expense_data()
    if (is.null(data)) {
      status <- "No Data"
      color <- "red"
    } else {
      latest_usage <- tail(data$monthly_totals$Budget_Used, 1)
      status <- if (latest_usage <= 80) "On Track" else if (latest_usage <= 100) "Caution" else "Over Budget"
      color <- if (latest_usage <= 80) "green" else if (latest_usage <= 100) "yellow" else "red"
    }
    valueBox(
      value = status,
      subtitle = "Budget Status",
      icon = icon("chart-pie"),
      color = color
    )
  })
  
  # Daily Expenses Plot
  output$daily_expenses <- renderPlotly({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    p <- ggplot(data$daily_summary, aes(x = Day, y = Total, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "₹")) +
      labs(title = "Daily Expenses by Category", x = "Date", y = "Amount")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Category Pie Chart
  output$category_pie <- renderPlotly({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    category_totals <- data$data %>%
      group_by(Category) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop")
    
    p <- plot_ly(category_totals, labels = ~Category, values = ~Total, type = "pie",
                 textposition = "inside", textinfo = "label+percent",
                 marker = list(colors = c("#FF6384", "#36A2EB", "#FFCE56", "#4BC0C0", "#9966FF", "#FF9F40")))
    p %>% layout(title = "Category Distribution", showlegend = TRUE)
  })
  
  # Category Trends
  output$category_trends <- renderPlotly({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    p <- ggplot(data$monthly_summary, aes(x = Month, y = Total, color = Category)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "₹")) +
      labs(title = "Category Spending Trends", x = "Month", y = "Amount")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Category Analysis
  output$category_analysis <- renderPlotly({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    filtered_data <- data$data
    if (!is.null(input$category_filter) && input$category_filter != "All") {
      filtered_data <- filtered_data %>% filter(Category == input$category_filter)
    }
    if (!is.null(input$date_range)) {
      filtered_data <- filtered_data %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    }
    
    p <- ggplot(filtered_data, aes(x = Date, y = Amount, color = Category)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = TRUE) +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "₹")) +
      labs(title = "Category Spending Analysis", x = "Date", y = "Amount")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Spending Patterns
  output$pattern_analysis <- renderPlotly({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    p <- ggplot(data$data, aes(x = Weekday, y = Amount, fill = IsWeekend)) +
      geom_boxplot() +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "₹")) +
      labs(title = "Spending Patterns by Weekday", x = "Weekday", y = "Amount")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Savings Forecast
  output$savings_forecast <- renderPlotly({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    forecast_data <- forecast_savings(data$monthly_totals, input$forecast_months)
    if (is.null(forecast_data)) return(NULL)
    
    p <- ggplot(forecast_data, aes(x = Month, y = Savings, color = Type)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      geom_ribbon(aes(ymin = Lower_CI_95, ymax = Upper_CI_95, fill = Type), alpha = 0.2, color = NA) +
      geom_ribbon(aes(ymin = Lower_CI_80, ymax = Upper_CI_80, fill = Type), alpha = 0.3, color = NA) +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format(prefix = "₹")) +
      labs(title = "Savings Forecast with Confidence Intervals", x = "Month", y = "Projected Savings") +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Forecast Summary
  output$forecast_summary <- renderTable({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    forecast_data <- forecast_savings(data$monthly_totals, input$forecast_months)
    if (is.null(forecast_data)) return(NULL)
    
    forecast_data %>%
      filter(Type == "Forecast") %>%
      mutate(
        Month = format(Month, "%b %Y"),
        Savings = paste0("₹", format(round(Savings), big.mark = ",")),
        `80% CI` = paste0("₹", format(round(Lower_CI_80), big.mark = ","), " - ₹", format(round(Upper_CI_80), big.mark = ",")),
        `95% CI` = paste0("₹", format(round(Lower_CI_95), big.mark = ","), " - ₹", format(round(Upper_CI_95), big.mark = ","))
      ) %>%
      select(Month, Savings, `80% CI`, `95% CI`)
  })
  
  # Recommendations
  output$recommendations <- renderText({
    data <- expense_data()
    if (is.null(data)) return("No data available for recommendations")
    
    top_category <- data$data %>%
      group_by(Category) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice(1) %>%
      pull(Category)
    
    paste0(
      "• Consider reducing spending in ", top_category, " category\n",
      "• Aim to maintain savings rate above 20%\n",
      "• Review discretionary spending monthly"
    )
  })
  
  # Data Overview
  output$total_transactions <- renderInfoBox({
    data <- expense_data()
    count <- if (is.null(data)) 0 else nrow(data$data)
    infoBox(title = "Total Transactions", value = format(count, big.mark = ","), icon = icon("receipt"), color = "blue")
  })
  
  output$date_range_info <- renderInfoBox({
    data <- expense_data()
    if (is.null(data)) {
      range_text <- "No data"
    } else {
      start_date <- min(data$data$Date, na.rm = TRUE)
      end_date <- max(data$data$Date, na.rm = TRUE)
      range_text <- paste(format(start_date, "%b %Y"), "-", format(end_date, "%b %Y"))
    }
    infoBox(title = "Date Range", value = range_text, icon = icon("calendar-alt"), color = "green")
  })
  
  output$categories_count <- renderInfoBox({
    data <- expense_data()
    count <- if (is.null(data)) 0 else length(data$categories)
    infoBox(title = "Categories", value = count, icon = icon("tags"), color = "yellow")
  })
  
  output$data_quality <- renderInfoBox({
    data <- expense_data()
    if (is.null(data)) {
      quality <- "No data"
      color <- "red"
    } else {
      missing_pct <- sum(is.na(data$data$Amount)) / nrow(data$data) * 100
      quality <- if (missing_pct < 5) "Good" else if (missing_pct < 15) "Fair" else "Poor"
      color <- if (missing_pct < 5) "green" else if (missing_pct < 15) "yellow" else "red"
    }
    infoBox(title = "Data Quality", value = quality, icon = icon("check-circle"), color = color)
  })
  
  # Data Tables
  output$detailed_table <- DT::renderDataTable({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    filtered_data <- data$data
    if (!is.null(input$category_filter) && input$category_filter != "All") {
      filtered_data <- filtered_data %>% filter(Category == input$category_filter)
    }
    if (!is.null(input$date_range)) {
      filtered_data <- filtered_data %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    }
    
    DT::datatable(
      filtered_data %>% select(Date, Category, Amount, Description) %>% arrange(desc(Date)),
      options = list(pageLength = 15, searchHighlight = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
      extensions = 'Buttons',
      rownames = FALSE
    ) %>% DT::formatCurrency('Amount', currency = "₹")
  })
  
  output$full_data_table <- DT::renderDataTable({
    data <- expense_data()
    if (is.null(data)) return(NULL)
    
    DT::datatable(
      data$data %>% arrange(desc(Date)),
      options = list(pageLength = 25, searchHighlight = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>% DT::formatCurrency('Amount', currency = "₹")
  })
  
  # Data Export
  output$download_summary <- downloadHandler(
    filename = function() { "expense_summary.csv" },
    content = function(file) {
      data <- expense_data()
      if (!is.null(data)) {
        write_csv(data$monthly_summary, file)
      }
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() { "raw_data.csv" },
    content = function(file) {
      data <- expense_data()
      if (!is.null(data)) {
        write_csv(data$data, file)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)