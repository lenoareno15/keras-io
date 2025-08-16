# Load required packages
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(DT)
library(janitor)
library(stringr)
library(scales)
library(rmarkdown)
library(broom)
library(caret)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(thematic)

# Enable auto-theming for ggplotly
thematic::thematic_shiny()

# Allow bookmarking app state in URL
enableBookmarking("url")

# Define the custom %notin% operator
`%notin%` <- Negate(`%in%`)

# Helper: safe percent
percent_num <- function(x) scales::percent(x, accuracy = 0.01)

# Function to train a predictive model with configurable method and features
train_predictive_model <- function(df, formula_string, method = "lm") {
  model_data <- df %>% filter(complete.cases(.))
  set.seed(42)
  control <- trainControl(method = "cv", number = 3)
  model <- train(as.formula(formula_string), data = model_data, method = method, trControl = control)
  return(model)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dental Claims Analytics - Pro"),
  dashboardSidebar(
    useShinyjs(),
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Facility Insights", tabName = "facilities", icon = icon("hospital")),
      menuItem("Peer Comparison", tabName = "peers", icon = icon("user-md")),
      menuItem("Facility Visual", tabName = "facility_plot", icon = icon("chart-bar")),
      menuItem("Clinician Insights", tabName = "clinician_plot", icon = icon("user")),
      menuItem("Clinician Comparison", tabName = "clinician_compare", icon = icon("balance-scale")),
      menuItem("Market Analysis", tabName = "market_analysis", icon = icon("chart-line")),
      menuItem("Statistical Analysis", tabName = "stat_analysis", icon = icon("chart-area")),
      menuItem("Predictive Analysis", tabName = "predictive", icon = icon("chart-bar")),
      conditionalPanel(
        condition = "input.enable_code_exploration == true",
        menuItem("Explore by Code", tabName = "code_explore", icon = icon("search"))
      )
    ),
    hr(),
    fileInput("claims_file", "Upload Claims Excel File (.xlsx)", accept = ".xlsx"),
    fileInput("clinician_file", "Upload Clinician Details Excel File (.xlsx)", accept = ".xlsx"),
    fileInput("code_description_file", "Upload Code Description File (.xlsx)", accept = ".xlsx"),
    checkboxInput("enable_code_exploration", "Enable Code Exploration", value = FALSE),
    hr(),
    pickerInput(
      "facility_select", "Select Facility:", choices = NULL, selected = NULL, multiple = TRUE,
      options = list(`live-search` = TRUE, `actions-box` = TRUE, `selected-text-format` = "count > 3", title = "Type to search...")
    ),
    pickerInput(
      "clinician_compare", "Compare Clinicians:", choices = NULL, selected = NULL, multiple = TRUE,
      options = list(`live-search` = TRUE, `actions-box` = TRUE)
    ),
    pickerInput(
      "multiple_clinicians", "Select Clinicians:", choices = NULL, selected = NULL, multiple = TRUE,
      options = list(`live-search` = TRUE, `actions-box` = TRUE, title = "Type to search...")
    ),
    conditionalPanel(
      condition = "input.enable_code_exploration == true",
      pickerInput(
        "code_select", "Select USCLS Code:", choices = NULL, selected = NULL, multiple = TRUE,
        options = list(`live-search` = TRUE, `actions-box` = TRUE, title = "Type or search code...")
      )
    ),
    hr(),
    sliderInput("anomaly_quantile", "Odd Claims Quantile Threshold", min = 0.80, max = 0.995, value = 0.95, step = 0.005),
    actionBttn("reset_filters", "Reset Filters", style = "fill", color = "warning", icon = icon("eraser")),
    br(),
    prettySwitch("enable_dark", "Dark Mode (thematic)", status = "info", fill = TRUE, value = FALSE),
    hr(),
    pickerInput("predictive_features", "Predictors for Model:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
    radioGroupButtons(
      inputId = "predictive_model",
      label = "Model Type",
      choices = c("Linear" = "lm", "Random Forest" = "ranger"),
      justified = TRUE
    ),
    br(),
    downloadButton("download_report", "Download PDF Report"),
    downloadButton("download_filtered_csv", "Download Filtered CSV")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".small-note { color:#666; font-size:12px; margin-left:6px } .plot-box{min-height:380px}"))),

    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("kpi_total_paid", width = 4),
          valueBoxOutput("kpi_total_qty", width = 4),
          valueBoxOutput("kpi_total_patients", width = 4)
        ),
        fluidRow(
          box(title = "Top USCLS Codes by Volume (Approved Quantity)", width = 6, status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("top_codes_vol", height = 360), type = 4)),
          box(title = "Top USCLS Codes by Cost", width = 6, status = "danger", solidHeader = TRUE,
              withSpinner(plotlyOutput("top_codes_cost", height = 360), type = 4))
        ),
        fluidRow(uiOutput("clinician_specific_ui"))
      ),

      tabItem(
        tabName = "facilities",
        fluidRow(box(title = "Facility Utilization Table", width = 12, status = "info", solidHeader = TRUE, DTOutput("facility_table"))),
        fluidRow(box(title = "Facility Metrics: Claims, Patients, Paid", width = 12, status = "info", solidHeader = TRUE,
                     withSpinner(plotlyOutput("facility_claims_plot", height = 400), type = 4)))
      ),

      tabItem(
        tabName = "peers",
        fluidRow(box(title = "Provider vs Peer Avg Claims", width = 12, status = "purple", solidHeader = TRUE,
                     withSpinner(plotlyOutput("peer_comparison", height = 420), type = 4)))
      ),

      tabItem(tabName = "facility_plot", uiOutput("facility_specific_ui")),

      tabItem(
        tabName = "clinician_plot",
        fluidRow(
          box(title = "Top Performing Clinicians by Paid Amount", width = 6, status = "success", solidHeader = TRUE,
              withSpinner(plotlyOutput("top_clinician_paid", height = 380), type = 4)),
          box(title = "Clinicians with Highest Number of Patients", width = 6, status = "warning", solidHeader = TRUE,
              withSpinner(plotlyOutput("top_clinician_patients", height = 380), type = 4))
        )
      ),

      tabItem(
        tabName = "clinician_compare",
        fluidRow(box(title = "Claims vs Patients vs Paid per Selected Clinicians", width = 12, status = "primary", solidHeader = TRUE,
                     withSpinner(plotlyOutput("compare_clinicians_plot", height = 420), type = 4)))
      ),

      tabItem(
        tabName = "market_analysis",
        fluidRow(
          box(title = "Market Analysis Overview", width = 12, status = "primary", solidHeader = TRUE, uiOutput("market_overview_ui"))
        ),
        fluidRow(
          box(title = "Clinician Distribution by Location", width = 12, status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("location_clinician_plot", height = 360), type = 4)),
          box(title = "Patient & Claims Distribution by Location", width = 12, status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("location_patient_claims_plot", height = 360), type = 4))
        ),
        fluidRow(
          box(title = "Patient Share by Specialty", width = 6, status = "success", solidHeader = TRUE,
              withSpinner(plotlyOutput("patient_share_plot", height = 360), type = 4)),
          box(title = "Market Share by Specialty", width = 6, status = "danger", solidHeader = TRUE,
              withSpinner(plotlyOutput("market_share_plot", height = 360), type = 4))
        ),
        fluidRow(
          box(title = "Patient Share Insights", width = 6, status = "success", solidHeader = TRUE, uiOutput("patient_share_insights")),
          box(title = "Market Share Insights", width = 6, status = "danger", solidHeader = TRUE, uiOutput("market_share_insights"))
        ),
        fluidRow(
          box(title = "Top 20 Clinicians by Payment", width = 6, status = "primary", solidHeader = TRUE, uiOutput("top_clinicians_payment")),
          box(title = "Top 20 Facilities by Payment", width = 6, status = "primary", solidHeader = TRUE, uiOutput("top_facilities_payment"))
        ),
        fluidRow(
          box(title = "Facility Classification", width = 6, status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("facility_classification", height = 380), type = 4))
        )
      ),

      tabItem(
        tabName = "predictive",
        fluidRow(
          box(title = "Predictive Analysis of Claims", width = 12, status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("predictive_plot", height = 420), type = 4), uiOutput("predictive_insights"))
        )
      ),

      tabItem(
        tabName = "stat_analysis",
        fluidRow(
          box(title = "Odd Claims Detection", width = 6, status = "warning", solidHeader = TRUE, DTOutput("odd_claims_table")),
          box(title = "Odd Claims Narrative", width = 6, status = "warning", solidHeader = TRUE, uiOutput("odd_claims_narrative"))
        ),
        fluidRow(
          box(title = "Statistical Analysis Results", width = 12, status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("stat_analysis_plot", height = 420), type = 4), uiOutput("stat_analysis_narrative"))
        )
      ),

      tabItem(
        tabName = "code_explore",
        conditionalPanel(
          condition = "input.enable_code_exploration == true",
          fluidRow(box(title = "Code Expenditure", width = 12, status = "primary", solidHeader = TRUE, uiOutput("code_expenditure_ui"))),
          fluidRow(
            box(title = "Top Clinicians by Approved Quantity", width = 6, status = "success", solidHeader = TRUE,
                withSpinner(plotlyOutput("top_clinicians_quantity_plot", height = 360), type = 4)),
            box(title = "Top Facilities by Approved Quantity", width = 6, status = "success", solidHeader = TRUE,
                withSpinner(plotlyOutput("top_facilities_quantity_plot", height = 360), type = 4))
          ),
          fluidRow(box(title = "Top Quantity Insights", width = 12, status = "info", solidHeader = TRUE, uiOutput("top_quantity_entities"))),
          fluidRow(
            box(title = "Paid by Facility for Selected Code", width = 6, status = "primary", solidHeader = TRUE,
                withSpinner(plotlyOutput("code_by_facility", height = 360), type = 4)),
            box(title = "Paid by Clinician for Selected Code", width = 6, status = "primary", solidHeader = TRUE,
                withSpinner(plotlyOutput("code_by_clinician", height = 360), type = 4))
          ),
          fluidRow(
            box(title = "Claims and Patients by Facility for Selected Code", width = 6, status = "info", solidHeader = TRUE,
                withSpinner(plotlyOutput("claims_patients_facility", height = 360), type = 4)),
            box(title = "Claims and Patients by Clinician for Selected Code", width = 6, status = "info", solidHeader = TRUE,
                withSpinner(plotlyOutput("claims_patients_clinician", height = 360), type = 4))
          ),
          fluidRow(box(title = "Market Share Insights", width = 12, status = "info", solidHeader = TRUE, uiOutput("market_share_analysis")))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 200 * 1024^2)

  observe({
    if (isTRUE(input$enable_dark)) {
      thematic::thematic_on()
    } else {
      thematic::thematic_off()
    }
  })

  claims_data <- reactive({
    req(input$claims_file)
    df <- read_excel(input$claims_file$datapath) %>% clean_names()
    df <- df %>%
      mutate(executingclinicianid = toupper(str_trim(as.character(executingclinicianid)))) %>%
      filter(!is.na(activitydetail_id), !is.na(executingclinicianid), !is.na(patientcount), !is.na(paid), paid > 0)
    df
  }) %>% bindCache(input$claims_file$datapath)

  clinician_data <- reactive({
    req(input$clinician_file)
    df <- read_excel(input$clinician_file$datapath) %>% clean_names()
    df <- df %>%
      mutate(
        clinician_license = toupper(str_trim(as.character(clinician_license))),
        category = str_trim(as.character(category)),
        location = str_trim(as.character(location))
      )
    df
  }) %>% bindCache(input$clinician_file$datapath)

  code_description_data <- reactive({
    req(input$code_description_file)
    df <- read_excel(input$code_description_file$datapath) %>% clean_names()
    df <- df %>% rename(Code = code, Description = description)
    df
  }) %>% bindCache(input$code_description_file$datapath)

  merged_data <- reactive({
    c_data <- claims_data()
    d_data <- clinician_data()
    code_data <- code_description_data()

    merged <- c_data %>%
      left_join(d_data, by = c("executingclinicianid" = "clinician_license")) %>%
      left_join(code_data, by = c("activitydetail_id" = "Code")) %>%
      mutate(
        clinician_name = ifelse(is.na(clinician_name), executingclinicianid, clinician_name),
        facility_name = ifelse(is.na(facility_name), "Unknown Facility", facility_name)
      )
    merged
  }) %>% bindCache(input$claims_file$datapath, input$clinician_file$datapath, input$code_description_file$datapath)

  # Populate selectors
  observe({
    df <- merged_data()
    clinicians <- sort(unique(df$clinician_name))
    codes <- sort(unique(df$activitydetail_id))
    facilities <- sort(unique(df$facility_name))

    updatePickerInput(session, "clinician_compare", choices = c("All", clinicians), selected = NULL)
    updatePickerInput(session, "multiple_clinicians", choices = c("All", clinicians), selected = NULL)
    updatePickerInput(session, "code_select", choices = c("All", codes), selected = NULL)
    updatePickerInput(session, "facility_select", choices = c("All", facilities), selected = NULL)

    # predictive features: suggest numeric columns
    numeric_candidates <- df %>% select(where(is.numeric)) %>% names()
    numeric_candidates <- setdiff(numeric_candidates, c("approvedquantity"))
    updatePickerInput(session, "predictive_features", choices = numeric_candidates, selected = intersect(c("paid", "patientcount"), numeric_candidates))
  })

  # Reset filters
  observeEvent(input$reset_filters, {
    df <- merged_data()
    updatePickerInput(session, "facility_select", selected = "All")
    updatePickerInput(session, "clinician_compare", selected = "All")
    updatePickerInput(session, "multiple_clinicians", selected = NULL)
    updatePickerInput(session, "code_select", selected = NULL)
  })

  # Filtered data helpers
  filtered_data <- reactive({
    df <- merged_data()
    if (!is.null(input$facility_select) && !("All" %in% input$facility_select)) {
      df <- df %>% filter(facility_name %in% input$facility_select)
    }
    if (!is.null(input$clinician_compare) && !("All" %in% input$clinician_compare)) {
      df <- df %>% filter(clinician_name %in% input$clinician_compare)
    }
    df
  }) %>% bindCache(input$facility_select, input$clinician_compare)

  filtered_code_data <- reactive({
    df <- merged_data()
    if (!is.null(input$code_select) && length(input$code_select) > 0 && !("All" %in% input$code_select)) {
      df <- df %>% filter(activitydetail_id %in% input$code_select)
    }
    df
  }) %>% bindCache(input$code_select)

  # KPIs
  output$kpi_total_paid <- renderValueBox({
    df <- filtered_data()
    valueBox(value = dollar(sum(df$paid, na.rm = TRUE)), subtitle = "Total Expenditure (Filtered)", icon = icon("dollar-sign"), color = "green")
  })
  output$kpi_total_qty <- renderValueBox({
    df <- filtered_data()
    valueBox(value = comma(sum(df$approvedquantity, na.rm = TRUE)), subtitle = "Approved Quantity (Filtered)", icon = icon("file-invoice"), color = "blue")
  })
  output$kpi_total_patients <- renderValueBox({
    df <- filtered_data()
    valueBox(value = comma(sum(df$patientcount, na.rm = TRUE)), subtitle = "Patients (Filtered)", icon = icon("user-injured"), color = "purple")
  })

  # Market Overview
  market_overview_stats <- reactive({
    df <- merged_data()
    list(
      total_paid = sum(df$paid, na.rm = TRUE),
      total_approved_quantity = sum(df$approvedquantity, na.rm = TRUE),
      total_patients = sum(df$patientcount, na.rm = TRUE)
    )
  })

  output$market_overview_ui <- renderUI({
    stats <- market_overview_stats()
    HTML(sprintf(
      "<div style='padding: 16px; background-color: #e9ecef; border-radius: 6px;'>
        <h4 style='margin-top:0;'>Market Analysis Overview</h4>
        <p><strong>Total Expenditure:</strong> %s</p>
        <p><strong>Total Approved Quantity:</strong> %s</p>
        <p><strong>Total Patients:</strong> %s</p>
      </div>",
      dollar(stats$total_paid), comma(stats$total_approved_quantity), comma(stats$total_patients)
    ))
  })

  # Facility table (server-side, buttons, scroller)
  output$facility_table <- renderDT({
    df <- filtered_data() %>%
      group_by(facility_name) %>%
      summarise(
        Claims = sum(approvedquantity, na.rm = TRUE),
        Patients = sum(patientcount, na.rm = TRUE),
        Paid = sum(paid, na.rm = TRUE)
      ) %>% arrange(desc(Paid))

    datatable(
      df,
      extensions = c("Buttons", "Scroller", "FixedColumns"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 380,
        scroller = TRUE,
        pageLength = 20
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  # Code expenditure UI
  code_expenditure_stats <- reactive({
    df <- filtered_code_data()
    sum(df$paid, na.rm = TRUE)
  })
  output$code_expenditure_ui <- renderUI({
    if (!is.null(input$code_select) && length(input$code_select) == 1) {
      total_expenditure <- code_expenditure_stats()
      code_description <- input$code_select
      HTML(sprintf(
        "<div style='padding: 16px; background-color: #e9ecef; border-radius: 6px;'>
          <h4>Expenditure for Code: %s</h4>
          <p><strong>Total Expenditure:</strong> %s</p>
        </div>",
        code_description, dollar(total_expenditure)
      ))
    } else {
      HTML("")
    }
  })

  # Top entities by approved quantity for selected code
  top_entities_by_quantity <- function() {
    df <- filtered_code_data()
    total_approved <- sum(df$approvedquantity, na.rm = TRUE)
    top_clinicians <- df %>% group_by(clinician_name) %>% summarise(TotalQuantity = sum(approvedquantity, na.rm = TRUE)) %>%
      mutate(Percentage = (TotalQuantity / total_approved) * 100) %>% arrange(desc(TotalQuantity)) %>% slice_head(n = 10)
    top_facilities <- df %>% group_by(facility_name) %>% summarise(TotalQuantity = sum(approvedquantity, na.rm = TRUE)) %>%
      mutate(Percentage = (TotalQuantity / total_approved) * 100) %>% arrange(desc(TotalQuantity)) %>% slice_head(n = 10)
    list(top_clinicians = top_clinicians, top_facilities = top_facilities)
  }

  output$top_quantity_entities <- renderUI({
    entities <- top_entities_by_quantity()
    top_clinicians <- entities$top_clinicians
    top_facilities <- entities$top_facilities
    clinicians_text <- paste(
      top_clinicians$clinician_name, "accounts for",
      round(top_clinicians$Percentage, 2), "% of total approved quantity.",
      collapse = "<br>"
    )
    facilities_text <- paste(
      top_facilities$facility_name, "accounts for",
      round(top_facilities$Percentage, 2), "% of total approved quantity.",
      collapse = "<br>"
    )
    HTML(sprintf(
      "<div style='margin: 8px;'>
          <h4 style='color: #3c8dbc;'>Top Clinicians by Approved Quantity for %s</h4>
          <div style='padding: 10px; background-color: #f9f9f9; border-radius: 5px;'>%s</div>
          <h4 style='color: #3c8dbc; margin-top: 16px;'>Top Facilities by Approved Quantity for %s</h4>
          <div style='padding: 10px; background-color: #f9f9f9; border-radius: 5px;'>%s</div>
        </div>",
      paste(input$code_select, collapse = ", "), clinicians_text,
      paste(input$code_select, collapse = ", "), facilities_text
    ))
  })

  # Crossfilter helpers from plotly clicks
  observeEvent(event_data("plotly_click", source = "top_codes_vol"), {
    d <- event_data("plotly_click", source = "top_codes_vol")
    clicked <- if (!is.null(d$x)) d$x else d$y
    if (!is.null(clicked)) updatePickerInput(session, "code_select", selected = clicked)
  })
  observeEvent(event_data("plotly_click", source = "top_codes_cost"), {
    d <- event_data("plotly_click", source = "top_codes_cost")
    clicked <- if (!is.null(d$x)) d$x else d$y
    if (!is.null(clicked)) updatePickerInput(session, "code_select", selected = clicked)
  })
  observeEvent(event_data("plotly_click", source = "facility_metrics"), {
    d <- event_data("plotly_click", source = "facility_metrics")
    clicked <- if (!is.null(d$x)) d$x else d$y
    if (!is.null(clicked)) updatePickerInput(session, "facility_select", selected = clicked)
  })
  observeEvent(event_data("plotly_click", source = "top_paid_clinician"), {
    d <- event_data("plotly_click", source = "top_paid_clinician")
    clicked <- if (!is.null(d$x)) d$x else d$y
    if (!is.null(clicked)) updatePickerInput(session, "clinician_compare", selected = clicked)
  })

  # Plots
  output$top_codes_vol <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    top_codes <- df %>% group_by(activitydetail_id, Description) %>% summarise(volume = sum(approvedquantity, na.rm = TRUE), .groups = "drop") %>%
      top_n(10, volume) %>% arrange(desc(volume))
    p <- ggplot(top_codes, aes(x = reorder(activitydetail_id, volume), y = volume,
                               text = paste("Code:", activitydetail_id, "<br>Description:", Description, "<br>Approved Quantity:", comma(volume)))) +
      geom_col(fill = "#0073C2FF", width = 0.7) + coord_flip() +
      labs(title = "Top 10 USCLS Codes by Approved Quantity", x = "USCLS Code", y = "Approved Quantity") +
      theme_minimal() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text", source = "top_codes_vol")
  })

  output$top_codes_cost <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    top_cost <- df %>% group_by(activitydetail_id, Description) %>% summarise(cost = sum(paid, na.rm = TRUE), .groups = "drop") %>%
      top_n(10, cost) %>% arrange(desc(cost))
    p <- ggplot(top_cost, aes(x = reorder(activitydetail_id, cost), y = cost,
                              text = paste("Code:", activitydetail_id, "<br>Description:", Description, "<br>Paid Amount:", dollar(cost)))) +
      geom_col(fill = "#E64B35FF", width = 0.7) + coord_flip() +
      labs(title = "Top 10 USCLS Codes by Paid Amount", x = "USCLS Code", y = "Cost (Paid)") +
      theme_minimal() + scale_y_continuous(labels = dollar)
    ggplotly(p, tooltip = "text", source = "top_codes_cost")
  })

  output$clinician_specific_ui <- renderUI({
    req(input$multiple_clinicians)
    clinician_plots <- lapply(input$multiple_clinicians, function(clinician) {
      plotlyOutput(paste0("top_codes_clinician_", gsub("[^A-Za-z0-9]", "", clinician)))
    })
    fluidRow(do.call(tagList, clinician_plots))
  })

  observe({
    req(input$multiple_clinicians)
    df <- filtered_data()
    for (clinician in input$multiple_clinicians) {
      local({
        clinician_name_local <- clinician
        output[[paste0("top_codes_clinician_", gsub("[^A-Za-z0-9]", "", clinician_name_local))]] <- renderPlotly({
          df_clin <- df %>% filter(clinician_name == clinician_name_local)
          req(nrow(df_clin) > 0)
          top_codes_by_clinician <- df_clin %>% group_by(activitydetail_id, Description) %>%
            summarise(Claims = sum(approvedquantity, na.rm = TRUE), Paid = sum(paid, na.rm = TRUE), .groups = "drop") %>%
            top_n(10, Claims)
          df_melt <- top_codes_by_clinician %>% pivot_longer(cols = c("Claims", "Paid"), names_to = "Metric", values_to = "Value")
          p <- ggplot(df_melt, aes(x = reorder(activitydetail_id, Value), y = Value, fill = Metric,
                                   text = paste("Code:", activitydetail_id, "<br>Description:", Description, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
            geom_col(position = "dodge") + coord_flip() +
            labs(title = paste("Top Codes by", clinician_name_local), x = "USCLS Code", y = "Value") +
            theme_minimal() + scale_y_continuous(labels = comma)
          ggplotly(p, tooltip = "text")
        })
      })
    }
  })

  output$facility_claims_plot <- renderPlotly({
    df <- filtered_data() %>% group_by(facility_name) %>%
      summarise(Claims = sum(approvedquantity, na.rm = TRUE), Patients = sum(patientcount, na.rm = TRUE), Paid = sum(paid, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    df_melt <- df %>% pivot_longer(cols = c("Claims", "Patients", "Paid"), names_to = "Metric", values_to = "Value")
    p <- ggplot(df_melt, aes(x = facility_name, y = Value, fill = Metric,
                             text = paste("Facility:", facility_name, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
      geom_col(position = "dodge") + labs(title = "Facility Metrics Overview", x = "Facility", y = "Value") +
      theme_minimal() + coord_flip() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text", source = "facility_metrics")
  })

  output$facility_specific_ui <- renderUI({
    req(input$facility_select)
    facility_plots <- lapply(input$facility_select, function(fac) {
      plotlyOutput(paste0("facility_plot_", gsub("[^A-Za-z0-9]", "", fac)))
    })
    fluidRow(box(title = "Claims Distribution per Selected Facility", width = 12, status = "info", solidHeader = TRUE, do.call(tagList, facility_plots)))
  })

  observe({
    req(input$facility_select)
    df <- filtered_data()
    for (fac in input$facility_select) {
      local({
        facility_name_local <- fac
        output[[paste0("facility_plot_", gsub("[^A-Za-z0-9]", "", facility_name_local))]] <- renderPlotly({
          df_fac <- df %>% filter(facility_name == facility_name_local) %>% group_by(activitydetail_id, Description) %>%
            summarise(Claims = sum(approvedquantity, na.rm = TRUE), Patients = sum(patientcount, na.rm = TRUE), Paid = sum(paid, na.rm = TRUE), .groups = "drop")
          req(nrow(df_fac) > 0)
          df_melt <- df_fac %>% pivot_longer(cols = c("Claims", "Patients", "Paid"), names_to = "Metric", values_to = "Value")
          p <- ggplot(df_melt, aes(x = activitydetail_id, y = Value, fill = Metric,
                                   text = paste("Code:", activitydetail_id, "<br>Description:", Description, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
            geom_col(position = "dodge") + labs(title = paste("Facility:", facility_name_local), x = "USCLS Code", y = "Value") +
            theme_minimal() + scale_y_continuous(labels = comma)
          ggplotly(p, tooltip = "text")
        })
      })
    }
  })

  output$top_clinician_paid <- renderPlotly({
    df <- filtered_data() %>% group_by(clinician_name) %>% summarise(Paid = sum(paid, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    top_paid <- df %>% top_n(10, Paid)
    p <- ggplot(top_paid, aes(x = reorder(clinician_name, Paid), y = Paid, text = paste("Clinician:", clinician_name, "<br>Paid:", dollar(Paid)))) +
      geom_col(fill = "#00BA38") + coord_flip() +
      labs(title = "Top 10 Clinicians by Paid Amount", x = "Clinician", y = "Paid") + theme_minimal() + scale_y_continuous(labels = dollar)
    ggplotly(p, tooltip = "text", source = "top_paid_clinician")
  })

  output$top_clinician_patients <- renderPlotly({
    df <- filtered_data() %>% group_by(clinician_name) %>% summarise(Patients = sum(patientcount, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    top_patients <- df %>% top_n(10, Patients)
    p <- ggplot(top_patients, aes(x = reorder(clinician_name, Patients), y = Patients, text = paste("Clinician:", clinician_name, "<br>Patients:", comma(Patients)))) +
      geom_col(fill = "#F8766D") + coord_flip() +
      labs(title = "Top 10 Clinicians by Patient Count", x = "Clinician", y = "Patients") + theme_minimal() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text")
  })

  output$peer_comparison <- renderPlotly({
    df <- filtered_data() %>% group_by(clinician_name) %>% summarise(Claims = sum(approvedquantity, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    df <- df %>% mutate(PeerAvg = mean(Claims, na.rm = TRUE))
    p <- ggplot(df, aes(x = reorder(clinician_name, Claims), y = Claims, text = paste("Clinician:", clinician_name, "<br>Claims:", comma(Claims)))) +
      geom_col(fill = "#9E77B0") + geom_hline(aes(yintercept = PeerAvg), color = "red", linetype = "dashed") +
      labs(title = "Clinician Claims vs Peer Average", x = "Clinician", y = "Claims") + coord_flip() + theme_minimal() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text")
  })

  output$compare_clinicians_plot <- renderPlotly({
    req(input$clinician_compare)
    df <- filtered_data() %>% filter(clinician_name %in% input$clinician_compare)
    df_summary <- df %>% group_by(clinician_name) %>% summarise(Claims = sum(approvedquantity, na.rm = TRUE), Patients = sum(patientcount, na.rm = TRUE), Paid = sum(paid, na.rm = TRUE), .groups = "drop")
    req(nrow(df_summary) > 0)
    df_melt <- df_summary %>% pivot_longer(cols = c("Claims", "Patients", "Paid"), names_to = "Metric", values_to = "Value")
    p <- ggplot(df_melt, aes(x = clinician_name, y = Value, fill = Metric, text = paste("Clinician:", clinician_name, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
      geom_col(position = "dodge") + labs(title = "Clinician Comparison: Claims, Patients, Paid", x = "Clinician", y = "Value") + theme_minimal() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text")
  })

  output$claims_patients_facility <- renderPlotly({
    req(input$code_select)
    df <- filtered_code_data() %>% group_by(facility_name) %>% summarise(Claims = sum(approvedquantity, na.rm = TRUE), Patients = sum(patientcount, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    df_melt <- df %>% pivot_longer(cols = c("Claims", "Patients"), names_to = "Metric", values_to = "Value")
    p <- ggplot(df_melt, aes(x = facility_name, y = Value, fill = Metric, text = paste("Facility:", facility_name, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
      geom_col(position = "dodge") + labs(title = paste("Claims and Patients by Facility for Codes:", paste(input$code_select, collapse = ", ")), x = "Facility", y = "Value") +
      theme_minimal() + coord_flip() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text")
  })

  output$claims_patients_clinician <- renderPlotly({
    req(input$code_select)
    df <- filtered_code_data() %>% group_by(clinician_name) %>% summarise(Claims = sum(approvedquantity, na.rm = TRUE), Patients = sum(patientcount, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    df_melt <- df %>% pivot_longer(cols = c("Claims", "Patients"), names_to = "Metric", values_to = "Value")
    p <- ggplot(df_melt, aes(x = clinician_name, y = Value, fill = Metric, text = paste("Clinician:", clinician_name, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
      geom_col(position = "dodge") + labs(title = paste("Claims and Patients by Clinician for Codes:", paste(input$code_select, collapse = ", ")), x = "Clinician", y = "Value") +
      theme_minimal() + coord_flip() + scale_y_continuous(labels = comma)
    ggplotly(p, tooltip = "text")
  })

  # Market share calculation for code exploration
  calculate_market_share <- function() {
    df <- merged_data()
    if (!is.null(input$code_select) && "All" %notin% input$code_select) {
      total_market_share <- df %>% filter(activitydetail_id %in% input$code_select) %>% summarise(TotalPaid = sum(paid, na.rm = TRUE))
      facility_share_df <- df %>% filter(activitydetail_id %in% input$code_select, facility_name %in% input$facility_select) %>% group_by(facility_name) %>% summarise(FacilityPaid = sum(paid, na.rm = TRUE), .groups = "drop")
      clinician_share_df <- df %>% filter(activitydetail_id %in% input$code_select, clinician_name %in% input$clinician_compare) %>% group_by(clinician_name) %>% summarise(ClinicianPaid = sum(paid, na.rm = TRUE), .groups = "drop")
      next_highest_facility_df <- df %>% filter(activitydetail_id %in% input$code_select, !facility_name %in% input$facility_select) %>% group_by(facility_name) %>% summarise(TotalPaid = sum(paid, na.rm = TRUE), .groups = "drop") %>% arrange(desc(TotalPaid))
      next_highest_clinician_df <- df %>% filter(activitydetail_id %in% input$code_select) %>% group_by(clinician_name) %>% summarise(TotalPaid = sum(paid, na.rm = TRUE), .groups = "drop") %>% arrange(desc(TotalPaid))
      list(
        facility_share = (sum(facility_share_df$FacilityPaid, na.rm = TRUE) / total_market_share$TotalPaid) * 100,
        clinician_share = (sum(clinician_share_df$ClinicianPaid, na.rm = TRUE) / total_market_share$TotalPaid) * 100,
        top_facility = next_highest_facility_df$facility_name[1],
        top_facility_share = (next_highest_facility_df$TotalPaid[1] / total_market_share$TotalPaid) * 100,
        top_clinician = next_highest_clinician_df$clinician_name[1],
        top_clinician_share = (next_highest_clinician_df$TotalPaid[1] / total_market_share$TotalPaid) * 100
      )
    } else {
      list()
    }
  }

  output$market_share_analysis <- renderUI({
    shares <- calculate_market_share()
    if (length(shares) == 0) return(HTML(""))
    facility_share <- shares$facility_share
    clinician_share <- shares$clinician_share
    top_facility <- shares$top_facility
    top_facility_share <- shares$top_facility_share
    top_clinician <- shares$top_clinician
    top_clinician_share <- shares$top_clinician_share
    codes <- paste(input$code_select, collapse = ", ")
    facility_text <- if (!is.null(facility_share) && !is.na(facility_share)) {
      sprintf("Clinicians at <b>%s</b> represent <b>%.2f%%</b> of the utilization of Codes <b>%s</b> in the entire market.",
              ifelse(length(input$facility_select) > 1, "the selected facilities", paste(input$facility_select, collapse = ", ")), facility_share, codes)
    } else {
      "Insufficient data or invalid facility selection to perform analysis."
    }
    clinician_text <- if (!is.null(clinician_share) && !is.na(clinician_share)) {
      sprintf("The selected clinicians represent <b>%.2f%%</b> of the utilization of Codes <b>%s</b> in the entire market.", clinician_share, codes)
    } else {
      "Insufficient data or invalid clinician selection to perform analysis."
    }
    next_facility_text <- if (!is.na(top_facility_share)) {
      sprintf("The next highest utilization facility is <b>%s</b>, representing <b>%.2f%%</b> market share for Codes <b>%s</b>.", top_facility, top_facility_share, codes)
    } else {
      "Insufficient data to determine the next highest utilization facility."
    }
    next_clinician_text <- if (!is.na(top_clinician_share)) {
      sprintf("The highest clinician utilizing the code is <b>%s</b>, representing <b>%.2f%%</b> of the market utilization for Codes <b>%s</b>.", top_clinician, top_clinician_share, codes)
    } else {
      "Insufficient data to determine the highest utilization clinician."
    }
    HTML(sprintf("<h4>Market Share Insights</h4><p>%s</p><p>%s</p><h4>Additional Insights</h4><p>%s</p><p>%s</p>", facility_text, clinician_text, next_facility_text, next_clinician_text))
  })

  # Predictive model + plot
  output$predictive_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)

    # Build formula string from selected features
    predictors <- input$predictive_features
    if (is.null(predictors) || length(predictors) == 0) predictors <- "paid"
    formula_string <- paste("approvedquantity ~", paste(predictors, collapse = " + "))

    model <- tryCatch(train_predictive_model(df, formula_string, input$predictive_model), error = function(e) NULL)

    if (is.null(model)) return(NULL)

    df$PredictedClaims <- predict(model, df)

    # Tooltip: clinician + most used code
    top_code <- df %>% count(activitydetail_id, sort = TRUE) %>% slice_head(n = 1) %>% pull(activitydetail_id)
    p <- ggplot(df, aes(x = .data[[predictors[1]]], y = approvedquantity,
                        text = paste("Clinician:", clinician_name, "<br>Most Used Code:", top_code, "<br>Actual:", approvedquantity, "<br>Pred:", round(PredictedClaims, 2)))) +
      geom_point(size = 1.6, color = "#2c7be5", alpha = 0.5) +
      geom_smooth(aes(y = PredictedClaims), method = "loess", se = FALSE, color = "#e63757") +
      labs(title = "Predictive Analysis of Claims", x = predictors[1], y = "Approved Quantity (Claims)") + theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  output$predictive_insights <- renderUI({
    HTML("<div style='margin: 8px;'>
          <h4>Predictive Model Insights</h4>
          <p>The model estimates claims from your selected predictors. Change the model type and predictors to see how the fit responds. Hover points for clinician context and predicted vs actual.</p>
          </div>")
  })

  # Location and specialty plots
  output$location_clinician_plot <- renderPlotly({
    df <- merged_data()
    if ("location" %in% names(df)) {
      location_clinician_count <- df %>% group_by(location) %>% summarise(ClinicianCount = n_distinct(clinician_name), .groups = "drop")
      p <- ggplot(location_clinician_count, aes(x = reorder(location, -ClinicianCount), y = ClinicianCount, fill = location,
                                               text = paste("Location:", location, "<br>Clinicians:", ClinicianCount))) +
        geom_col(show.legend = FALSE) + coord_flip() +
        labs(title = "Clinician Distribution by Location", x = "Location", y = "Number of Clinicians") + theme_minimal()
      ggplotly(p, tooltip = "text")
    }
  })

  output$location_patient_claims_plot <- renderPlotly({
    df <- merged_data()
    if ("location" %in% names(df)) {
      location_data <- df %>% group_by(location) %>% summarise(TotalPatients = sum(patientcount, na.rm = TRUE), TotalClaims = sum(approvedquantity, na.rm = TRUE), .groups = "drop")
      df_melt <- location_data %>% pivot_longer(cols = c("TotalPatients", "TotalClaims"), names_to = "Metric", values_to = "Value")
      p <- ggplot(df_melt, aes(x = reorder(location, -Value), y = Value, fill = Metric,
                               text = paste("Location:", location, "<br>Metric:", Metric, "<br>Value:", comma(Value)))) +
        geom_col(position = "dodge") + coord_flip() +
        labs(title = "Patient and Claims Distribution by Location", x = "Location", y = "Count") + theme_minimal()
      ggplotly(p, tooltip = "text")
    }
  })

  # Patient share by specialty
  output$patient_share_insights <- renderUI({
    df <- merged_data()
    if ("category" %in% names(df)) {
      total_patients <- sum(df$patientcount, na.rm = TRUE)
      specialty_patient_count <- df %>% group_by(category) %>% summarise(PatientCount = sum(patientcount, na.rm = TRUE), .groups = "drop") %>%
        mutate(Percentage = (PatientCount / total_patients) * 100) %>% arrange(desc(PatientCount))
      patient_share_text <- paste(specialty_patient_count$category, "accounts for", round(specialty_patient_count$Percentage, 2), "% of total patients.", collapse = "<br>")
      HTML(sprintf("<div style='margin: 8px;'><div style='padding: 10px; background-color: #f9f9f9; border-radius: 5px;'>%s</div></div>", patient_share_text))
    } else {
      HTML("<div><h4>Patient Share by Specialty</h4><p>No category data available.</p></div>")
    }
  })

  output$patient_share_plot <- renderPlotly({
    df <- merged_data()
    if ("category" %in% names(df)) {
      specialty_patient_count <- df %>% group_by(category) %>% summarise(PatientCount = sum(patientcount, na.rm = TRUE), .groups = "drop")
      p <- ggplot(specialty_patient_count, aes(x = reorder(category, -PatientCount), y = PatientCount, fill = category,
                                              text = paste("Specialty:", category, "<br>Patients:", comma(PatientCount)))) +
        geom_col(show.legend = FALSE) + coord_flip() +
        labs(title = "Patient Distribution by Specialty", x = "Specialty", y = "Patient Count") + theme_minimal()
      ggplotly(p, tooltip = "text")
    }
  })

  # Market share by specialty
  output$market_share_insights <- renderUI({
    df <- merged_data()
    if ("category" %in% names(df)) {
      total_paid <- sum(df$paid, na.rm = TRUE)
      specialty_paid_count <- df %>% group_by(category) %>% summarise(TotalPaid = sum(paid, na.rm = TRUE), .groups = "drop") %>%
        mutate(Percentage = (TotalPaid / total_paid) * 100) %>% arrange(desc(TotalPaid))
      market_share_text <- paste(specialty_paid_count$category, "accounts for", round(specialty_paid_count$Percentage, 2), "% of total payments.", collapse = "<br>")
      HTML(sprintf("<div style='margin: 8px;'><div style='padding: 10px; background-color: #f9f9f9; border-radius: 5px;'>%s</div></div>", market_share_text))
    } else {
      HTML("<div><h4>Market Share by Specialty</h4><p>No category data available.</p></div>")
    }
  })

  output$market_share_plot <- renderPlotly({
    df <- merged_data()
    if ("category" %in% names(df)) {
      specialty_paid_count <- df %>% group_by(category) %>% summarise(TotalPaid = sum(paid, na.rm = TRUE), .groups = "drop")
      p <- ggplot(specialty_paid_count, aes(x = reorder(category, -TotalPaid), y = TotalPaid, fill = category,
                                           text = paste("Specialty:", category, "<br>Total Paid:", dollar(TotalPaid)))) +
        geom_col(show.legend = FALSE) + coord_flip() +
        labs(title = "Financial Market Share by Specialty", x = "Specialty", y = "Total Paid") + theme_minimal()
      ggplotly(p, tooltip = "text")
    }
  })

  # Top 20 clinicians/facilities by payment (UI)
  output$top_clinicians_payment <- renderUI({
    df <- merged_data()
    total_paid <- sum(df$paid, na.rm = TRUE)
    top_clinicians_paid <- df %>% group_by(clinician_name) %>% summarise(paid = sum(paid, na.rm = TRUE), .groups = "drop") %>%
      mutate(percentage = (paid / total_paid) * 100) %>% arrange(desc(paid)) %>% slice_head(n = 20)
    clinicians_paid_text <- paste(top_clinicians_paid$clinician_name, "accounts for", round(top_clinicians_paid$percentage, 2), "% of total paid amount.", collapse = "<br>")
    HTML(sprintf("<div style='margin: 8px;'><div style='padding: 10px; background-color: #f9f9f9; border-radius: 5px;'>%s</div></div>", clinicians_paid_text))
  })

  output$top_facilities_payment <- renderUI({
    df <- merged_data()
    total_paid <- sum(df$paid, na.rm = TRUE)
    top_facilities_paid <- df %>% group_by(facility_name) %>% summarise(paid = sum(paid, na.rm = TRUE), .groups = "drop") %>%
      mutate(percentage = (paid / total_paid) * 100) %>% arrange(desc(paid)) %>% slice_head(n = 20)
    facilities_paid_text <- paste(top_facilities_paid$facility_name, "accounts for", round(top_facilities_paid$percentage, 2), "% of total paid amount.", collapse = "<br>")
    HTML(sprintf("<div style='margin: 8px;'><div style='padding: 10px; background-color: #f9f9f9; border-radius: 5px;'>%s</div></div>", facilities_paid_text))
  })

  # Facility classification
  output$facility_classification <- renderPlotly({
    df <- merged_data() %>% group_by(facility_name) %>% summarise(Patients = sum(patientcount, na.rm = TRUE), Paid = sum(paid, na.rm = TRUE), .groups = "drop")
    big_facility_threshold <- quantile(df$Patients, 0.75, na.rm = TRUE)
    big_paid_threshold <- quantile(df$Paid, 0.75, na.rm = TRUE)
    df <- df %>% mutate(Classification = case_when(Patients >= big_facility_threshold & Paid >= big_paid_threshold ~ "Big Facility", TRUE ~ "Small Facility"))
    p <- ggplot(df, aes(x = Patients, y = Paid, color = Classification, text = paste("Facility:", facility_name))) +
      geom_point(size = 2) + theme_minimal() + labs(title = "Facility Size Classification", x = "Number of Patients", y = "Paid Amount") +
      scale_color_manual(values = c("Big Facility" = "#2C7BE5", "Small Facility" = "#E63757"))
    if (!is.null(input$facility_select) && length(input$facility_select) == 1) {
      selected_facility <- df %>% filter(facility_name == input$facility_select)
      p <- p + geom_point(data = selected_facility, aes(x = Patients, y = Paid), color = "#2EB85C", size = 3, shape = 4)
    }
    ggplotly(p, tooltip = "text")
  })

  # Odd claims detection with adjustable threshold
  output$odd_claims_table <- renderDT({
    df <- merged_data()
    req(nrow(df) > 0)
    q <- input$anomaly_quantile
    odd_facilities <- df %>% group_by(facility_name) %>% summarise(total_claims = sum(approvedquantity, na.rm = TRUE), total_paid = sum(paid, na.rm = TRUE), .groups = "drop") %>%
      filter(total_claims > quantile(total_claims, q, na.rm = TRUE) | total_paid > quantile(total_paid, q, na.rm = TRUE)) %>%
      mutate(flag = "Odd Behavior") %>% arrange(desc(total_paid))
    datatable(odd_facilities, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$odd_claims_narrative <- renderUI({
    HTML("<div style='margin: 8px;'><h4>Anomalies Detection</h4>
          <p>Facilities highlighted are above your selected quantile threshold for claims volume or payouts. Use the slider to tune sensitivity.</p>
          <p><b>Note:</b> High levels alone do not imply malpractice; they indicate cases warranting deeper review.</p></div>")
  })

  # Statistical analysis
  output$stat_analysis_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    regression_result <- lm(paid ~ approvedquantity + patientcount + clinician_name, data = df)
    plot_data <- tibble(Residuals = residuals(regression_result), predict = predict(regression_result))
    p <- ggplot(plot_data, aes(x = predict, y = Residuals)) + geom_point() + geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") + theme_minimal()
    ggplotly(p)
  })

  output$stat_analysis_narrative <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    lm_model <- lm(paid ~ approvedquantity + patientcount + clinician_name, data = df)
    lm_info <- tidy(lm_model)
    significant_terms <- lm_info %>% filter(p.value < 0.05)
    narrative <- if (nrow(significant_terms) > 0) {
      interpretation <- paste(sapply(significant_terms$term, function(term) {
        sprintf("The variable <b>%s</b> significantly influences the paid amounts (p=%.4f).", term, significant_terms$p.value[significant_terms$term == term])
      }), collapse = "<br>")
      sprintf("<p>The regression reveals predictors associated with paid amounts. Consider these when building forecasting and resource allocation models.</p>%s", interpretation)
    } else {
      "<p>No statistically significant predictors were found at p < 0.05 for this specification. Consider alternative features or interaction terms.</p>"
    }
    HTML(sprintf("<h4>Statistical Analysis Summary</h4><p>%s</p>", narrative))
  })

  # Code exploration plots (added hover text and consistent formatting)
  output$top_clinicians_quantity_plot <- renderPlotly({
    entities <- top_entities_by_quantity()
    top_clinicians <- entities$top_clinicians
    p <- ggplot(top_clinicians, aes(x = reorder(clinician_name, TotalQuantity), y = TotalQuantity, fill = clinician_name,
                                    text = paste("Clinician:", clinician_name, "<br>Quantity:", comma(TotalQuantity)))) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(title = paste("Top Clinicians by Approved Quantity for Code:", paste(input$code_select, collapse = ", ")), x = "Clinician", y = "Approved Quantity") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  output$top_facilities_quantity_plot <- renderPlotly({
    entities <- top_entities_by_quantity()
    top_facilities <- entities$top_facilities
    p <- ggplot(top_facilities, aes(x = reorder(facility_name, TotalQuantity), y = TotalQuantity, fill = facility_name,
                                    text = paste("Facility:", facility_name, "<br>Quantity:", comma(TotalQuantity)))) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(title = paste("Top Facilities by Approved Quantity for Code:", paste(input$code_select, collapse = ", ")), x = "Facility", y = "Approved Quantity") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # Placeholders for code_by_facility / code_by_clinician (optional detailed charts)
  output$code_by_facility <- renderPlotly({
    req(input$code_select)
    df <- filtered_code_data() %>% group_by(facility_name) %>% summarise(Paid = sum(paid, na.rm = TRUE), .groups = "drop") %>% arrange(desc(Paid))
    p <- ggplot(df, aes(x = reorder(facility_name, Paid), y = Paid, text = paste("Facility:", facility_name, "<br>Paid:", dollar(Paid)))) + geom_col(fill = "#2C7BE5") + coord_flip() + labs(x = "Facility", y = "Paid") + theme_minimal() + scale_y_continuous(labels = dollar)
    ggplotly(p, tooltip = "text")
  })
  output$code_by_clinician <- renderPlotly({
    req(input$code_select)
    df <- filtered_code_data() %>% group_by(clinician_name) %>% summarise(Paid = sum(paid, na.rm = TRUE), .groups = "drop") %>% arrange(desc(Paid))
    p <- ggplot(df, aes(x = reorder(clinician_name, Paid), y = Paid, text = paste("Clinician:", clinician_name, "<br>Paid:", dollar(Paid)))) + geom_col(fill = "#2EB85C") + coord_flip() + labs(x = "Clinician", y = "Paid") + theme_minimal() + scale_y_continuous(labels = dollar)
    ggplotly(p, tooltip = "text")
  })

  # Download report and data
  output$download_report <- downloadHandler(
    filename = function() paste("Market_Analysis_Report", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file, params = list(filtered_data = filtered_data()), envir = new.env(parent = globalenv()))
    }
  )

  output$download_filtered_csv <- downloadHandler(
    filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run app
shinyApp(ui, server)