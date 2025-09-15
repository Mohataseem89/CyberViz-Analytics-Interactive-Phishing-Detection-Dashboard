# Install and load required packages
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", "dplyr", "viridis")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load and prepare data
file_path <- "paste your dataset path"
data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
clean_data <- data[complete.cases(data), ]

# Identify target and feature columns - assumes target is the last column
target_col <- colnames(clean_data)[ncol(clean_data)]
feature_cols <- setdiff(colnames(clean_data), target_col)
clean_data[[target_col]] <- as.factor(clean_data[[target_col]])

# Convert all data to numeric for correlation analysis
numeric_data <- data.frame(lapply(clean_data, as.numeric))
colnames(numeric_data) <- colnames(clean_data)

# Calculate correlations with target variable for feature importance
cor_matrix <- cor(numeric_data, use = "complete.obs")
target_cors <- cor_matrix[, target_col]
target_cors <- target_cors[names(target_cors) != target_col]

# Create feature importance ranking based on absolute correlation
feature_importance <- data.frame(
  Feature = names(target_cors), 
  Correlation = as.numeric(target_cors), 
  AbsCorrelation = abs(as.numeric(target_cors))
) %>% arrange(desc(AbsCorrelation))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "🔒 Cybersecurity Analytics Dashboard"),
  
  # Sidebar with navigation menu
  dashboardSidebar(sidebarMenu(
    menuItem("📊 Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("🎯 Feature Analysis", tabName = "features", icon = icon("chart-bar")),
    menuItem("🔗 Correlations", tabName = "correlations", icon = icon("project-diagram")),
    menuItem("📈 Interactive Plots", tabName = "interactive", icon = icon("chart-line"))
  )),
  
  # Main dashboard body with custom CSS styling
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif; 
        font-weight: bold; 
        font-size: 18px;
      } 
      .content-wrapper {
        background-color: #f4f4f4;
      }'))),
    
    tabItems(
      # Overview Tab - Dataset summary and basic statistics
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_samples"), 
                valueBoxOutput("total_features"), 
                valueBoxOutput("class_balance")
              ),
              fluidRow(
                box(title = "📋 Dataset Summary", status = "primary", solidHeader = TRUE, width = 6, 
                    verbatimTextOutput("dataset_info")), 
                box(title = "🎯 Classification Distribution", status = "success", solidHeader = TRUE, width = 6, 
                    plotlyOutput("target_distribution"))
              ),
              fluidRow(
                box(title = "📊 Sample Feature Distributions", status = "warning", solidHeader = TRUE, width = 12, 
                    plotlyOutput("feature_histograms"))
              )
      ),
      
      # Feature Analysis Tab - Feature importance and distributions
      tabItem(tabName = "features",
              fluidRow(
                box(title = "🏆 Top Important Features", status = "primary", solidHeader = TRUE, width = 6, 
                    plotlyOutput("feature_importance_plot")), 
                box(title = "📈 Feature Distribution by Class", status = "info", solidHeader = TRUE, width = 6, 
                    selectInput("selected_feature", "Select Feature:", choices = head(feature_importance$Feature, 20)), 
                    plotlyOutput("feature_by_class"))
              ),
              fluidRow(
                box(title = "📊 Top Features Table", status = "success", solidHeader = TRUE, width = 12, 
                    DT::dataTableOutput("top_features_table"))
              )
      ),
      
      # Correlations Tab - Correlation analysis and heatmaps
      tabItem(tabName = "correlations",
              fluidRow(
                box(title = "🔗 Correlation Heatmap", status = "primary", solidHeader = TRUE, width = 8, 
                    plotlyOutput("correlation_heatmap")), 
                box(title = "📊 Target Correlations", status = "warning", solidHeader = TRUE, width = 4, 
                    plotlyOutput("target_correlations"))
              ),
              fluidRow(
                box(title = "🎯 Feature Variance Analysis", status = "info", solidHeader = TRUE, width = 12, 
                    plotlyOutput("variance_plot"))
              )
      ),
      
      # Interactive Plots Tab - User-controlled visualizations
      tabItem(tabName = "interactive",
              fluidRow(
                box(title = "🎮 Interactive Scatter Plot", status = "info", solidHeader = TRUE, width = 6, 
                    selectInput("x_axis", "X-Axis Feature:", choices = feature_cols[1:20]), 
                    selectInput("y_axis", "Y-Axis Feature:", choices = feature_cols[1:20], selected = feature_cols[2]), 
                    plotlyOutput("scatter_plot")), 
                box(title = "📊 Dynamic Feature Comparison", status = "success", solidHeader = TRUE, width = 6, 
                    sliderInput("top_n_features", "Number of Features:", min = 5, max = 25, value = 15), 
                    plotlyOutput("dynamic_bar_chart"))
              ),
              fluidRow(
                box(title = "🎯 Class Distribution Analysis", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("class_analysis"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Value boxes for overview metrics
  output$total_samples <- renderValueBox({
    valueBox(value = nrow(clean_data), subtitle = "Total Samples", icon = icon("database"), color = "blue")
  })
  
  output$total_features <- renderValueBox({
    valueBox(value = length(feature_cols), subtitle = "Features", icon = icon("list"), color = "green")
  })
  
  # Calculate class balance ratio for imbalanced dataset detection
  output$class_balance <- renderValueBox({
    target_table <- table(clean_data[[target_col]])
    balance_ratio <- round(min(target_table) / max(target_table), 2)
    valueBox(
      value = paste0(balance_ratio * 100, "%"), 
      subtitle = "Class Balance", 
      icon = icon("balance-scale"), 
      color = if(balance_ratio > 0.7) "green" else if(balance_ratio > 0.4) "yellow" else "red"
    )
  })
  
  # Dataset information summary
  output$dataset_info <- renderText({
    paste(
      "Dataset Dimensions:", nrow(clean_data), "x", ncol(clean_data), 
      "\nTarget Variable:", target_col, 
      "\nFeature Count:", length(feature_cols), 
      "\nData Types: Numeric/Binary",
      "\nMissing Values:", sum(is.na(clean_data)), 
      "\nUnique Classes:", length(unique(clean_data[[target_col]])), 
      "\n"
    )
  })
  
  # Target variable distribution bar chart
  output$target_distribution <- renderPlotly({
    target_counts <- table(clean_data[[target_col]])
    plot_ly(
      x = names(target_counts), 
      y = as.numeric(target_counts), 
      type = "bar", 
      marker = list(color = viridis::viridis(length(target_counts))), 
      text = paste("Count:", as.numeric(target_counts)), 
      textposition = "outside"
    ) %>% 
      layout(
        title = "Classification Distribution", 
        xaxis = list(title = "Class"), 
        yaxis = list(title = "Count"), 
        showlegend = FALSE
      )
  })
  
  # Sample feature histograms (first 4 features)
  output$feature_histograms <- renderPlotly({
    sample_features <- feature_cols[1:4]
    plots <- list()
    
    for (i in 1:4) {
      feature_name <- sample_features[i]
      plots[[i]] <- plot_ly(
        x = clean_data[[feature_name]], 
        type = "histogram", 
        name = feature_name, 
        nbinsx = 10
      ) %>% 
        layout(
          title = paste("Distribution of", feature_name), 
          xaxis = list(title = feature_name), 
          yaxis = list(title = "Frequency")
        )
    }
    subplot(plots, nrows = 2, titleX = TRUE, titleY = TRUE)
  })
  
  # Feature importance plot based on correlation with target
  output$feature_importance_plot <- renderPlotly({
    top_features <- head(feature_importance, 15)
    plot_ly(
      y = reorder(top_features$Feature, top_features$AbsCorrelation), 
      x = top_features$AbsCorrelation, 
      type = "bar", 
      orientation = 'h', 
      marker = list(color = top_features$AbsCorrelation, colorscale = "Viridis"), 
      text = round(top_features$AbsCorrelation, 3), 
      textposition = "outside"
    ) %>% 
      layout(
        title = "Top 15 Most Important Features", 
        xaxis = list(title = "Absolute Correlation with Target"), 
        yaxis = list(title = "Features")
      )
  })
  
  # Feature distribution by class (box plot)
  output$feature_by_class <- renderPlotly({
    req(input$selected_feature)
    feature_data <- data.frame(
      Feature_Value = clean_data[[input$selected_feature]], 
      Class = clean_data[[target_col]]
    )
    plot_ly(feature_data, y = ~Feature_Value, color = ~Class, type = "box") %>% 
      layout(
        title = paste("Distribution of", input$selected_feature, "by Class"), 
        yaxis = list(title = input$selected_feature), 
        xaxis = list(title = "Class")
      )
  })
  
  # Correlation heatmap for top features
  output$correlation_heatmap <- renderPlotly({
    top_15_features <- head(feature_importance$Feature, 15)
    heatmap_data <- numeric_data[c(top_15_features, target_col)]
    cor_subset <- cor(heatmap_data, use = "complete.obs")
    
    plot_ly(
      z = ~cor_subset, 
      x = colnames(cor_subset), 
      y = colnames(cor_subset), 
      type = "heatmap", 
      colorscale = "RdBu", 
      reversescale = TRUE
    ) %>% 
      layout(
        title = "Top 15 Features Correlation Heatmap", 
        xaxis = list(tickangle = -45), 
        yaxis = list(tickangle = 0)
      )
  })
  
  # Target correlations bar chart (positive/negative correlations)
  output$target_correlations <- renderPlotly({
    top_10_cors <- head(feature_importance, 10)
    plot_ly(
      x = top_10_cors$Correlation, 
      y = reorder(top_10_cors$Feature, abs(top_10_cors$Correlation)), 
      type = "bar", 
      orientation = 'h', 
      marker = list(color = ifelse(top_10_cors$Correlation > 0, "green", "red"))
    ) %>% 
      layout(
        title = "Top 10 Correlations with Target", 
        xaxis = list(title = "Correlation"), 
        yaxis = list(title = "Features")
      )
  })
  
  # Feature importance table with priority classification
  output$top_features_table <- DT::renderDataTable({
    display_table <- head(feature_importance, 20) %>% 
      mutate(
        Correlation = round(Correlation, 4), 
        AbsCorrelation = round(AbsCorrelation, 4), 
        Priority = ifelse(AbsCorrelation > 0.3, "High", 
                          ifelse(AbsCorrelation > 0.1, "Medium", "Low"))
      )
    
    DT::datatable(display_table, options = list(pageLength = 15, scrollX = TRUE), class = 'cell-border stripe') %>% 
      DT::formatStyle('Priority', 
                      backgroundColor = DT::styleEqual(
                        c('High', 'Medium', 'Low'), 
                        c('lightgreen', 'lightyellow', 'lightcoral')
                      )
      )
  })
  
  # Feature variance analysis - identifies features with highest variation
  output$variance_plot <- renderPlotly({
    feature_vars <- sapply(clean_data[feature_cols], var, na.rm = TRUE)
    var_data <- data.frame(Feature = names(feature_vars), Variance = as.numeric(feature_vars)) %>% 
      arrange(desc(Variance)) %>% 
      head(20)
    
    plot_ly(
      x = reorder(var_data$Feature, var_data$Variance), 
      y = var_data$Variance, 
      type = "bar", 
      marker = list(color = var_data$Variance, colorscale = "Plasma")
    ) %>% 
      layout(
        title = "Top 20 Features by Variance", 
        xaxis = list(title = "Features", tickangle = -45), 
        yaxis = list(title = "Variance")
      )
  })
  
  # Interactive scatter plot with user-selected axes
  output$scatter_plot <- renderPlotly({
    req(input$x_axis, input$y_axis)
    scatter_data <- data.frame(
      X = clean_data[[input$x_axis]], 
      Y = clean_data[[input$y_axis]], 
      Class = clean_data[[target_col]]
    )
    plot_ly(scatter_data, x = ~X, y = ~Y, color = ~Class, type = "scatter", mode = "markers") %>% 
      layout(
        title = paste(input$x_axis, "vs", input$y_axis), 
        xaxis = list(title = input$x_axis), 
        yaxis = list(title = input$y_axis)
      )
  })
  
  # Dynamic bar chart with user-controlled number of features
  output$dynamic_bar_chart <- renderPlotly({
    req(input$top_n_features)
    top_n <- head(feature_importance, input$top_n_features)
    plot_ly(
      y = reorder(top_n$Feature, top_n$AbsCorrelation), 
      x = top_n$AbsCorrelation, 
      type = "bar", 
      orientation = 'h', 
      marker = list(color = rainbow(nrow(top_n)))
    ) %>% 
      layout(
        title = paste("Top", input$top_n_features, "Features"), 
        xaxis = list(title = "Absolute Correlation"), 
        yaxis = list(title = "Features")
      )
  })
  
  # Class distribution pie chart
  output$class_analysis <- renderPlotly({
    class_counts <- table(clean_data[[target_col]])
    plot_ly(
      labels = names(class_counts), 
      values = as.numeric(class_counts), 
      type = "pie", 
      hole = 0.4, 
      marker = list(colors = viridis::viridis(length(class_counts)))
    ) %>% 
      layout(
        title = "Cybersecurity Class Distribution (Pie Chart)", 
        annotations = list(list(text = "Classes", x = 0.5, y = 0.5, showarrow = FALSE))
      )
  })
}

# Launch the Shiny application
shinyApp(ui = ui, server = server)
