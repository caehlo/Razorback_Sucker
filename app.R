library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(caret)
library(DT)
library(lubridate)
model <- readRDS("XYTE_reduced_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Fish Encounter Probability Prediction"),
  sidebarLayout(
    sidebarPanel(
      h3('Upload Stocking Files'),
      fileInput("file", "Upload", accept = c(".csv"), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      tags$p("Note: Please insure you have SPECIES_ID, TL, MSCP_REACH, and RELEASE_DATE column headings before uploading."),
      h3('Create hypothetical data'),
      selectInput("species_input", "Species:", choices = c("XYTE", "GIEL"), selected = "XYTE"),
      numericInput("mean_input", "Mean TL:", value = 0),
      numericInput("size_input", "Number Stocked:", value = 1000, min = 1),
      textInput("release_date_input", "Release Date:", value = "YYYY-MM-DD"),
      selectInput("mscp_reach_input", "MSCP Reach:", choices = c("2", "3", "4"), selected = "2"),
      actionButton("create_button", "Create Mock Data")
    ),
    mainPanel(
      dataTableOutput('monte_carlo_table'),
      plotOutput("prediction_plot"),
      plotOutput("prediction_plot2"),
    )
  )
)

# Define server
server <- function(input, output) {
  # Create dataframe based on user input or uploaded CSV file
  data <- eventReactive(input$create_button,{
    if (input$create_button) {
      set.seed(123)
      mean <- input$mean_input
      std_dev <- 48.2
      size <- input$size_input
      thresholds <- rlnorm(size, meanlog = log(mean), sdlog = log(1 + std_dev / mean))
      thresholds <- pmax(pmin(thresholds, 500), 300)
      RELEASE_DATE <- input$release_date_input
      validate(
        need(
          grepl("^\\d{4}-\\d{2}-\\d{2}$", RELEASE_DATE),
          "Invalid date format. Please use yyyy-mm-dd."
        )
      )
      
      df <- data.frame(
        TL = thresholds,
        RELEASE_DATE = RELEASE_DATE,
        MSCP_REACH = input$mscp_reach_input,
        SPECIES_ID = input$species_input
      )
      
      return(df)
    } else {
      req(input$file)
      files <- input$file
      import <- lapply(files$datapath, function(path) read.csv(path, header = input$header, sep = input$sep))
      dplyr::bind_rows(import)
    }
  })
  
  merged_data <- reactive({
    req(data())
    merged_data <- data()
    
    # Perform necessary mutations and column selection
    merged_data <- merged_data %>%
      mutate(RELEASE_DATE = parse_date_time(RELEASE_DATE, orders = c("ymd", "y/m/d", "mdy", "m/d/y")), ReleaseSeason = case_when(
        format(RELEASE_DATE, format = "%m") %in% c("9", "10", "11") ~ "pre-spawn",
        format(RELEASE_DATE, format = "%m") %in% c("12", "01", "02", "03") ~ "spawn",
        format(RELEASE_DATE, format = "%m") %in% c("04", "05") ~ "post-spawn"
      )) %>% select(SPECIES_ID, RELEASE_DATE, TL, MSCP_REACH, ReleaseSeason)
    merged_data$MSCP_REACH <- as.factor(merged_data$MSCP_REACH)
    merged_data
  })
  
  
  # Perform prediction using the GLM model
  prediction <- reactive({
    req(merged_data())
    merged_data <- merged_data()
    predictions <- predict(model, newdata = merged_data, type = "response")
    merged_data <- cbind(merged_data, predictions)
    print(merged_data)
    merged_data
  })
  
  # Render the predicted results plot
  output$prediction_plot <- renderPlot({
    req(prediction())
    ggplot(prediction(), aes(x = predictions, fill = MSCP_REACH)) + 
      geom_histogram(position = "identity", alpha = 0.75) +
      facet_grid(MSCP_REACH ~ .) + 
      theme(strip.text.y = element_blank(), axis.title.x = element_text(vjust = -5), axis.title.y = element_text(vjust = 5),
            plot.margin = margin(1,1,1,1, "cm")) +
      labs(x = 'Probability of Detection', y = 'Count')
  })
  
  output$prediction_plot2 <- renderPlot({
    req(prediction())
    ggplot(prediction(), aes(x = TL, y = predictions, color = MSCP_REACH)) + 
      geom_line(linewidth = 1) + 
      theme(axis.title.x = element_text(vjust = -5), axis.title.y = element_text(vjust = 5),
            plot.margin = margin(1,1,1,1, "cm")) +
      labs(x = 'Size at Stocking (mm)', y = 'Probability of Detection')
  })
  
  # Perform Monte Carlo simulations
  mc_results <- reactive({
    req(prediction())
    num_simulations <- 1000
    mc_results <- replicate(num_simulations, rbinom(n = nrow(prediction()), size = 1, prob = prediction()$predictions))
    mc_results
  })
  
  # Generate the table data
  table_data <- reactive({
    req(prediction(), mc_results())
    proportion_survived <- as.data.frame(rowMeans(mc_results()))
    colnames(proportion_survived) <- "prob"  # Assign column name "prob"
    print(proportion_survived)
    confidence_level <- 0.95
    conf_intervals <- t(apply(mc_results(), 1, function(x) {
      prop.test(sum(x), length(x), conf.level = confidence_level)$conf.int
    }))
    print(confidence_level)
    MC_final <- cbind(prediction()$TL, prediction()$MSCP_REACH, proportion_survived, conf_intervals)
    colnames(MC_final) <- c('TL', 'Reach', 'prob', 'LCI', 'UCI')
    MC_estimate <- MC_final %>% group_by(Reach) %>% 
      summarize(Stocked = n(), Est_Surv = round(sum(prob), 2), LCI = round(sum(LCI), 2), UCI = round(sum(UCI), 2)) %>% 
      mutate('Est. Surv. (95% CI)' = paste(Est_Surv, '(',LCI,'-',UCI,')')) %>%
      mutate(Perc_Surv = Est_Surv/Stocked) %>% mutate(Perc_Surv = round(Perc_Surv, 2)) %>%
      select(-Est_Surv, -LCI, -UCI)
    MC_estimate
  })
  
  # Render the formattable table
  output$monte_carlo_table <- DT::renderDataTable({
    req(table_data())
    datatable(table_data(), 
              rownames = FALSE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
