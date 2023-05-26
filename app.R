library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(caret)
library(formattable)
library(DT)
library(knitr)
model <- readRDS("XYTE_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Fish Encounter Probability Prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV files", accept = c(".csv"), multiple = TRUE),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")
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
  # Load data from uploaded CSV file
  data <- reactive({
    req(input$file)
    files <- input$file
    data <- lapply(files$datapath, function(path) read.csv(path, header = input$header, sep = input$sep))
    dplyr::bind_rows(data)
  })
  
  merged_data <- reactive({
    req(data())
    newdata <- data()
    
    # Load the additional table data
    locations <- readRDS("locations.rds")
    
    # Perform left join by merging newdata with additional_data based on the common column "LOCATION"
    merged_data <- left_join(newdata, locations, by = "LOCATION", copy = TRUE)
    # Perform necessary mutations and column selection
    merged_data$RELEASE_DATE <- as.Date(merged_data$RELEASE_DATE)
    merged_data <- merged_data %>%
      mutate(ReleaseSeason = case_when(
        format(RELEASE_DATE, format = "%m") %in% c("9", "10", "11") ~ "pre-spawn",
        format(RELEASE_DATE, format = "%m") %in% c("12", "01", "02", "03") ~ "spawn",
        format(RELEASE_DATE, format = "%m") %in% c("04", "05") ~ "post-spawn"
      )) %>% select(SPECIES_ID, RELEASE_DATE, LOCATION, TL, MSCP_REACH, ReleaseSeason)
    merged_data$MSCP_REACH <- as.factor(merged_data$MSCP_REACH)
    merged_data
  })
  
  # Perform prediction using the GLM model
  prediction <- reactive({
    req(merged_data())
    merged_data <- merged_data()
    predictions <- predict(model, newdata = merged_data, type = "response", se.fit = TRUE)
    merged_data$fit <- predictions$fit
    merged_data$se.fit <- predictions$se.fit
    merged_data$lowerCI <- merged_data$fit - (1.96 * merged_data$se.fit)
    merged_data$upperCI <- merged_data$fit + (1.96 * merged_data$se.fit)
    merged_data
  })
  
  # Render the predicted results plot
  output$prediction_plot <- renderPlot({
    req(prediction())
    ggplot(prediction(), aes(x = fit, fill = MSCP_REACH)) + 
      geom_histogram(position = "identity", alpha = 0.75) +
      facet_grid(MSCP_REACH ~ .) + 
      theme(strip.text.y = element_blank(), axis.title.x = element_text(vjust = -5), axis.title.y = element_text(vjust = 5),
            plot.margin = margin(1,1,1,1, "cm")) +
      labs(x = 'Probability of Detection', y = 'Count')
  })
  
  output$prediction_plot2 <- renderPlot({
    req(prediction())
    ggplot(prediction(), aes(x = TL, y = fit, color = MSCP_REACH, fill = MSCP_REACH)) + 
      geom_line(linewidth = 1) + 
      geom_ribbon(aes(x = TL, ymin = lowerCI, ymax = upperCI), alpha = 0.3) + 
      theme(axis.title.x = element_text(vjust = -5), axis.title.y = element_text(vjust = 5),
            plot.margin = margin(1,1,1,1, "cm")) +
      labs(x = 'Size at Stocking (mm)', y = 'Probability of Detection')
  })
  
  # Perform Monte Carlo simulations
  mc_results <- reactive({
    req(prediction())
    num_simulations <- 1000
    mc_results <- replicate(num_simulations, rbinom(n = nrow(prediction()), size = 1, prob = prediction()$fit))
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
