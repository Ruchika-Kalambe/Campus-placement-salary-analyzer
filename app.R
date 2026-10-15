
library(shiny)

# -----------------------------------------
# Campus Placement and Salary Analyzer
# -----------------------------------------

placement_data <- data.frame(
  ID = 1:24,
  CGPA = c(8.5, 7.8, 9.1, 6.9, 8.0, 7.2, 6.5, 8.8,
           7.5, 8.2, 6.8, 7.0, 8.7, 7.6, 6.4, 8.1,
           9.0, 7.9, 6.7, 7.3, 8.4, 7.1, 6.2, 8.6),
  Branch = rep(c("CSE", "ECE", "IT"), each = 8),
  Gender = rep(c("Male", "Female"), 12),
  Status = c(
    "Placed", "Placed", "Placed", "Not Placed",
    "Placed", "Placed", "Not Placed", "Placed",
    "Placed", "Placed", "Not Placed", "Not Placed",
    "Placed", "Placed", "Not Placed", "Placed",
    "Placed", "Placed", "Not Placed", "Not Placed",
    "Placed", "Placed", "Not Placed", "Placed"
  ),
  Package = c(
    8.2, 6.5, 10.5, NA, 7.0, 5.8, NA, 9.2,
    6.0, 7.5, NA, NA, 9.0, 6.2, NA, 7.8,
    10.0, 7.0, NA, NA, 8.5, 5.5, NA, 9.5
  )
)

placed_data <- subset(placement_data, Status == "Placed")

# User Interface
ui <- navbarPage(
  title = "Campus Placement Analyzer",
  
  tabPanel(
    "Dashboard",
    sidebarLayout(
      sidebarPanel(
        h4("Analysis Settings"),
        sliderInput(
          "cgpa", "Enter CGPA for prediction:",
          min = 5, max = 10, value = 8, step = 0.1
        ),
        helpText("Salary packages are measured in LPA."),
        helpText("Sample dataset: 24 students.")
      ),
      mainPanel(
        h3("Placement Overview"),
        fluidRow(
          column(4, wellPanel(
            h4("Total Students"),
            h2(nrow(placement_data))
          )),
          column(4, wellPanel(
            h4("Placed Students"),
            h2(nrow(placed_data))
          )),
          column(4, wellPanel(
            h4("Mean Package"),
            h2(paste(round(mean(placed_data$Package), 2), "LPA"))
          ))
        ),
        h4("Student Dataset"),
        tableOutput("dataset")
      )
    )
  ),
  
  tabPanel(
    "P3: Confidence Interval",
    h3("95% Confidence Interval for Mean Package"),
    p("The interval estimates the population mean package."),
    verbatimTextOutput("confidence"),
    tableOutput("confidence_table")
  ),
  
  tabPanel(
    "P2: Sampling Distribution",
    h3("Sampling Distribution of Mean Package"),
    p("The histogram displays 1,000 bootstrap sample means."),
    plotOutput("sampling_plot"),
    verbatimTextOutput("sampling_summary")
  ),
  
  tabPanel(
    "P6: Regression",
    h3("CGPA vs Salary Package"),
    plotOutput("regression_plot"),
    verbatimTextOutput("regression_summary"),
    h4("Predicted Package"),
    textOutput("prediction")
  ),
  
  tabPanel(
    "P7: Chi-Square Test",
    h3("Branch and Placement Status"),
    tableOutput("branch_table"),
    verbatimTextOutput("chi_result")
  ),
  
  tabPanel(
    "P9: Mann-Whitney Test",
    h3("Male vs Female Salary Packages"),
    tableOutput("gender_summary"),
    verbatimTextOutput("mw_result")
  )
)

# Server Logic
server <- function(input, output, session) {
  
  output$dataset <- renderTable({
    placement_data
  }, digits = 2)
  
  # P3: Confidence Interval
  output$confidence <- renderPrint({
    result <- t.test(placed_data$Package, conf.level = 0.95)
    cat("Mean Package:", round(mean(placed_data$Package), 3), "LPA\n")
    cat("95% Confidence Interval:\n")
    print(round(result$conf.int, 3))
  })
  
  output$confidence_table <- renderTable({
    result <- t.test(placed_data$Package)
    data.frame(
      Measure = c("Mean Package", "Lower Limit", "Upper Limit"),
      Value = round(c(
        mean(placed_data$Package),
        result$conf.int[1],
        result$conf.int[2]
      ), 3)
    )
  })
  
  # P2: Sampling Distribution
  sample_means <- reactive({
    set.seed(123)
    replicate(1000, mean(sample(
      placed_data$Package,
      size = nrow(placed_data),
      replace = TRUE
    )))
  })
  
  output$sampling_plot <- renderPlot({
    hist(
      sample_means(),
      main = "Sampling Distribution of Mean Package",
      xlab = "Sample Mean Package (LPA)",
      col = "skyblue",
      border = "white"
    )
    abline(v = mean(placed_data$Package),
           col = "red", lwd = 2)
    legend("topright", "Observed Mean",
           col = "red", lwd = 2)
  })
  
  output$sampling_summary <- renderPrint({
    cat("Number of sample means:", length(sample_means()), "\n")
    cat("Mean of sample means:",
        round(mean(sample_means()), 3), "LPA\n")
  })
  
  # P6: Linear Regression
  regression_model <- lm(Package ~ CGPA, data = placed_data)
  
  output$regression_plot <- renderPlot({
    plot(
      placed_data$CGPA, placed_data$Package,
      main = "CGPA vs Salary Package",
      xlab = "CGPA",
      ylab = "Package (LPA)",
      pch = 19,
      col = "blue"
    )
    abline(regression_model, col = "red", lwd = 2)
  })
  
  output$regression_summary <- renderPrint({
    print(summary(regression_model))
  })
  
  output$prediction <- renderText({
    predicted <- predict(
      regression_model,
      newdata = data.frame(CGPA = input$cgpa)
    )
    paste("Predicted package for CGPA",
          input$cgpa, "=", round(predicted, 3), "LPA")
  })
  
  # P7: Chi-Square Test
  output$branch_table <- renderTable({
    table(placement_data$Branch, placement_data$Status)
  }, rownames = TRUE)
  
  output$chi_result <- renderPrint({
    result <- chisq.test(
      table(placement_data$Branch, placement_data$Status),
      simulate.p.value = TRUE,
      B = 10000
    )
    print(result)
    if (result$p.value < 0.05) {
      cat("\nDecision: Reject H0")
    } else {
      cat("\nDecision: Fail to reject H0")
    }
  })
  
  # P9: Mann-Whitney U Test
  male_packages <- subset(
    placed_data, Gender == "Male"
  )$Package
  
  female_packages <- subset(
    placed_data, Gender == "Female"
  )$Package
  
  output$gender_summary <- renderTable({
    data.frame(
      Gender = c("Male", "Female"),
      Count = c(length(male_packages), length(female_packages)),
      Median_Package = c(
        median(male_packages),
        median(female_packages)
      )
    )
  })
  
  output$mw_result <- renderPrint({
    result <- wilcox.test(
      male_packages, female_packages,
      alternative = "two.sided",
      exact = FALSE
    )
    print(result)
    if (result$p.value < 0.05) {
      cat("\nDecision: Reject H0")
    } else {
      cat("\nDecision: Fail to reject H0")
    }
  })
}

# Launch Application
shinyApp(ui = ui, server = server)
