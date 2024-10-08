#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(MASS)

# Define R-squared calculation function
Rquared <- function(y, y_pred) {
  SS_res <- sum((y - y_pred)^2)
  SS_tot <- sum((y - mean(y_pred))^2)
  R_squared <- 1 - (SS_res / SS_tot)
  return(R_squared)
}

# Create the dataset
set.seed(123)
data_uncorrelated <- data.frame(
  x1 = rnorm(1000, mean = 10, sd = 5),
  x2 = rnorm(1000, mean = 10, sd = 5),
  x3 = rnorm(1000, mean = 10, sd = 5),
  x4 = rnorm(1000, mean = 10, sd = 5),
  x5 = rnorm(1000, mean = 10, sd = 5),
  x6 = rnorm(1000, mean = 10, sd = 5),
  x7 = rnorm(1000, mean = 10, sd = 5),
  x8 = rnorm(1000, mean = 10, sd = 5),
  x9 = rnorm(1000, mean = 10, sd = 5),
  x10 = rnorm(1000, mean = 10, sd = 5)
)

n <- 10
cor_mat <- diag(1,n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    cor_value <- runif(1, min = 0, max = 0.3)  # Random value between 0 and 0.3
    cor_mat[i, j] <- cor_value
    cor_mat[j, i] <- cor_value  # Symmetric matrix
  }
}

mean_vector <- rep(0,10)

predictors <- mvrnorm(n = 1000, mu = mean_vector, Sigma = cor_mat)
data_correlated <- as.data.frame(predictors)
colnames(data_correlated) <- paste0("x", 1:10)


data$y <- 10 + (2 * data$x1) + rnorm(1000, mean = 5, sd = 5)

# Define UI
ui <- fluidPage(
  titlePanel("Regression Model with Dynamic Inputs"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("participants", "Number of Participants (Training Set):", 
                  min = 10, max = 500, value = 50),
      sliderInput("n_variables", "Number of Predictors:", 
                  min = 1, max = 10, value = 5),
      sliderInput("max_power", "Maximum Power for x1:", 
                  min = 1, max = 10, value = 1),
      selectInput("data_type",
                  "Chose data type",
                  choices = c("Uncorrelated" = "uncorrelated",
                              "Correlated" = "correlated"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Training vs. Test",
                 plotOutput("trainingPlot"),
                 plotOutput("testPlot"),
                 verbatimTextOutput("train_r2"),
                 verbatimTextOutput("test_r2")
        ),
        tabPanel("1 vs. many Predictors",
                 plotOutput("manyTrainPlot"),
                 plotOutput("manyTestPlot"),
                 verbatimTextOutput("manyTrain_r2"),
                 verbatimTextOutput("manyTest_r2")
        ),
        tabPanel("Powers of x1",
                 plotOutput("powerTrainPlot"),
                 plotOutput("powerTestPlot"),
                 verbatimTextOutput("powerTrain_r2"),
                 verbatimTextOutput("powerTest_r2")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # chose data based on dropdown
  selected_data <- reactive({
    if (input$data_type == "uncorrelated") {
      data <- uncorrelated_data
      return(data)
    } else {
      data <- correlated_data
      return(data)
    }
  })

  
  # Reactive expression to create the training and test sets
  data_split <- reactive({
    set.seed(123)
    participants <- input$participants
    trainingset <- data[sample(1:nrow(data), participants), ]
    test_indices <- setdiff(1:nrow(data), rownames(trainingset))
    testset <- data[test_indices, ]
    list(trainingset = trainingset, testset = testset)
  })
  
  # Case 1: Training vs Test
  output$trainingPlot <- renderPlot({
    datasets <- data_split()
    trainingset1 <- datasets$trainingset
    testset1 <- datasets$testset
    
    lm1 <- lm(y ~ x1, data = trainingset1)
    trainingset1$y_pred <- predict(lm1, newdata = trainingset1)
    
    ggplot(trainingset1, aes(x = y, y = y_pred)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Training Set: Actual vs Predicted", x = "Actual Y", y = "Predicted Y") +
      theme_minimal()
  })
  
  output$testPlot <- renderPlot({
    datasets <- data_split()
    testset1 <- datasets$testset
    
    lm1 <- lm(y ~ x1, data = datasets$trainingset)
    testset1$y_pred <- predict(lm1, newdata = testset1)
    
    ggplot(testset1, aes(x = y, y = y_pred)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Test Set: Actual vs Predicted", x = "Actual Y", y = "Predicted Y") +
      theme_minimal()
  })
  
  output$train_r2 <- renderPrint({
    datasets <- data_split()
    lm1 <- lm(y ~ x1, data = datasets$trainingset)
    trainingset1 <- datasets$trainingset
    trainingset1$y_pred <- predict(lm1, newdata = trainingset1)
    R_squared_train <- Rquared(trainingset1$y, trainingset1$y_pred)
    paste("R² (Training):", R_squared_train)
  })
  
  output$test_r2 <- renderPrint({
    datasets <- data_split()
    lm1 <- lm(y ~ x1, data = datasets$trainingset)
    testset1 <- datasets$testset
    testset1$y_pred <- predict(lm1, newdata = testset1)
    R_squared_test <- Rquared(testset1$y, testset1$y_pred)
    paste("R² (Test):", R_squared_test)
  })
  
  # Case 2: 1 vs Many Predictors
  output$manyTrainPlot <- renderPlot({
    datasets <- data_split()
    trainingset2 <- datasets$trainingset
    testset2 <- datasets$testset
    
    ind_variables <- colnames(data)[1:input$n_variables]
    formula <- as.formula(paste("y ~", paste(ind_variables, collapse = " + ")))
    
    lm2 <- lm(formula, data = trainingset2)
    trainingset2$y_pred <- predict(lm2, newdata = trainingset2)
    
    ggplot(trainingset2, aes(x = y, y = y_pred)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Training Set: Many Predictors", x = "Actual Y", y = "Predicted Y") +
      theme_minimal()
  })
  
  output$manyTestPlot <- renderPlot({
    datasets <- data_split()
    testset2 <- datasets$testset
    
    ind_variables <- colnames(data)[1:input$n_variables]
    formula <- as.formula(paste("y ~", paste(ind_variables, collapse = " + ")))
    
    lm2 <- lm(formula, data = datasets$trainingset)
    testset2$y_pred <- predict(lm2, newdata = testset2)
    
    ggplot(testset2, aes(x = y, y = y_pred)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Test Set: Many Predictors", x = "Actual Y", y = "Predicted Y") +
      theme_minimal()
  })
  
  output$manyTrain_r2 <- renderPrint({
    datasets <- data_split()
    ind_variables <- colnames(data)[1:input$n_variables]
    formula <- as.formula(paste("y ~", paste(ind_variables, collapse = " + ")))
    lm2 <- lm(formula, data = datasets$trainingset)
    trainingset2 <- datasets$trainingset
    trainingset2$y_pred <- predict(lm2, newdata = trainingset2)
    R_squared_train <- Rquared(trainingset2$y, trainingset2$y_pred)
    paste("R² (Training - Many Predictors):", R_squared_train)
  })
  
  output$manyTest_r2 <- renderPrint({
    datasets <- data_split()
    ind_variables <- colnames(data)[1:input$n_variables]
    formula <- as.formula(paste("y ~", paste(ind_variables, collapse = " + ")))
    lm2 <- lm(formula, data = datasets$trainingset)
    testset2 <- datasets$testset
    testset2$y_pred <- predict(lm2, newdata = testset2)
    R_squared_test <- Rquared(testset2$y, testset2$y_pred)
    paste("R² (Test - Many Predictors):", R_squared_test)
  })
  
  # Case 3: Powers of x1
  output$powerTrainPlot <- renderPlot({
    datasets <- data_split()
    trainingset3 <- datasets$trainingset
    testset3 <- datasets$testset
    
    terms <- paste("I(x1^", 1:input$max_power, ")", sep = "")
    formula3 <- as.formula(paste("y ~", paste(terms, collapse = " + ")))
    
    lm3 <- lm(formula3, data = trainingset3)
    trainingset3$y_pred <- predict(lm3, newdata = trainingset3)
    
    ggplot(trainingset3, aes(x = y, y = y_pred)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Training Set: Powers of x1", x = "Actual Y", y = "Predicted Y") +
      theme_minimal()
  })
  
  output$powerTestPlot <- renderPlot({
    datasets <- data_split()
    testset3 <- datasets$testset
    
    terms <- paste("I(x1^", 1:input$max_power, ")", sep = "")
    formula3 <- as.formula(paste("y ~", paste(terms, collapse = " + ")))
    
    lm3 <- lm(formula3, data = datasets$trainingset)
    testset3$y_pred <- predict(lm3, newdata = testset3)
    
    ggplot(testset3, aes(x = y, y = y_pred)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Test Set: Powers of x1", x = "Actual Y", y = "Predicted Y") +
      theme_minimal()
  })
  
  output$powerTrain_r2 <- renderPrint({
    datasets <- data_split()
    terms <- paste("I(x1^", 1:input$max_power, ")", sep = "")
    formula3 <- as.formula(paste("y ~", paste(terms, collapse = " + ")))
    lm3 <- lm(formula3, data = datasets$trainingset)
    trainingset3 <- datasets$trainingset
    trainingset3$y_pred <- predict(lm3, newdata = trainingset3)
    R_squared_train <- Rquared(trainingset3$y, trainingset3$y_pred)
    paste("R² (Training - Powers of x1):", R_squared_train)
  })
  
  output$powerTest_r2 <- renderPrint({
    datasets <- data_split()
    terms <- paste("I(x1^", 1:input$max_power, ")", sep = "")
    formula3 <- as.formula(paste("y ~", paste(terms, collapse = " + ")))
    lm3 <- lm(formula3, data = datasets$trainingset)
    testset3 <- datasets$testset
    testset3$y_pred <- predict(lm3, newdata = testset3)
    R_squared_test <- Rquared(testset3$y, testset3$y_pred)
    paste("R² (Test - Powers of x1):", R_squared_test)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
