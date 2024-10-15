# Load necessary libraries
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(MASS)
library(rsconnect)

# Define UI for the application
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Linear Regression Simulation with Dynamic Inputs Version 2"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("data_type", "Choose Data Type:",
                  choices = list("Uncorrelated Data" = "uncorrelated", "Correlated Data" = "correlated")),
      
      numericInput("participants", "Number of Participants:", min = 10, max = 999, value = 20),
      
      numericInput("n_variables", "Number of Predictors:", min = 1, max = 10, value = 1),
      
      numericInput("max_power", "Power of x1:", min = 1, max = 3, value = 1),
      
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("How To",
                 h2("How to use this Applet"),
                 p("This applet has been programmed to visualize how different training datasets and assumptions influence the fit in subsequent test datasets. You can chose the number of participants, the number of predictors (there are a total of 10), and the maximum power of x1."),
                 p("The true formula that underlies the Y is: Y = 10 + 2*X1 + Error, meaning that adding additional predictors or powers adds spurious predictions to the model which are not there in the real data."),
                 p("In the tabs 'plots' and 'summary & R-squared', you can compare how your Y and Y_predicted compare to each other on a visual level as well as on a performance level using the metric R-squared.")
                 ),
        tabPanel("Plots",
                 h2("Plotting the actual Y versus the Predicted Y according to the parameter Estimates calculated"),
                 p("This plot displays where the actual Y of any given person is and where the model estimates Y based on the training and the model. The red line in the middle is where the dots would be if we had a perfect prediction (every Y is predicted perfectly by our parameter estimates).",
                   class="alert alert-info"),
                 plotOutput("trainPlot"),
                 plotOutput("testPlot"),
                 h2("Plotting the actual progression of Y and the prediction of Y according to the parameter Estimates calculated"),
                 p("These plots show how the true Y progresses in ascending order, and how the predicted Y compares to it. The more overfitted the training dataset is, the closer Y_pred follows Y in the training plot, but the more inaccurate it is in the test plot.",
                   class = "alert alert-info"),
                 plotOutput("trainPlot2"),
                 plotOutput("testPlot2")
        ),
        tabPanel("Summary & R-squared",
                 h2("Checking the Model fit according to the training data"),
                 p("The summary output displays if the training set falely assumes that some predictors are more relevant to fit the model than they are (anything other than X1 should not be significant)."),
                 verbatimTextOutput("modelSummary"),
                 p("The two R-squared values show how good the model fitted on the training data predicts the test data."),
                 verbatimTextOutput("rSquaredtrain"),
                 verbatimTextOutput("rSquaredtest")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Observe to enforce the dependency between n_variables and max_power
  observe({
    if (input$max_power > 1) {
      # If max_power > 1, force n_variables to 1 and disable it
      updateNumericInput(session, "n_variables", value = 1)
      disable("n_variables")
    } else {
      # If max_power is 1 or less, enable n_variables
      enable("n_variables")
    }
    
    if (input$n_variables > 1) {
      # If n_variables > 1, force max_power to 1 and disable it
      updateNumericInput(session, "max_power", value = 1)
      disable("max_power")
    } else {
      # If n_variables is 1 or less, enable max_power
      enable("max_power")
    }
  })
  
  # Reactive data generation based on user inputs
  data <- reactive({
    set.seed(123)
    
    if (input$data_type == "uncorrelated") {
      data_uncorrelated <- data.frame(matrix(rnorm(1000 * 10, mean = 10, sd = 5), ncol = 10))
      colnames(data_uncorrelated) <- paste0("x", 1:10)
      data <- data_uncorrelated
    } else {
      n <- 10
      cor_mat <- diag(1, n)
      for (i in 1:(n-1)) {
        for (j in (i+1):n) {
          cor_value <- runif(1, min = 0, max = 0.3)  
          cor_mat[i, j] <- cor_value
          cor_mat[j, i] <- cor_value
        }
      }
      predictors <- mvrnorm(n = 1000, mu = rep(0, 10), Sigma = cor_mat)+10
      data_correlated <- as.data.frame(predictors)
      colnames(data_correlated) <- paste0("x", 1:10)
      data <- data_correlated
    }
    
    # Add the response variable y
    data$y <- 10 + 2 * data$x1 + rnorm(n = 1000, mean = 0, sd = 2)
    data
  })
  
  # Run analysis on button click
  observeEvent(input$run_analysis, {
    participants <- input$participants
    n_variables <- input$n_variables
    max_power <- input$max_power
    
    # Generate training and test sets
    data <- data()
    trainingsset <- data[sample(1:nrow(data), participants), ]
    test_indices <- setdiff(1:nrow(data), rownames(trainingsset))  # Ensures non-overlapping
    testset <- data[test_indices, ]
    
    # Create formula based on user input
    if (n_variables == 1 && max_power > 1) {
      formula <- as.formula(paste("y ~", paste0("I(x1^", 1:max_power, ")", collapse = " + ")))
    } else {
      ind_variables <- paste0("x", 1:n_variables)
      formula <- as.formula(paste("y ~", paste(ind_variables, collapse = " + ")))
    }
    
    # Fit the model
    lm_model <- lm(formula, data = trainingsset)
    
    # Predictions for training and test sets
    trainingsset$y_pred <- predict(lm_model, newdata = trainingsset)
    testset$y_pred <- predict(lm_model, newdata = testset)
    
    ## RSQ Formula
    Rquared <- function(y,y_pred) {
      SS_res <- sum((y - y_pred)^2)
      SS_tot <- sum((y - mean(y_pred))^2)
      
      R_squared <- 1 - (SS_res / SS_tot)
      return(R_squared)
    }
    # R-squared
    train_rsq <- Rquared(trainingsset$y,trainingsset$y_pred)
    test_rsq <- Rquared(testset$y,testset$y_pred)
    
    # Regression Summary
    output$modelSummary <- renderPrint({
      summary(lm_model)
    })
    
    output$rSquaredtrain <- renderPrint({
      paste("Training Set R-squared:", round(train_rsq, 4))
    })
    
    output$rSquaredtest <- renderPrint({
      paste("Test Set R-squared:",round(test_rsq,4))
    })
    
    ## Create X
    testset <- testset %>% dplyr::arrange(y)
    testset$x <- c(1:length(testset$y))
    trainingsset <- trainingsset %>% dplyr::arrange(y)
    trainingsset$x <- c(1:length(trainingsset$y))
    
    # Plot for training set
    output$trainPlot <- renderPlot({
      ggplot(trainingsset, aes(x = y, y = y_pred)) +
        geom_point(color = "blue") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Training Set: Actual vs Predicted", x = "Actual Y", y = "Predicted Y") +
        theme_minimal()
    })
    
    # Plot for test set
    output$testPlot <- renderPlot({
      ggplot(testset, aes(x = y, y = y_pred)) +
        geom_point(color = "blue") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Test Set: Actual vs Predicted", x = "Actual Y", y = "Predicted Y") +
        theme_minimal()
    })
    
    output$trainPlot2 <- renderPlot({
      ggplot(trainingsset, aes(x = x)) +
        geom_line(aes(y = y, color = "y"), size = 1) +       # Line for y
        geom_line(aes(y = y_pred, color = "y_pred"), size = 1) +  # Line for y_pred
        labs(x = "Index", y = "Values", color = "Legend") +  # Label axes and legend
        theme_minimal() +
        ggtitle("Training Set: Actual vs Predicted")
    })
    
    output$testPlot2 <- renderPlot({
      ggplot(testset, aes(x = x)) +
        geom_line(aes(y = y, color = "y"), size = 1) +       # Line for y
        geom_line(aes(y = y_pred, color = "y_pred"), size = 1) +  # Line for y_pred
        labs(x = "Index", y = "Values", color = "Legend") +  # Label axes and legend
        theme_minimal() +
        ggtitle("Training Set: Actual vs Predicted")
    })

  })
}

# Run the application 
shinyApp(ui = ui, server = server)
