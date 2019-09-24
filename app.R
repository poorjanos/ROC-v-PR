library(shiny)
library(mlbench)
library(PRROC)



# Load data ---------------------------------------------------------------
data("PimaIndiansDiabetes")
PimaIndiansDiabetes$diabetes <- as.numeric(PimaIndiansDiabetes$diabetes)
PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes == 1, "diabetes"] <- 0
PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes == 2, "diabetes"] <- 1
names(PimaIndiansDiabetes)[names(PimaIndiansDiabetes) == "diabetes"] <- "y"


# Split 0 and 1 labels
label_0 <- PimaIndiansDiabetes[PimaIndiansDiabetes$y == 0, ]
label_1 <- PimaIndiansDiabetes[PimaIndiansDiabetes$y == 1, ]


# User interface ----------------------------------------------------------
ui <- fluidPage(
  titlePanel("ROC v PR"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        class = "header", checked = NA,
        tags$strong("Compare ROC and PR Curves"),
        tags$p("Use slider below to alter skewness in dataset. The app fits a logistic
                      regression to the adjusted dataset then returns ROC and PR curves on the 
                      test set."),
        tags$p("The dataset is the Pima Indians Diabetes dataset available in the mlbench package.
                      The dataset contains 768 observations and 9 variables. Observations are individuals.
                      Variables are health-related metrics. 8 variables are continuous predictors and 1
                      variable is a binary response telling if the individual has diabetes or not."),
        tags$p("Positive class rate is the proportion of individuals with diabetes in the dataset."),
        tags$br(),
        sliderInput("balanceInput", "Positive Class Rate",
          min = 0, max = 0.34, value = 0.34
        )
      )
    ),
    mainPanel(
      plotOutput("ROC"),
      plotOutput("PR")
    )
  )
)


# Server ------------------------------------------------------------------------
server <- function(input, output) {
  rebalanced <- reactive({

    # Gen rebalanced dataset
    label_1_rate <- input$balanceInput
    sample_size <- round(label_1_rate * nrow(PimaIndiansDiabetes), 0)

    sample_ind <- sample(seq_len(nrow(label_1)), size = sample_size)
    label_1_rebalanced <- label_1[sample_ind, ]

    data <- rbind(label_0, label_1_rebalanced)

    # Train test split
    smp_size <- floor(0.8 * nrow(data))

    set.seed(123)
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    test <- data[-train_ind, ]

    # Fit glm
    fit <- glm(y ~ ., data = train, family = "binomial")

    # Predict test
    preds <- predict(fit, test, type = "response")

    # Return predicted probabilities
    data.frame(preds = preds, y = test$y)
  })


  output$ROC <- renderPlot({
    roc <- roc.curve(scores.class0 = rebalanced()$preds, weights.class0 = rebalanced()$y, curve = TRUE)
    plot(roc)
  })

  output$PR <- renderPlot({
    pr <- pr.curve(scores.class0 = rebalanced()$preds, weights.class0 = rebalanced()$y, curve = TRUE)
    plot(pr)
  })
}

shinyApp(ui, server)