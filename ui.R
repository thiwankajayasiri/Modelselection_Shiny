library(shiny)
library(DT)

ppchoices <- c("knnImpute", "bagImpute", "BoxCox", "spatialSign", "corr", "medianImpute", 
               "pca", "ica", "center", "scale", "YeoJohnson", "nzv" )

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Thiwanka"),
  
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             sliderInput("Multiplier", "IQR multiplier", min = 0, max=10, step = 0.1, value = 1.5),
             plotOutput("BoxPlots"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max=1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("NULL Model",
             verbatimTextOutput("NullModelSummary2")
    ),
    tabPanel("GLMnet Model",
             selectizeInput(inputId = "GlmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("GlmModelSummary1"),
             hr(),
             plotOutput("GlmModelPlots"),
             verbatimTextOutput("GlmModelSummary2")
    ),
    tabPanel("PLS Model",
             selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("NNET Model",
             selectizeInput(inputId = "NnetPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("NnetModelSummary1"),
             hr(),
             plotOutput("NnetModelPlots"),
             verbatimTextOutput("NnetModelSummary2")
    ),    

    tabPanel("Gaussian Model",
             selectizeInput(inputId = "GusPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("GusModelSummary1"),
             hr(),
             plotOutput("GusModelPlots"),
             verbatimTextOutput("GusModelSummary2")
    ),

    tabPanel("Model Tree",
             selectizeInput(inputId = "CubPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("CubModelSummary1"),
             hr(),
             plotOutput("CubModelPlots"),
             verbatimTextOutput("CubModelSummary2")
    ),

    tabPanel("Bayesian Regularized ",
             selectizeInput(inputId = "BrnPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("BrnModelSummary1"),
             hr(),
             plotOutput("BrnModelPlots"),
             verbatimTextOutput("BrnModelSummary2")
    ),
    
    tabPanel("Ridge Regression -VB",
             selectizeInput(inputId = "RrvPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("Best tuning parameters:"),
             tableOutput("RrvModelSummary1"),
             hr(),
             plotOutput("RrvModelPlots"),
             verbatimTextOutput("RrvModelSummary2")
    ),


    tabPanel("Parallel Random Forest",
             selectizeInput(inputId = "PrfPreprocess", label = "Pre-processing", choices = ppchoices, multiple = TRUE),
             tags$h5("MODEL 5 work:"),
             tableOutput("PrfModelSummary1"),
             hr(),
             plotOutput("PrfModelPlots"),
             verbatimTextOutput("PrfModelSummary2")
    ),

    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             checkboxInput("NullNormalise", "Normalise", value = TRUE),
             plotOutput("SelectionBoxPlot"),
             # Keep the choices below in-line with the models you are creating (always leave the null model off)
             radioButtons("Choice", "Model choice", choices = c("GLMnet", "PLS", "NNET", "GAUSSIAN", "TREE", "BRN", "RRV","PRF"), selected = "PLS"),
             hr(),
             tags$h3("Model Selection Approach:"),
             tags$br(),
             tags$p(" Specification of the correct model depends on way of measuring the proper variables."),
             
             tags$div(
               tags$a(href = "https://blog.minitab.com/blog/how-to-choose-the-best-regression-model","Ref Link!"),
                     tags$br(),
               "Once it's come to model selection , We could take the following in to consideration,",
               tags$br(),
               
               tags$ol(
                 tags$li("Too few: An underspecified model tends to produce biased estimates"), 
                 tags$li("Too many: An overspecified model tends to have less precise estimates."), 
                 tags$li("Just right: A model with the correct terms has no bias and the most precise estimates")
               ),
               
               tags$p(" By taking above points in to consideration and also the performance of the reference models provided, I explored through specific parameter tunings under models in  regression category,
                      , always selected one with parameter tuning as I observed the some of the models under parameter tuning are not performing well. E.g. Gaussian model only without taking parameter tuning doesn't perform well
                      . Even looking at the data it show case that it's a gaussian mixture distribution."),
               
               tags$p("I've tested few of the Generalized aditive models in a seprate r-script notebook but it had issue interms with the archived packages hence shifted to the chosen and seems givinig good results, this should under perform in many cases
                      in the real world problems, hence it's worth to look at the data as well and think about the models that may work, The best thing is to look at the multicollinearity, some of the advance varint model currently being used are to address the issues such as sensitivity to multicollinearity and outliers to avoid overfitting are,
                      Ridge regression, Lasso Regression and Partial least square regression ; although these models have distinguish characteristics, some of models currently been used my model selection are spin-offs from those principles.
                      
                      "),
               
               tags$p("To avoid some of the common mistakes it's always best to check out on the residual plots, it's quick and easy way to check for the problems in the regresion model.However , in this case under the reactive function getTrcontrol : Each model is automatically tuned and is evaluated using 5 repeats of 10-fold cross validation."),
               
               tags$p("Choosing the correct regression model is one issue , while choosing the right type of regression analysis for the data is entirely different matter.")
               
               
             )
             
    ),
    
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
  )
))
