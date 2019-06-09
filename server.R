library(shiny)
library(DT)
library(ggplot2)
# We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(doParallel)

# This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(caret)

# The libraries below implement method specific algorithms. This list will change as methods are added / removed
library(pls)
library(nnet)
library(glmnet)
library(mboost)
library(plyr)
library(import)
library(AdaptGauss)
library(kernlab)
library(brnn)
library(foba)
library(e1071)
library(randomForest)
library(import)
library(foreach)




shinyServer(function(input, output, session) {
  
  getData <- reactive({
    read.csv(file="Ass3Data.csv", row.names = "ID")
  })

  ############################################################################## 
  getTrControl <- reactive({
    # shared cross validation specification
    y <- getData()[,"Y"]
    trainControl("cv", number = 10, allowParallel = TRUE, repeats = 5)  
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    boxplot(d[,numeric], outline=TRUE, main=paste("Boxplot using IQR multiplier of",input$Multiplier), range = input$Multiplier)
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  
  
  ##############################################################################  
  getNullModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "null"
    showNotification(id = method, paste("Assessing", method, "model using cross-validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, preProcess = NULL, 
                    metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$NullModelSummary2 <- renderPrint({
    print(getNullModel())
  })
  
  
  
  ##############################################################################  
  getGlmModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)

    method <- "glmnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross-validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, preProcess = input$GlmPreprocess,
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$GlmModelSummary1 <- renderTable({
    mods <- getGlmModel()
    as.data.frame(mods$bestTune) 
  })  
  
  ##################################################################### ######### 
  output$GlmModelPlots <- renderPlot({
    plot(getGlmModel())
  })     
  
  ############################################################################## 
  output$GlmModelSummary2 <- renderPrint({
    print(getGlmModel())
  })
  
  
  ##############################################################################
  getPlsModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)  # this will work on windows
    
    method <- "pls"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    
    mods <- train(Y ~ ., data = getTrainData(), method = method, preProcess = input$PlsPreprocess, 
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$PlsModelSummary1 <- renderTable({
    mods <- getPlsModel()
    as.data.frame(mods$bestTune)
  })  
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModel())
  })     
  
  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    mods <- getPlsModel()
    summary(mods$finalModel)
  })
  
  
  ##############################################################################
  getNnetModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "nnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, preProcess = input$NnetPreprocess, 
                         maxit = 1000, trace = F, linout = 1, 
                         metric = "RMSE", trControl = getTrControl(), tuneLength = 6) 
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$NnetModelSummary1 <- renderTable({
    mods <- getNnetModel()
    as.data.frame(mods$bestTune)
  })  
  
  ############################################################################## 
  output$NnetModelPlots <- renderPlot({
    plot(getNnetModel())
  })     
  
  ############################################################################## 
  output$NnetModelSummary2 <- renderPrint({
    mods <- getNnetModel()
    print(mods$finalModel)
  })
  
  
  #########MYMODEL WORK STARTS################
  
  
  ###Model 1 ## Gaussian Process with Polynomial Kernel
  getGusModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "gaussprPoly"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross-validation"), session = session, duration = NULL)
    
    mods <- train(Y ~., data = getTrainData(), method = method, preProcess = input$GusPreprocess,
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ##############################################################################
  output$GusModelSummary1 <- renderTable({
    mods <- getGusModel()
    as.data.frame(mods$bestTune)
  })
  
  ##################################################################### #########
  output$GusModelPlots <- renderPlot({
    plot(getGusModel())
  })
  
  ##############################################################################
  output$GusModelSummary2 <- renderPrint({
    print(getGusModel())
  })
  
  ##############################################################################
  
  ##My Model 2 Adaptive Mixture Discriminant Analysis
  
  
  getCubModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "cubist"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross-validation"), session = session, duration = NULL)
    
    mods <- train(Y ~., data = getTrainData(), method = method, preProcess = input$CubPreprocess,
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$CubModelSummary1 <- renderTable({
    mods <- getCubModel()
    as.data.frame(mods$bestTune) 
  })  
  
  ##################################################################### ######### 
  output$CubModelPlots <- renderPlot({
    plot(getCubModel())
  })     
  
  ############################################################################## 
  output$CubModelSummary2 <- renderPrint({
    print(getCubModel())
  })
  
  
  ##########my MODEL 3 Regularization -Bayesian Regularized Neural Networks######
  
  
  
  getBrnModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "brnn"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross-validation"), session = session, duration = NULL)
    
    mods <- train(Y ~., data = getTrainData(), method = method, preProcess = input$BrnPreprocess,
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$BrnModelSummary1 <- renderTable({
    mods <- getBrnModel()
    as.data.frame(mods$bestTune) 
  })  
  
  ##################################################################### ######### 
  output$BrnModelPlots <- renderPlot({
    plot(getBrnModel())
  })     
  
  ############################################################################## 
  output$BrnModelSummary2 <- renderPrint({
    print(getBrnModel())
  })
  
  
  
  ######Mymodel work 4 - Ridge Regression with Variable Selection#######
  
  getRrvModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "foba"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross-validation"), session = session, duration = NULL)
    
    mods <- train(Y ~., data = getTrainData(), method = method, preProcess = input$RrvPreprocess,
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$RrvModelSummary1 <- renderTable({
    mods <- getRrvModel()
    as.data.frame(mods$bestTune) 
  })  
  
  ##################################################################### ######### 
  output$RrvModelPlots <- renderPlot({
    plot(getRrvModel())
  })     
  
  ############################################################################## 
  output$RrvModelSummary2 <- renderPrint({
    print(getRrvModel())
  })
  
  
  
  
  
  
  
  ######Mymodel work 5- Parallel Random Forest#######
  
  getPrfModel <- reactive({
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    
    method <- "parRF"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross-validation"), session = session, duration = NULL)
    
    mods <- train(Y ~., data = getTrainData(), method = method, preProcess = input$PrfPreprocess,
                  metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
    removeNotification(id=method)
    stopCluster(clus)
    registerDoSEQ()
    mods
  })
  
  ############################################################################## 
  output$PrfModelSummary1 <- renderTable({
    mods <- getPrfModel()
    as.data.frame(mods$bestTune) 
  })  
  
  ##################################################################### ######### 
  output$PrfModelPlots <- renderPlot({
    plot(getPrfModel())
  })     
  
  ############################################################################## 
  output$PrfModelSummary2 <- renderPrint({
    print(getPrfModel())
  })
  
  
  
  
  
  
  
  
  
  #####################MYMODEL WORKS ENDS#######################
  
  
  
  
  
  
  
  
  
  
  ##############################################################################  
  getAllModels <- reactive({
    list("NULL" = getNullModel(), GLMnet=getGlmModel(), PLS=getPlsModel(), NNET=getNnetModel(), GAUSSIAN = getGusModel() ,TREE= getCubModel() , BRN = getBrnModel(), RRV=getRrvModel(), PRF =getPrfModel())  # expand this list with further models
  })
  
  ############################################################################## 
  output$SelectionSummary <- renderPrint({
    results <- resamples(getAllModels())
    summary(results)
  })
  
  
 
  
  
  ############################################################################## 
  getResamples <- reactive({
    mods <- caret::resamples(getAllModels())
    NullModel <- "NULL"

    #scale metrics using null model. Tough code to follow -sorry
    if(input$NullNormalise & NullModel %in% mods$models) {
      actualNames <- colnames(mods$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep="~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(mods$values[, col], na.rm = TRUE)
          if(!is.na(nullMetric) & nullMetric != 0) {
            for (model in mods$models) {
              mcol <- paste(sep="~", model, metric)
              if(mcol %in% actualNames) {
                mods$values[, mcol] <- mods$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    mods
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mods <- getResamples()
    bwplot(mods, notch=input$Notch)
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    mod <- getAllModels()[input$Choice]
    predictions <- predict(mod, newdata=test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)
  
})
