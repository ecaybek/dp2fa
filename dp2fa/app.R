##### Loading Packages #####

library(shiny)
library(shinydashboard)
library(MVN)
library(mice)
library(BaylorEdPsych)
library(foreign)
library(imputeTS)
library(report)

##### UI #####
ui <- dashboardPage(
  dashboardHeader(title = "Data Preparation 2 FA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Cite", tabName = "cite", icon = icon("th")),
      menuItem("Manual", tabName = "manual", icon = icon("book-open"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title = "User Input", 
                    status = "primary", 
                    solidHeader = TRUE,
                    fileInput("file", "Upload your data [sav or csv]"),
                    uiOutput("firstItem"),
                    selectInput("cutoff", 
                                "Alpha for Multivariate Outliers",
                                choices = c(0.001, 0.01, 0.05), 
                                selected = 1),
                    checkboxInput("fixmissing", 
                                  "Impute missing values?", 
                                  value = TRUE),
                    fluidRow(
                      column(6, uiOutput("run")),
                      column(6, uiOutput("dlButton"))
                    )
                ),
                box(title = "Results", 
                    status = "warning", 
                    solidHeader = TRUE,
                    verbatimTextOutput("little"),
                    verbatimTextOutput("md")
                )
              )
      ),
      tabItem(tabName = "cite",
              fluidRow(
                box(title = "How to Cite?", 
                    status = "primary", 
                    solidHeader = TRUE,
                    print("Please cite this work as:"),
                    br(),
                    br(),
                    print(strong("Aybek, E.C. (2021) Data preparation for factor 
                              analysis. URL: https://shiny.eptlab.com/dp2fa/")),
                    br(),
                    br(),
                    print("This app uses following packages:"),
                    br(),
                    print("shiny, shinydashboard, mice, BaylorEdPsych, 
                              mvnmle, MVN, foreign, imputeTS"),
                    br(),
                    verbatimTextOutput("citation")
                )
              )),
      tabItem(tabName = "manual",
              fluidRow(
                box(title = "Manual", 
                    status = "primary", 
                    solidHeader = TRUE,
                    print(strong("This application 
                                     basically runs following:")),
                    br(),
                    print("Checking if your missing values are 
                              completely at random or not"),
                    br(),
                    print("If missings are CAR: 
                              Assigning column means to the missing values"),
                    br(),
                    print("If missings are NOT CAR: 
                              Running multiple imputation"),
                    br(),
                    print("Detecting and removing multivariate outliers"),
                    br(),
                    print("Running and reporting 
                              Mardia's multivariate test"),
                    br(),
                    print("Creates a button for 
                              downloading the edited data")
                )
              )
      )
    )
  )
)

##### SERVER #####
server <- function(input, output, session) {
  ##### Loading Data #####
  data <- reactive({
    dataFile <- input$file
    req(dataFile)
    dataFileExt <- tools::file_ext(dataFile$datapath)
    if(dataFileExt == "csv"){
      data.tmp <- read.csv2(dataFile$datapath, header = TRUE)
    } else if(dataFileExt == "sav"){
      data.tmp <- read.spss(dataFile$datapath,
                            to.data.frame = T,
                            use.value.labels = F)
    }
  })
  
  ##### Selecting the First Variable #####
  output$firstItem <- renderUI({
    req(input$file)
    data <- data()
    varNames <- colnames(data)
    selectInput("firstItem",
                "Please select your first item",
                choices = varNames)
  })
  
  ##### Run Analysis Button #####
  output$run <- renderUI({
    req(input$file)
    actionButton("run", "Run Analysis", icon = icon("play"))
  })
  
  observeEvent(input$run, {
    ##### Little's MCAR Test #####
    output$little <- renderPrint({
      fullData <- data()
      first <- which(input$firstItem == colnames(data()))
      data <- fullData[,first:ncol(fullData)]
      missing.test <- LittleMCAR(data)
      if(sum(missing.test$amount.missing) == 0){
        print("Your data does not contain any missing value!")
        data2 <<- data
      } else {
        if(isTRUE(input$fixmissing)){
          if(missing.test$p.value < 0.05){
            print("")
            print("Missings are NOT completely at random.")
            br()
            print("Multiple imputation method was used.")
            mimp <- mice(data, print = FALSE)
            imputed.data <- complete(mimp)
          } else{
            br()
            print("Missings are completely at random. Variable means will be used.")
            imputed.data <- round(na_mean(data))
          }
          data2 <<- imputed.data
          #colnames(data2) <<- paste0("i",1:ncol(data2))
          print(head(data2))
        }}
    })
    
    ##### Mahalanobis Distance & Remove Outliers #####
    output$md <- renderPrint({
      req(input$file)
      first <- which(input$firstItem == colnames(data()))
      if(isTRUE(input$fixmissing)){
        data <- data2
      } else {
        data <- data()[,first:ncol(data())]
      }
      md <- mahalanobis(data, center = colMeans(data), cov = cov(data))
      alpha <- as.numeric(input$cutoff)
      cut <- qchisq(p = 1 - alpha, df = ncol(data))
      outliers <- which(md > cut)
      if(first == 1){
        rest.of.data.without.outliers <- NULL
      } else {
        rest.of.data.without.outliers <- data()[,1:(first-1)]
      }
      if(length(outliers) == 0){
        data.without.outliers <- data
        rest.of.data.without.outliers <- rest.of.data.without.outliers
      } else {
        data.without.outliers <- data[-outliers,]
        rest.of.data.without.outliers <- rest.of.data.without.outliers[-outliers,]
      }
      data3 <<- data.without.outliers
      data4 <<- rest.of.data.without.outliers
      if(first == 1){
        data5 <<- data3
      } else {
        data5 <<- cbind(data4, data3)
      }
      print(paste("Number of multivariate outliers:", length(outliers)))
      if(length(outliers) == 0){
        print("You do NOT have any multivariate outliers")
      } else {
        print(" These outliers removed from your dataset.")
      }
      br()
      br()
      print("Multivariate normality test results for outliers-free data")
      mvn(data3)$multivariateNormality
    })
    
    ##### Download Button #####
    output$dlButton <- renderUI({
      req(input$file)
      downloadButton("download", "Your Data is Ready to FA")
    })
    
    output$download <- downloadHandler(
      filename = function() {
        "data2fa.csv"
      },
      content = function(file) {
        write.csv2(data5, file, row.names = FALSE)
      }
    )
    
    output$citation <- renderPrint({
      cite_packages()  
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
