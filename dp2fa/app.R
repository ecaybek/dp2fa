##### Loading Packages #####

library(shiny)
library(shinydashboard)
library(mice)
library(BaylorEdPsych)
library(mvnmle)
library(MVN)
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
                        fileInput("file", "Upload your items-only data 
                                  [sav or csv]"),
                        selectInput("cutoff", 
                                    "Alpha for Multivariate Outliers",
                                    choices = c(0.001, 0.01, 0.05), 
                                    selected = 1),
                        checkboxInput("fixmissing", 
                                      "Impute missing values?", 
                                      value = TRUE),
                        uiOutput("dlButton")
                        ),
                    box(title = "Results", 
                        status = "warning", 
                        solidHeader = TRUE,
                        verbatimTextOutput("little"),
                        verbatimTextOutput("md"),
                        verbatimTextOutput("mvn")
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
                        verbatimTextOutput( "citation")
                        
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
                ))
        )
        )
    )

##### SERVER #####
server <- function(input, output) {
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

    ##### Little's MCAR Test #####
    output$little <- renderPrint({
        data <- data()
        missing.test <- LittleMCAR(data)
        if(isTRUE(input$fixmissing)){
            if(missing.test$p.value < 0.05){
                print("Missings are NOT completely at random. Multiple imputation method will be used.")
                mimp <- mice(data)
                imputed.data <- complete(mimp)
            } else{
                print("Missings are completely at random. Variable means will be used.")
                imputed.data <- round(na_mean(data))
            }
            data2 <<- imputed.data
            colnames(data2) <<- paste0("i",1:ncol(data2))
            print(head(data2))
        }
        
    })
    
    ##### Mahalanobis Distance & Remove Outliers #####
    output$md <- renderPrint({
        req(input$file)
        if(isTRUE(input$fixmissing)){
            data <- data2
        } else {
            data <- data()
        }
        md <- mahalanobis(data, center = colMeans(data), cov = cov(data))
        alpha <- as.numeric(input$cutoff)
        cut <- qchisq(p = 1 - alpha, df = ncol(data))
        outliers <- which(md > cut)
        data.without.outliers <<- data[-outliers,]
        print(paste("Number of multivariate outliers:", length(outliers)))
        print(" These outliers removed from your dataset.")
    })
    
    ##### Multivariate Normality #####
    output$mvn <- renderPrint({
        req(input$file)
        req(data.without.outliers)
        print("Multivariate normality test results for outliers-free data")
        mvn(data.without.outliers)$multivariateNormality
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
            write.csv2(data.without.outliers, file, row.names = FALSE)
        }
    )
    
    output$citation <- renderPrint({
      cite_packages()  
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
