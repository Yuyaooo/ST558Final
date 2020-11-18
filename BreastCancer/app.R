library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(knitr)
library(caret)
library(tree)

breast <- read_csv(file = "./breastcancer.csv")

ui <- dashboardPage(skin="red",
                    #add title
                    dashboardHeader(title="Breast Cancer Recurrence",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                        menuItem("About", tabName = "about", icon = icon("archive")),
                        menuItem("Data Exploration", tabName = "data", icon = icon("database")),
                        menuItem("Clustering", tabName = "cluster", icon = icon("sitemap")),
                        menuItem("Modeling", tabName = "model", icon = icon("chart-line")),
                        menuItem("Save Data", tabName = "save", icon = icon("file-csv"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                        tabItems(
                            # First tab content
                            tabItem(tabName = "about",
                                    fluidRow(
                                        #add in latex functionality if needed
                                        withMathJax(),
                                        
                                        #two columns for each of the two items
                                        column(8,
                                               #Description of App
                                               h1("Data Description"),
                                               #box to contain description
                                               box(background="yellow",width=12,
                                                   h4("The data in this app is ",a("Breast Cancer Dataset", href="https://archive.ics.uci.edu/ml/datasets/breast+cancer"), "showing the recurrence of breast cancer and several features about it, which was provided by the University Medical Centre, Institute of Oncology, Ljubljana, Yugoslavia", span("(Zwitter & Soklic 1988).",style = "font-style:italic")),
                                                   h4("This data set contains 285 observations, one categorical response, and 9 predictors. The response includes two levels: recurrence and no-recurrence."),
                                                   h4("The predictors include: "),
                                                   h5("1. patient’s average age in years (age.avg)"),
                                                   h5("2. the period in a woman’s life when menstruation ceases (menopause)"),
                                                   h5("3. patient’s average tumor-size on her breast (tumor.size.avg)"),
                                                   h5("4. average node size in the main portion of the breast (inv.nodese.avg)"),
                                                   h5("5. whether node is present or not in cap of the breast (node.caps)"),
                                                   h5("6. stage of breast cancer (deg.malig)"),
                                                   h5("7. position of the breast cancer for example left breast"),
                                                   h5("8. right breast (breast)"),
                                                   h5("9. portion of the breast for example left-up, left-low (breast.quad)"),
                                                   h5("10. whether irradiate present or not (irradiat)")
                                               )
                                        ),
                                        column(4,
                                               #Description of App
                                               h1("What does this app do?"),
                                               #box to contain description
                                               box(background="blue",width=12,
                                                   h4("This application is used for Breast Cancer Dataset. We can do the data exploration, clustering, modeling for this data, and look through the whole data, subset it, and save it.")),
                                               #How to use the app
                                               h1("How to use the app?"),
                                               #box to contain description
                                               box(background="red",width=12,
                                                   h4("The controls for the app are located to the left and the visualizations are available on the right."),
                                                   h4("To change any conditions you want, just click the check box or drop-down list to choose one."),
                                                   h4("On the data exploration part, you can click on the plot or select a region you want."),
                                                   h4("To download any plot or table, just click the button under the plot or table.")
                                               )
                                        )
                                    )
                            ),
                            #actual app layout      
                            tabItem(tabName = "data",
                                    fluidRow(
                                        column(width=6,
                                               box(width=12,background="red",
                                                   selectInput("var", "See the frequency table for variable recurrence/no-recurrence and the variable you choose", 
                                                   choices = c("menopause","node.caps", "breast", "irradiat"), selected = "menopause")),
                                               box(width=12,
                                                   h3(uiOutput("title")),
                                                   #tableOutput("table")
                                                   verbatimTextOutput("table")
                                                   ),
                                               box(width = 12,
                                                   h3("Summary for quantitative variables"),
                                                   tableOutput("summary")
                                                   )
                                               ),
                                        column(width=6,
                                               box(width=12,
                                                   title="What is the distribution of patient’s average age in years?",
                                                   background="red",
                                                   solidHeader=TRUE,
                                                   checkboxInput("check", "Do you want to see recurrence or no-recurrence separately? Please check it if Yes"),
                                                   conditionalPanel(condition = "input.check",
                                                                    radioButtons("RB", label = "Select the Class Type", choices = list("recurrence-events", "no-recurrence-events")))
                                                   ),
                                               box(width=12,
                                                   plotlyOutput("plot"),
                                                   br(),
                                                   downloadButton("downloadplot", "Download this plot")
                                                   )
                                               )
                                        )
                                    ),
                            
                            
                            tabItem(tabName = "cluster",
                                    fluidPage(
                                        titlePanel("Clustering for Recurrence Cases"),
                                        sidebarLayout(
                                            sidebarPanel(
                                                radioButtons("clusterRB", label = "Variables to cluster", choices = list("Age & Tumor Size", "Age & Inv. Nodes", "Tumor Size & Inv. Nodes")),
                                                checkboxInput("scale", "Smaller and specific sample size to cluster"),
                                                sliderInput("size", "The sample size you want to cluster",
                                                            min = 10, max = 85, value = 30, step = 5)
                                                ),
                                            mainPanel(
                                                plotOutput("clusterPlot")
                                                )
                                        )
                                    )
                            ),
                            
                            tabItem(tabName = "model",
                                    fluidRow(
                                        withMathJax(),
                                        column(width=6,
                                               h2("Supervised learning model"),
                                               box(width=12,background="red",
                                                   h3("Logistic Regression"),
                                                   varSelectInput("regvar", "The variables you want to put in the model:", data=select(read_csv(file = "./breastcancer.csv"),-class), multiple = TRUE)
                                               ),
                                               box(width=12,
                                                   #tableOutput("table")
                                                   verbatimTextOutput("logsum")
                                               ),
                                               box(width = 12,
                                                   background="red",
                                                   h3("Classification Tree"),
                                                   numericInput("num", "Select the number of trees", value = 6, min = 3, max = 8, step=1)
                                               ),
                                               box(width = 12,
                                                   plotOutput("treeplot")
                                               )
                                        ),
                                        column(width=6,
                                               h2("Prediction"),
                                               box(width = 12,
                                                   background="red",
                                                   h3("Choose the values for each variables you want to predict recurrence or not (we will include all predictors in the model): "),
                                                   column(width= 4,
                                                          radioButtons("men", label = "menopause", choices = list("premeno", "ge40", "lt40"))),
                                                   column(width = 4, 
                                                          radioButtons("node", label = "node present or not", choices = list("yes", "no"))),
                                                   column(width = 4,
                                                          radioButtons("deg", label = "Stage of breast cancer", choices = list("1", "2", "3")))
                                                   ),
                                               box(width = 12,
                                                   background="red",
                                                   column(width = 4,
                                                          radioButtons("irra", label = "Irradiate present or not", choices = list("yes", "no"))),
                                                   column(width = 4,
                                                          radioButtons("breast", label = "Stage of breast cancer", choices = list("right", "left"))),
                                                   column(width = 4,
                                                          radioButtons("breast.quad", label = "Stage of breast cancer", choices = list("right_low", "right_up", "left_low","left_up")))
                                                   ),
                                               box(width = 12,
                                                   sliderInput("agepred", "The value of age:", min = 30, max = 80, value = 45, step = 0.5),
                                                   sliderInput("tumorpred", "The value of tumor size:", min = 10, max = 45, value = 25, step = 1),
                                                   sliderInput("nodepred", "The value of node size:", min = 1, max = 8, value = 3, step = 1)
                                                   ),
                                               box(width = 12, 
                                                   h4("The predicted response for the values given above using ", strong("Logistic Regression"), " model is: ", strong(textOutput("glmpred"))),
                                                   br(),
                                                   h4("The predicted response for the values given above using ", strong("Classification Tree"), " model is: ", strong(textOutput("treepred"))),
                                                   br()
                                                   ),
                                               withMathJax(),
                                               h2("Confusion Matrix"),
                                               box(width=12,
                                                   background = "light-blue",
                                                   h4("The misclassification rate \\(\\theta\\) is: "),
                                                   h4("\\(\\theta = \\frac{(predyes.trueno+predno.trueyes)}{(Totalpredictions)}\\)")
                                                   ),
                                               box(width=12,
                                                   title="Logistic Regression",
                                                   h5("It will update if you change", strong("the variables in the model.")),
                                                   solidHeader=TRUE,
                                                   tableOutput("glmmatrix"),
                                                   strong(textOutput("glmtext"))
                                               ),
                                               box(width=12,
                                                   title="Classification Tree",
                                                   h5("It will update if you change", strong("the number of trees.")),
                                                   tableOutput("treematrix"),
                                                   strong(textOutput("treetext"))
                                               )
                                        )
                                    )
                            ),       
                            
                            tabItem(tabName = "save",
                                    fluidPage(
                                        titlePanel("Process the data and save it!"),
                                        sidebarLayout(
                                            sidebarPanel(
                                                checkboxInput("subset", "Subset the data"),
                                                conditionalPanel(condition = "input.subset",
                                                                 selectInput("subvar", "Which categorical variable you want to subset in the data?", choices = c("menopause","node.caps", "deg.malig", "irradiat"), selected = "menopause"),
                                                                 conditionalPanel(
                                                                     condition = "input.subvar == 'menopause'",
                                                                     radioButtons("menRB", label = "Select the menopause type", choices = list("premeno", "ge40", "lt40"))
                                                                     ),
                                                                 conditionalPanel(
                                                                     condition = "input.subvar == 'node.caps'",
                                                                     radioButtons("nodeRB", label = "Tne node is present or not", choices = list("yes", "no"))
                                                                    ),
                                                                 conditionalPanel(
                                                                     condition = "input.subvar == 'deg.malig'",
                                                                     radioButtons("degRB", label = "Stage of breast cancer", choices = list("1", "2", "3"))
                                                                    ),
                                                                 conditionalPanel(
                                                                     condition = "input.subvar == 'irradiat'",
                                                                     radioButtons("irRB", label = "Irradiate present or not", choices = list("yes", "no"))
                                                                     )
                                                                 ),
                                                downloadButton("downloadtable", "Download this table")
                                                ),
                                            mainPanel(
                                                dataTableOutput("subdata")
                                            )
                                        )
                                    )
                            )
                        )
                    )
)

# Define server logic required to draw the plots
server <- shinyServer(function(input, output, session) {
    
    breast <- read_csv(file = "./breastcancer.csv")
    
    output$title <- renderUI({
        paste0("Frequency table of recurrence/no-recurrence and ", input$var)
    })
    
    myPlot <- function(){
        if(input$check){
            getData <- reactive({
                newData <- breast %>% filter(class == input$RB)
            })
            newdata <- getData()
            if(input$RB == "recurrence-events"){
                ggplot(newdata, aes(x=age.avg))+geom_histogram(bins=20)
            } else {
                ggplot(newdata, aes(x=age.avg))+geom_histogram(bins=20)
            }
        }
        else{
            ggplot(breast, aes(x=age.avg))+geom_histogram(bins=20)
        }
    }
    
    output$plot<- renderPlotly({
        ggplotly(myPlot())
        })
    
    output$downloadplot <- downloadHandler(
        filename = function(){
            if(input$check){
                paste("Histogram of average age-", input$RB, ".pdf", sep = '')
            }
            else{
                paste("Histogram of average age.pdf")
            }
        },
        content = function(file){
            pdf(file)
            print(myPlot())
            dev.off()
        },
        contentType = 'pdf'
    )
    
    myTable <- function(){
        if(input$var == "menopause"){
            table(breast$class, breast$menopause)
        }
        else if(input$var == "node.caps"){
            table(breast$class, breast$node.caps)
        }
        else if(input$var == "breast"){
            table(breast$class, breast$breast)
        }
        else if(input$var == "irradiat"){
            table(breast$class, breast$irradiat)
        }
    }
    
    
    output$table <- renderPrint({
        myTable()
    })
    
    output$summary <- renderTable({
        a <- apply(select(breast, age.avg, tumor.size.avg, inv.nodese.avg), MARGIN = 2, summary)
        a <- round(a, digits = 2)
        Name <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
        c <- cbind(Name,a)
        c
    })
    
    output$clusterPlot <- renderPlot({
        if(input$scale){
            observe({updateSliderInput(session, "size", max = 40, step = 1)})
            set.seed(1)
            br <- breast %>% filter(class == "recurrence-events") 
            new <- sample(1:nrow(br), size = input$size)
            newData <- br[new, ]
        }else{
            observe({updateSliderInput(session, "size", min = 10, max = 85, step = 5)})
            set.seed(1)
            br <- breast %>% filter(class == "recurrence-events") 
            new <- sample(1:nrow(br), size = input$size)
            newData <- br[new, ]
        }
        if (input$clusterRB == "Age & Tumor Size"){
            hierClust <- hclust(dist(data.frame(newData$age.avg, newData$tumor.size.avg)))
            plot(hierClust, xlab = "")}
        else if (input$clusterRB == "Age & Inv. Nodes"){
            hierClust <- hclust(dist(data.frame(newData$age.avg, newData$inv.nodese.avg)))
            plot(hierClust, xlab = "")}
        else if (input$clusterRB == "Tumor Size & Inv. Nodes"){
            hierClust <- hclust(dist(data.frame(newData$tumor.size.avg, newData$inv.nodese.avg)))
            plot(hierClust, xlab = "")}
    })
    
    set.seed(50)
    breast$class <- as.factor(breast$class)
    breast$menopause <- as.factor(breast$menopause)
    breast$node.caps <- as.factor(breast$node.caps)
    breast$deg.malig <- as.factor(breast$deg.malig)
    breast$breast <- as.factor(breast$breast)
    breast$breast.quad <- as.factor(breast$breast.quad)
    breast$irradiat <- as.factor(breast$irradiat)
    trainIndex <- createDataPartition(breast$class, p = 0.7, list = FALSE)
    breastTrain <- breast[trainIndex, ]
    breastTest <- breast[-trainIndex, ]
    
    glmdata <- reactive({
        if (length(input$regvar) == 0){
            new <- breastTrain
            }else{
            new <- breastTrain %>% select(class, !!!input$regvar)
            }
        #new <- select(breastTrain, input$regvar)
    })
    
    glmfit <- function(){
        new <- glmdata()
        glm(class~., data = new, family = "binomial")
    }
    
    output$logsum <- renderPrint({
        summary(glmfit())
    })
    
    
    output$treeplot <- renderPlot({
        fullFit <- tree(class ~ ., data=breastTrain)
        pruned.tree <- prune.tree(fullFit, best=input$num)
        plot(pruned.tree)
        text(pruned.tree)
    })
    
    predData <- reactive({
        data.frame(menopause=as.factor(input$men), node.caps=as.factor(input$node), deg.malig = as.factor(input$deg), breast = as.factor(input$breast), breast.quad = as.factor(input$breast.quad), irradiat = as.factor(input$irra), age.avg = input$agepred, tumor.size.avg = input$tumorpred, inv.nodese.avg = input$nodepred)
    })
    
    predglm <- function(){
        pred <-predData()
        model <- glm(class~., data = breast, family = "binomial")
        predvalue <- predict(model, newdata = pred,type = "response")
        if(predvalue>.5){
            predvalue <- "recurrence-events"
            predvalue
        }
        else{
            predvalue <- "no-recurrence-events"
            predvalue
        }
    }
    
    output$glmpred <- renderText({
        paste(predglm())
    })
    
    predtree <- function(){
        pred <-predData()
        fullFit <- tree(class ~ ., data=breast)
        predict(fullFit, newdata = pred, type = "class")
    }
    
    output$treepred <- renderText({
        paste(predtree())
    })
    
    confusion <- function(){
        new <- glmdata()
        fit <- train(class ~ ., data = new, 
                     method = "glm", 
                     family = "binomial",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 10))
        confusionMatrix(data = breastTest$class, reference = predict(fit, newdata = breastTest))
    }
    
    output$glmmatrix <- renderTable({
        a <- confusion()
        a$table
    })
    
    output$glmtext <- renderText({
        a <- confusion()
        paste("Misclassification rate is ", round(sum(diag(a$table)/sum(a$table)), digits = 3))
    })
    
    
    output$treematrix <- renderTable({
        fullFit <- tree(class ~ ., data=breastTrain)
        Pred <- predict(prune.tree(fullFit, best=input$num), newdata = breastTest,type = "class")
        fullTbl <- table(data.frame(Pred, breastTest[, "class"]))
        fullTbl
        #sum(diag(fullTbl)/sum(fullTbl))
    })
    
    output$treetext <- renderText({
        fullFit <- tree(class ~ ., data=breastTrain)
        fullPred <- predict(prune.tree(fullFit, best=input$num), newdata = breastTest,type = "class")
        fullTbl <- table(data.frame(fullPred, breastTest[, "class"]))
        paste("Misclassification rate is ", round(sum(diag(fullTbl)/sum(fullTbl)), digits = 3))
    })
    
    getsubData <- reactive({
        if(input$subset){
            if(input$subvar == "menopause"){
                newData <- breast %>% filter(menopause == input$menRB)
                }
            else if(input$subvar == "node.caps"){
                newData <- breast %>% filter(node.caps == input$nodeRB)
                }
            else if(input$subvar == "deg.malig"){
                newData <- breast %>% filter(deg.malig == input$degRB)
                }
            else{
                newData <- breast %>% filter(irradiat == input$irRB)
                }
            }
        else{
            newData <- breast
            }
        })
    
    
    output$subdata <- renderDataTable({
        subData <- getsubData()
        datatable(subData, options = list(paging = FALSE))
    })
    
    
    output$downloadtable <- downloadHandler(
        filename = function(){
                paste("Breast Cancer Data.csv")
            },
        content = function(file){
            subData <- getsubData()
            write.csv(subData, file)
            }
    )
})

shinyApp(ui = ui, server = server)

