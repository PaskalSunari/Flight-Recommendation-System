

#Recommendation System
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(assertthat)
library(shinythemes)
library(askpass)
library(randomForest)
library(Metrics)
#library(bslib)
#library(shinyWidgets)

#Data preparation and testing visualization
data <- read.csv("sales.csv")
test_set <- read.csv("data_test.csv")
train_set <- read.csv("data_train.csv")

levels(test_set$Pax) <- levels(test_set$Pax)
levels(test_set$Sector)<- levels(test_set$Sector)
levels(test_set$Cls) <- levels(test_set$Cls)
# str(data)
# #data <- data[-c(0,1,3),]
# View(data)
# dim(data)
# summary(data)
# summary(data$Fare)
# summary(data$Cls)
# summary(data$Com)
# levels(data$Cls)
# library(ggplot2)
 

# Building the UI, Server and APP
ui <- fluidPage(
  theme = shinytheme("darkly"),
  navbarPage(title = "Recommendation System",
                 tabPanel(title = "Data Dictionary",
                          verbatimTextOutput("d"),
                          verbatimTextOutput("view")
                          ),
            
                 navbarMenu(title = "Statistical Summary",
                            tabPanel(title = "uniform data",
                                     plotOutput("unif"),
                                     # verbatimTextOutput("stats"),
                                    actionButton("reunif", "Resample"),
                                     actionButton("dunif","Download")
                                     ),
                            tabPanel(title = "chi squared data",
                                     plotOutput("chisq"),
                                     verbatimTextOutput("stats1"),
                                     actionButton("rechisq","Resample"),
                                     actionButton("dchiq", "Download")
                                     )
                            ),
             navbarMenu(title = "Graphs",
                        tabPanel(title = " Boxplot",
                                 plotOutput("box"),
                                 verbatimTextOutput("stats"),
                                 actionButton("rebox","Resample"),
                                 actionButton("dbox","Download Plot")),

                        tabPanel(title = "Histogram",
                                 plotOutput("histo"),
                                 verbatimTextOutput("cl"),
                                 verbatimTextOutput("summary"),
                                 actionButton("rehisto", "Resample"),
                                 actionButton("dhisto","Download"))
                        ),
             navbarMenu(title = "Machine Learning",
                        tabPanel(title = "prediction",
                                 sidebarPanel(
                                   selectInput("ycol", "select the input",
                                               names(data)[c(8,10,11)],
                                               selected = names(data)[[8]]),
                                               h1(""),
                                               h5("head of dataset"),
                                               tableOutput("table")
                                 ),
                                 mainPanel(
                                   h5("The chart shows amount of Fare, Tax and commission vs the percent Net balance ."),
                                   h5("Source:  dataset from library"),
                                   h5("A linear regression model is calculated for each type of amount and shown below."),
                                   plotOutput("plot1"),
                                   h4("Slope"),
                                   textOutput("pred1slope"),
                                   textOutput("pred2slope"),
                                   textOutput("pred3slope"),
                                   h4("Intercept"),
                                   textOutput("pred1intercept"),
                                   textOutput("pred2intercept"),
                                   textOutput("pred3intercept"),
                                   h5("The linear model of price explained by percent of Net cost
                                      can be evaluated with the P value"),
                                   h4("P Value of Regression Model"),
                                   textOutput("pred1p"),
                                   textOutput("pred2p"),
                                   textOutput("pred3p")
                                   
                                 ),
                                 verbatimTextOutput("regress"),
                                 plotOutput("predict")),

                        tabPanel(title = "Recommendation",
                                 selectInput('p_pax','Pax',c('ADULT', 'CHILD')),
                                 div(),
                                 selectInput('p_cls', 'Class of Ticket', c('N','O','P','Q','R','S')),
                                 div(),
                                 selectInput('p_sector','Sector',c("BDP-KTM","BIR-KTM","BWA-KTM","KTM-BDP","KTM-BIR" ,"KTM-BWA")),
                                 div(),
                                 h5("System has predicted the following detail for the recommendation with the travel topic of PAX, Sector and Class that you may like"),
                        )

                                 
                        )
             )
  )



server <- function(input, output) {
  rv <- reactiveValues(
    norm = rnorm(50),
    unif = runif(50),
    chisq = rchisq(50,2)
    )
  observeEvent(input$renorm, {rv$norm <- rnorm(data$Fare)})
  observeEvent(input$reunif, {rv$unif <- runif(data$Fare)})
  observeEvent(input$rechisq, {rv$chisq <- rchisq(data$Fare,2)})
  observeEvent(input$rebox, {rv$box <- rnorm(data$Net)})
  
  output$d <- renderPrint({
    summary(data)
  })
  output$view <- renderPrint({
    head(data)
  })
 
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from as standard uniform distribution")
  })
  
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random from a chi square distribution with two degree of freedom")
  })
  
  output$box <- renderPlot({
    boxplot(data$Net)
  })
  
  output$stats <- renderPrint({
    summary(data$Net)
  })
  output$histo <- renderPlot({
    ggplot(data, aes( x = Sector, fill = Cls )) +
      theme_bw() +
      geom_bar() +
      labs(Y = "Number of Passengers",
           title = "Passenger's Count by the class of Ticket")
  })
  output$cl <- renderPrint({
    summary(data$Cls) 
  })
  output$summary <- renderPrint({
    summary(data$Sector)
  })
  
  # Machine learning Part
  selectedData <-reactive({
    data[, c("Net",input$ycol)]
  })
  
  model1 <- lm(Fare ~ Net , data = data)
  model2 <- lm(Tax ~ Net, data = data)
  model3 <- lm(Com ~ Net, data = data)
  
  output$predict <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData())
    
    if(input$ycol == "Fare"){
      abline(model1, col = "blue", lwd = 2)
    }
    if(input$ycol == "Tax"){
      abline(model2, col = "green", lwd = 2)
    }
    if(input$ycol == "Com"){
      abline(model3, col = "red", lwd = 2)
    }
  })
  
  
  
  output$pred1p <- renderText({
    if(input$ycol == "Fare"){
      anova(model1)$"Pr(>F)"[1]
    }
  })
  output$pred2p <- renderText({
    if(input$ycol== "Tax"){
      anova(model2)$'Pr(>)'[1]
    }
  })
  output$pred3p <- renderText({
    if(input$ycol == "Com"){
      anova(model3)$'Pr(>F)'[1]
    }
  })
  
  
  output$pred1slope <- renderText({
    if(input$ycol == "Fare"){
      model1[[1]][2]
    }
  })
  output$pred2slope <- renderText({
    if(input$ycol == "Tax"){
      model2[[1]][2]
    }
  })
  output$pred3slope <- renderText({
    if(input$ycol == "Com"){
      model3[[1]][2]
    }
  })
  
  
  output$pred1intercept <- renderText({
    if(input$ycol == "Fare"){
      model1[[1]][1]
    }
  })
  output$pred2intercept <- renderText({
    if(input$ycol == "Tax"){
      model2[[1]][1]
    }
  })
  output$pred3intercept <- renderText({
    if(input$ycol == "Com"){
      model3[[1]][1]
    }
  })
  
  
  output$table <- renderTable({
    head(selectedData(), 10)
  })
}
 

shinyApp(ui = ui, server = server)
