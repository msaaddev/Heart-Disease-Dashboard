## app.R ##
library(shiny)
library(shinydashboard)
library(shinyTree)
library(rpart)
library(party)

data <- read.csv ("processed.cleveland.data",header=FALSE)

names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")

data$num[data$num > 1] <- 1

data <- transform(
    data,
    age=as.integer(age),
    sex=as.factor(sex),
    cp=as.factor(cp),
    trestbps=as.integer(trestbps),
    choi=as.integer(choi),
    fbs=as.factor(fbs),
    restecg=as.factor(restecg),
    thalach=as.integer(thalach),
    exang=as.factor(exang),
    oldpeak=as.numeric(oldpeak),
    slope=as.factor(slope),
    ca=as.factor(ca),
    thai=as.factor(thai),
    num=as.factor(num)
)

data[ data == "?"] <- NA

data$thai[which(is.na(data$thai))] <- as.factor("3.0")
data <- data[!(data$ca %in% c(NA)),]

data$ca <- factor(data$ca)
data$thai <- factor(data$thai)

sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

graph_rft <- randomForest(num~., data=train)

Predict_Forest <- predict(graph_rft, newdata = test[-14])

confusionMatrix <- table(test[,14], Predict_Forest)

accuracyRandForest <- sum(diag(confusionMatrix)) / sum(confusionMatrix)

graph_ID3 <- rpart(num~., data = data, method = 'class')

predictid3 <-predict(graph_ID3, data, type = 'class')

table_ID3 <- table(data$num, predictid3)

accuracy_ID3 <- sum(diag(table_ID3)) / sum(table_ID3)

graph_C45 <- J48(num~., data=data)

predictc45 <- predict(graph_C45, data)

table_C45 <- table(data$num, predictc45)

accuracy_C45 <- sum(diag(table_C45)) / sum(table_C45)


ui<-dashboardPage(skin = "blue",
                  dashboardHeader(title = "Heart Disease Data Visualization"),
                  
                  dashboardSidebar( sidebarMenu(id="General",
                                                menuItem("Dropdown", tabName = "dropdown",
                                                         menuSubItem("RandomForest", tabName = "tree1"),
                                                         menuSubItem("DecisionTree ID3", tabName = "tree2"),
                                                         menuSubItem("DecisionTree C4.5", tabName = "tree3"))
                  )),
                  dashboardBody(
                      
                      tabItems(
                          tabItem(tabName = "tree1",
                          fluidRow(
                              box(
                                  title = "Random Forest Graph", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  plotOutput("randomforestgraph")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "Compare Table", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  tableOutput("randomforest")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "Random Forest Accuracy", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  textOutput("accu_rft")
                              )
                          )
                          
                          ),
                          tabItem(tabName = "tree2",
                          fluidRow(
                              box(
                                  title = "ID3 Graph", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  fluidRow(plotOutput("id3graph"))
                              )
                          ),
                            fluidRow(
                              box(
                                  title = "Compare Table", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  tableOutput("id3table")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "ID3 Accuracy", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  textOutput("accu_ID3")
                              )
                          )
                          
                          
                          ),
                          tabItem(tabName = "tree3",
                          fluidRow(
                              box(
                                  title = "C4.5 Graph", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  fluidRow(plotOutput("c4t"))
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "Compare Table", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width  = 250,
                                  tableOutput("c4table")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "C4.5 Accuracy", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  textOutput("accu_C45")
                              )
                          )
                          
                          )
                      )
                  )
)

server <- function(input, output) {
    output$randomforest <- renderTable(cm)
    output$id3table <- renderTable({
        table_ID3
    })
    output$c4table <- renderTable({
        table_C45
    })
    
    output$randomforestgraph <- renderPlot(plot(graph_rft))
    
    output$id3graph <- renderPlot(rpart.plot(graph_ID3, extra = 106))
    output$accu_rft <-renderText(paste("Accuracy : ", accuracyRandForest)) 
    output$accu_ID3 <-renderText(paste("Accuracy : ", accuracy_ID3)) 
    output$accu_C45 <-renderText(paste("Accuracy : ", accuracy_C45)) 
    output$c4t <- renderPlot(plot(graph_C45))
}

shinyApp(ui, server)