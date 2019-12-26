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

sickid3 <- rpart(num~., data = data, method = 'class')

predictid3 <-predict(sickid3, data, type = 'class')

matrix_ID3 <- table(data$num, predictid3)

accuracy_ID3 <- sum(diag(matrix_ID3)) / sum(matrix_ID3)

sickc45 <- J48(num~., data=data)

predictc45 <- predict(sickc45, data)

matrix_C45 <- table(data$num, predictc45)

accuracy_C45 <- sum(diag(matrix_C45)) / sum(matrix_C45)

Random_Forest <- randomForest(num~., data=data)

Predict_Forest <- predict(Random_Forest, data)

confusionMatrix <- table(data$num, Predict_Forest)

accuracyRandForest <- sum(diag(confusionMatrix)) / sum(confusionMatrix)

ui<-dashboardPage(skin = "green",
                  dashboardHeader(title = "Heart Disease Data Visualization"),
                  dashboardSidebar( sidebarMenu(id="General",
                                                menuItem("Dropdown", tabName = "dropdown",
                                                         menuSubItem("RandomForest", tabName = "tree1"),
                                                         menuSubItem("DecisionTree ID3", tabName = "tree2"),
                                                         menuSubItem("DecisionTree C4.5", tabName = "tree3"))
                  )),
                  dashboardBody(

                      tabItems(
                          tabItem(tabName = "tree1",fluidRow(
                              box(
                                  title = "Compare Table", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  tableOutput("randomforest")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "Random Forest Graph", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  plotOutput("randomforestgraph")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "Random Forest Tree", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  plotOutput("rft")
                              )
                          )

                          ),
                          tabItem(tabName = "tree2",fluidRow(
                              box(
                                  title = "Compare Table", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  tableOutput("id3")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "ID3 Graph", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  fluidRow(plotOutput("id3graph"))
                              )
                          ),



                          ),
                          tabItem(tabName = "tree3",fluidRow(
                              box(
                                  title = "Compare Table", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width  = 250,
                                  tableOutput("c4table")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "C4.5 Tree", status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,width = 250,
                                  fluidRow(plotOutput("c4t"))
                              )
                          )

                          )
                      )
                  )
)

server <- function(input, output) {
    output$randomforest <- renderTable(cm)
    output$id3 <- renderTable(id3cm)
    output$c4table <- renderTable(c4cm)

    output$randomforestgraph <- renderPlot(plot(rf))
    output$rft <- renderPlot(plot(rFTree))

    output$id3graph <- renderPlot(rpart.plot(id3))

    output$c4t <- renderPlot(plot(c4Tree))
}

shinyApp(ui, server)