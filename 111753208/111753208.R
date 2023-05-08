#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggbiplot)
library(shinythemes)
library(FactoMineR)
library(factoextra)
data(iris)

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    title = "Henry's HW4 shiny web",
    tabPanel("PCA",     
      h3("What is PCA?"),
      span("PCA stands for Principal Component Analysis. It is a statistical technique\
           used for dimensionality reduction and data analysis. PCA aims to transform a\
           dataset with a potentially large number of variables into a smaller set of \
           uncorrelated variables called principal components. These principal components\
           capture the maximum amount of information from the original dataset while\
           minimizing the loss of information."),
      hr(),
      h3("iris dataset:"),
      p(),
      verticalLayout(
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            verticalLayout(
              selectInput("select1", "Dim X", choices = c("PC1", "PC2", "PC3")),
              selectInput("select2", "Dim Y", choices = c("PC1", "PC2", "PC3")),
              radioButtons("displayRadio", "Display class:",
                           c("All",
                             "setosa",
                             "versicolor",
                             "virginica")),
            )
          ),
          # Show a plot of the generated distribution
          mainPanel(plotOutput("pcaPlot"))
        ),
        tabsetPanel(
          tabPanel("PCA result", p(), dataTableOutput("pcaResult")), 
          tabPanel("Input data", p(), dataTableOutput("pcaInputData"))
        )
      )
    ), 
    
    tabPanel("CA",
       h3("What is CA?"),
       span("Correspondence Analysis (CA) is a statistical technique used to explore\
            and analyze relationships between categorical variables in a contingency\
            table. It is particularly useful when dealing with large tables with \
            multiple rows and columns.The main objective of CA is to uncover patterns\
            and associations among the categories of variables in a contingency table\
            and to visually represent these relationships. It allows for the exploration\
            of dependencies and associations between variables, highlighting similarities\
            and dissimilarities in the data."),
       hr(),
       h3("iris dataset:"),
       p(),
       verticalLayout(
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
           sidebarPanel(
             verticalLayout(
               radioButtons("displayRadio2", "Display class:",
                            c("All",
                              "setosa",
                              "versicolor",
                              "virginica")),
               hr(),
               selectInput("caselect1", "Plot select", choices = c("BiPlot", "BarPlot"))
             )
           ),
           # Show a plot of the generated distribution
           mainPanel(plotOutput("caPlot"))
         ),
         tabsetPanel(
           tabPanel("CA result",
              verticalLayout(
                p(),
                h4("eig."),
                p(),
                tableOutput("caEigTable"),
                splitLayout(
                  p(
                    h4("rowcoord."),
                    tableOutput("caRcTable")
                  ),
                  p(
                    h4("colcoord."),
                    tableOutput("caCcTable")
                  )
                )
              )
            ), 
           tabPanel("Input data", p(), dataTableOutput("caInputData"))
         )
       )
      ), 
    
    tabPanel("About",
       tabsetPanel(
       tabPanel("iris",
          verticalLayout(
            p(),
            h3("About Dataset"),
            hr(),
            img(src = "iris-machinelearning.png", height = "150px"),
            p(),
            h4("Context"),
            p(),
            span("The Iris flower data set is a multivariate data set introduced by\
            the British statistician and biologist Ronald Fisher in his 1936 paper The\
            use of multiple measurements in taxonomic problems. It is sometimes called\
            Anderson's Iris data set because Edgar Anderson collected the data to\
            quantify the morphologic variation of Iris flowers of three related\
            species. The data set consists of 50 samples from each of three species\
            of Iris (Iris Setosa, Iris virginica, and Iris versicolor). Four features\
            were measured from each sample: the length and the width of the sepals and\
            petals, in centimeters."),
            p(),
            span("This dataset became a typical test case for many\
            statistical classification techniques in machine learning such as support\
            vector machines."),
            p(),
            h4("Content"),
            p(),
            span("The dataset contains a set of 150 records under 5 attributes - \
                 Petal Length, Petal Width, Sepal Length, Sepal width and Class(Species)."),
            p(),
            h4("Acknowledgements"),
            p(),
            span("This dataset is free and is publicly available at the ", 
              a(href = "http://archive.ics.uci.edu/ml/datasets/Iris", 
                "UCI Machine Learning Repository.")),
            p(),
            span("sourcs - ", a(href = "https://www.kaggle.com/datasets/arshid/iris-flower-dataset",
                                "kaggle/iris-flower-dataset"))
          )
        ), 
       tabPanel("data", p(), dataTableOutput("aboutIrisData"))))
)


selectTran <- function(select_value) {
  if (select_value == "PC1") {
    return (1)
  } else if (select_value == "PC2") {
    return (2)
  } else {
    return (3)
  }
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
      select1_value <- input$select1
      select2_value <- input$select2
      # check diff
      if (select1_value == select2_value) {
        new_choices <- setdiff(c("PC1", "PC2", "PC3"), select1_value)
        updateSelectInput(session, "select2", choices = new_choices)
      }
    })
    
    output$aboutIrisData <- renderDataTable({
      iris
    })
    
    observeEvent(c(input$displayRadio2, input$caselect1), {
      disClass2 <- input$displayRadio2
      #CA
      if (disClass2 == "All") {
        # log transform 
        inputData2 <- iris[, 1:4]
        inputLabel2 <- iris[, 5]
      } else {
        # log transform 
        inputData2 <- iris[iris$Species == disClass2, 1:4]
        inputLabel2 <- iris[iris$Species == disClass2, 5]
      }
      
      ir.ca <- CA(inputData2)
      output$caInputData <- renderDataTable({
        inputData2
      })
      
      output$caEigTable <- renderTable({
        ir.ca$eig
      })
      
      output$caRcTable <- renderTable({
        ir.ca$row$coord
      })
      
      output$caCcTable <- renderTable({
        ir.ca$col$coord
      })
      
      output$caPlot <- renderPlot({
        if (input$caselect1 == "BiPlot") {
          fviz_ca_biplot(ir.ca, col.var = "black", repel = FALSE)
        } else {
          barplot(ir.ca$eig[, 2], 
                  names.arg = 1:nrow(ir.ca$eig), 
                  main = "Variances Explained by Dimensions (%)",
                  xlab = "Principal Dimensions",
                  ylab = "Percentage of variances",
                  col ="steelblue")
          # Add connected line segments to the plot
          lines(x = 1:nrow(ir.ca$eig), ir.ca$eig[, 2], 
                type = "b", pch = 19, col = "red")
        }
      })
    })
  
    observeEvent(input$displayRadio, {
      disClass <- input$displayRadio
      if (disClass == "All") {
        # log transform 
        inputData <- iris[, 1:4]
        log.ir <- log(iris[, 1:4])
        ir.species <- iris[, 5]
      } else {
        # log transform 
        inputData <- iris[iris$Species == disClass, 1:4]
        log.ir <- log(iris[iris$Species == disClass, 1:4])
        ir.species <- iris[iris$Species == disClass, 5]
      }
      # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
      ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
      
      output$pcaInputData <- renderDataTable({
        inputData
      })
      
      output$pcaResult <- renderDataTable({
        ir.pca$x
      })
      
      output$pcaPlot <- renderPlot({
        g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species,
                      choices = c(selectTran(input$select1), selectTran(input$select2)))
        #g <- g + scale_color_discrete(name = '')
        if (disClass == "All") {
          color <- scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF"))
        } else if (disClass == "setosa") {
          color <- scale_color_manual(values=c("#F8766D"))
        } else if (disClass == "versicolor") {
          color <- scale_color_manual(values=c("#00BA38"))
        } else {
          color <- scale_color_manual(values=c("#619CFF"))
        }
        g <- g + color
        g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
        print(g)
      })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
