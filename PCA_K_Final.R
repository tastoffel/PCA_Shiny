library(shiny)
library(ggplot2)
library(DT)
library(cluster)
library(factoextra)
library(dplyr)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Principal Component Analysis (PCA) and K-means Clustering"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV File", accept = ".csv"),
      uiOutput("var_select_ui"),
      actionButton("pca_button", "Perform PCA")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("PCA? What's that?",
                 uiOutput("pca_intro")
        ),
        tabPanel("Dataset Summary",
                 DTOutput("dataset_summary_table"),
                 checkboxInput("standardize", "Standardize Data (z-score)", value = FALSE),
                 checkboxInput("remove_na", "Remove NA rows", value = FALSE)
        ),
        
        tabPanel("Variance Explained", 
                 plotOutput("scree_plot"),
                 DTOutput("explained_var_table"),
                 textOutput("mean_eigenvalue_text")
        ),
        
        tabPanel("Principal Component Contributions", 
                 uiOutput("pc_instructions"),
                 selectInput("retain_pc", "Select number of PCs to Retain", choices = NULL),
                 verbatimTextOutput("pc_equations"),
                 DTOutput("contributions_table"),
                 DTOutput("correlation_table")
        ),
        
        tabPanel("Biplot", 
                 plotOutput("biplot", height = "700px", width = "1000px"),
                 textOutput("biplot_text")),
        
        tabPanel("K-means Clustering", 
                 numericInput("num_clusters", "Select number of clusters", value = 3, min = 1),
                 plotOutput("kmeans_plot"), 
                 textOutput("kmeans_text"),
                 DTOutput("cluster_means_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_input <- reactiveVal(NULL)
  pca_result <- reactiveVal(NULL)
  kmeans_result <- reactiveVal(NULL)
  
  output$pca_intro <- renderUI({
    HTML(
      paste(
        tags$h3(tags$b("Principal Component Analysis (PCA)")),
        tags$p("When presented with multivariate data, the problem of having too many variables arises, hence ways to deal with this problem were created, and essentially mathematical techniques were developed aiming the dimensionality reduction."),
        tags$p(tags$b("Principal component analysis (PCA)")," is the name attributed to a technique which uses mathematical principles to transform a number of possibly correlated variables into a smaller number of variables, uncorrelated with each other, which are called \u201cprincipal components\u201d (PC) (Wolfgang Karl Hardle, 2020)."),
        tags$p("In this type of analysis, the principal idea of reducing the dimension of X (a data matrix) is achieved through linear combinations of the data with the variables, generating new variables. These new variables are linear combinations of the original variables and are derived in decreasing order of importance so that the first PC is the one amongst all that explains the highest value of variability, the second PC is the one that explains the second largest variability, and so on. (Chatfield & Collins, 1980)."),
        tags$br(),
        tags$h3(tags$b("Instructions for Using the App")),
        tags$p("The first step to use this app, regarding the appliance of a PCA to your dataset, you should ensure that the file you're uploading is in the .csv format!"),
        tags$p("- On the ", tags$b("'Dataset Summary'"), "tab, you'll have the summary for your dataset, contemplating the means, standard deviation, and the count of missing values. If you observe that the standard deviation or the means aren't relatively close, you should mark the \"Standardize Data\". And if any of your observations has missing values, it's mandatory that you check the \"Remove NA rows\"."),
        tags$p("- On the ", tags$b("'Variance Explained'")," tab you'll find a barplot representing the variane explained by each principal component, as well as a table with the eigenvalues, variance explained and the comulative variance explained, for each PC"),
        tags$p("- On the ", tags$b("'Principal Component Contributions'")," you'll  have to select a number of PC's to retain and below that is presented the equations for the linear combinations of each PC, and lastly it's presented a table of correlation between the PC and each initial variable"),
        tags$p("- On the ", tags$b("'Biplot'")," tab you'll find a graphical 2-dimensional representation of the observations and the variables retained by the 2 PC's"),
        tags$p("- On the ", tags$b("'K-means clustering'")," tab you'll find the same plot structure of the biplot, where you can group the observations according to a specified number of clusters, making your analysis way easier. You'll also have below a table that contains the mean values for each variable in each cluster"),
        tags$br(),
        tags$h4("Have fun!")
        )
    )
  })
  
  # Helper function to process selected data
  process_selected_data <- function(data, selected_vars, remove_na = FALSE) {
    selected_data <- data[, selected_vars, drop = FALSE]
    
    if (remove_na) {
      selected_data <- drop_na(selected_data)
    }
    
    return(selected_data)
  }
  
  observeEvent(input$datafile, {
    req(input$datafile)
    data <- read.csv(input$datafile$datapath)
    data_input(data)
    
    # Reset PCA and k-means results when a new dataset is uploaded
    pca_result(NULL)
    kmeans_result(NULL)
    
    updateCheckboxGroupInput(session, "vars", 
                             choices = names(data), 
                             selected = names(data))
  })
  
  # Dataset Summary
  output$dataset_summary_table <- renderDT({
    req(data_input())
    data <- data_input()
    
    summary_stats <- data.frame(
      Mean = sapply(data, function(x) if (is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else NA),
      SD = sapply(data, function(x) if (is.numeric(x)) round(sd(x, na.rm = TRUE), 3) else NA),
      NA_Count = sapply(data, function(x) sum(is.na(x)))
    )
    
    datatable(summary_stats, options = list(pageLength = 10, searching = TRUE))
  })
  
  # Variable Selection UI
  output$var_select_ui <- renderUI({
    req(data_input())
    numeric_columns <- names(data_input())[sapply(data_input(), is.numeric)]
    checkboxGroupInput("vars", "Select Variables for PCA", choices = numeric_columns, selected = numeric_columns)
  })
  
  observeEvent(input$pca_button, {
    req(data_input(), input$vars)
    selected_data <- process_selected_data(data_input(), input$vars, input$remove_na)
    
    # Check if there are missing values and user didn't select "Remove NA rows"
    if (any(is.na(selected_data)) && !input$remove_na) {
      showModal(modalDialog(
        title = "Error",
        "The database has missing values, thus the PCA can't be performed. Please either remove the missing values or check 'Remove NA rows'.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)  # Prevent PCA from running
    }
    
    # Perform PCA
    pca <- prcomp(selected_data, center = TRUE, scale. = input$standardize)
    pca_result(pca)
  })
  
  # Variance Explained Tab
  output$scree_plot <- renderPlot({
    req(pca_result())
    pca <- pca_result()
    eigenvalues <- eigen(cor(process_selected_data(data_input(), input$vars, input$remove_na)))$values
    
    ggplot(data.frame(PC = 1:length(eigenvalues), VarianceExplained = round(eigenvalues / sum(eigenvalues), 3)), 
           aes(x = PC, y = VarianceExplained)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle("Principal Component vs Variance Explained") +
      xlab("Principal Component") +
      ylab("Variance Explained") +
      theme_minimal()
  })
  
  output$explained_var_table <- renderDT({
    req(pca_result())
    eigenvalues <- eigen(cor(process_selected_data(data_input(), input$vars, input$remove_na)))$values
    cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)
    
    explained_variance_df <- data.frame(
      PrincipalComponents = 1:length(eigenvalues),
      Eigenvalues = round(eigenvalues, 3),
      VarianceExplained = round(eigenvalues / sum(eigenvalues), 3),
      CumulativeVarianceExplained = round(cumulative_variance, 3)
    )
    
    datatable(explained_variance_df, options = list(pageLength = 10, searching = TRUE))
  })
  
  output$mean_eigenvalue_text <- renderText({
    req(pca_result())
    eigenvalues <- eigen(cor(process_selected_data(data_input(), input$vars, input$remove_na)))$values
    paste("Mean Eigenvalue:", round(mean(eigenvalues), 3))
  })
  
  # Updated Principal Component Contributions Tab
  output$pc_instructions <- renderUI({
    req(pca_result())
    HTML("<h3>Instructions</h3><p>Select how many principal components to retain according to the criteria performed before. You can retain as many as the total number of original variables. Generally, you would select the number of components that explain a significant portion of the variance (e.g., 80-90%).</p>")
  })
  
  observe({
    req(pca_result())
    data <- data_input()
    selected_data <- process_selected_data(data, input$vars, input$remove_na)
    
    # Update the selection for the number of principal components to retain
    updateSelectInput(session, "retain_pc", 
                      choices = 1:ncol(selected_data), 
                      selected = 2)
  })
  
  # Correlation Table
  output$correlation_table <- renderDT({
    req(pca_result())
    pca <- pca_result()
    selected_data <- process_selected_data(data_input(), input$vars, input$remove_na)
    
    correlations <- cor(selected_data[, input$vars], pca$x)
    
    datatable(as.data.frame(round(correlations, 3)), options = list(pageLength = 10, searching = TRUE))
  })
  
  # Table for Principal Component Contributions
  output$pc_contributions_table <- renderDT({
    req(pca_result())
    pca <- pca_result()
    selected_data <- process_selected_data(data_input(), input$vars, input$remove_na)
    
    contributions <- as.data.frame(pca$rotation[, 1:ncol(selected_data)]) 
    contributions <- round(contributions, 3)
    contributions <- cbind(Variable = rownames(contributions), contributions)
    
    datatable(contributions, options = list(pageLength = 10, searching = TRUE))
  })
  
  # Displaying Equations for Principal Components
  output$pc_equations <- renderPrint({
    req(pca_result(), input$retain_pc)
    pca <- pca_result()
    retain_pc <- as.numeric(input$retain_pc)
    
    equations <- lapply(1:retain_pc, function(pc) {
      components <- pca$rotation[, pc]
      terms <- paste0(rownames(pca$rotation), " * ", round(components, 3))
      paste0("PC", pc, " = ", paste(terms, collapse = " + "))
    })
    
    # Collapse the list into a single string and print it
    cat(paste(equations, collapse = "\n"))
  })
  
  # Biplot Tab
  output$biplot <- renderPlot({
    req(pca_result())
    pca <- pca_result()
    fviz_pca_biplot(pca, repel = TRUE, 
                    col.var = "purple", 
                    col.ind = "steelblue")
  })
  
  output$biplot_text <- renderText({
    "The biplot displays both the principal components and the original variables in the PCA space. Points represent the observations, while arrows indicate the variables' contributions."
  })
  
  # K-means Clustering Tab
  output$kmeans_plot <- renderPlot({
    req(pca_result())
    pc_data <- pca_result()$x[, 1:2]
    
    num_clusters <- input$num_clusters
    
    set.seed(123)
    kmeans_result <- kmeans(pc_data, centers = num_clusters) 
    
    kmeans_result(kmeans_result)  
    
    ggplot(as.data.frame(pc_data), aes(x = PC1, y = PC2, color = factor(kmeans_result$cluster))) +
      geom_point(size = 3) +
      labs(title = paste("K-means Clustering with", num_clusters, "Clusters"), x = "PC1", y = "PC2") +
      scale_color_discrete(name = "Cluster") +
      theme_minimal()
  })
  
  output$cluster_means_table <- renderDT({
    req(kmeans_result())
    data <- data_input()
    selected_data <- process_selected_data(data, input$vars, input$remove_na)
    
    clusters <- kmeans_result()$cluster
    cluster_means <- aggregate(selected_data[, input$vars], by = list(Cluster = clusters), FUN = mean)
    
    cluster_means <- round(cluster_means, 3)
    datatable(cluster_means, options = list(pageLength = 10, searching = TRUE))
  })
}

shinyApp(ui, server)