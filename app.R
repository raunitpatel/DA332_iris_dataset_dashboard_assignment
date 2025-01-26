library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(reshape2)
library(Hmisc)

df <- iris

plot.histogram <- function(df, var, xlab, ylab) {
  out <- ggplot(df, aes_string(x = var)) +
    geom_histogram(bins = 15, fill = "#42c2f5", color = "white", alpha = 0.7) +
    labs(x = xlab, y = ylab)
  return(out)
}

plot.boxplot <- function(df, var, xlab, ylab) {
  out <- ggplot(df, aes_string(x = "Species", y = var, fill = "Species")) +
    geom_boxplot() +
    labs(x = xlab, y = ylab) +
    theme_minimal()
  return(out)
}

plot.scatter <- function(df, var.x, var.y, c.factor, xlab, ylab, lname) {
  out <- ggplot(df, aes_string(x = var.x, y = var.y, color = c.factor)) +
    geom_point() +
    labs(x = xlab, y = ylab) +
    scale_color_discrete(name = lname)
  return(out)
}



ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = HTML('Iris Dataset Dashboard'),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Histograms", tabName = "histograms", icon = icon("fas fa-list")),
      menuItem("Scatter Plots", tabName = "scatter_plots", icon = icon("fas fa-list")),
      menuItem("Box Plots", tabName = "box_plots", icon = icon("fas fa-list")),
      menuItem("K-Means Clustering", tabName = "kmeans_clustering", icon = icon("fas fa-list")),
      menuItem("Correlation Heatmap", tabName = "correlation_heatmap", icon = icon("fas fa-list")),
      menuItem("Violin Plots", tabName = "violin_plots", icon = icon("fas fa-list")),
      menuItem("3D Scatter Plot", tabName = "scatter_3d", icon = icon("fas fa-list"))
    ),
    tags$style(HTML('
      .main-sidebar { background-color: #ffeb3b; }
      .sidebar-menu > li > a { color: #d32f2f; }
      .sidebar-menu > li > a:hover { background-color: #d32f2f; color: white; }
      .sidebar-header { background-color: #d32f2f; color: white; }
    '))
  ),
  
  dashboardBody(
    tags$style(HTML('
      .content-wrapper { background-color: #fff9c4; }
      .box { border-color: #d32f2f; }
      .box-header { background-color: #d32f2f; color: white; }
    ')),
    
    
    tabItems(
      tabItem(tabName = "histograms",
              fluidRow(
                box(width = 2,
                    selectInput(inputId = "select_specie", label = "Select Species", 
                                choices = unique(df$Species), 
                                selected = 1)),
                valueBoxOutput(width = 2, outputId = "value_observations"),
                valueBoxOutput(width = 2, outputId = "value_sepal_len_mean"),
                valueBoxOutput(width = 2, outputId = "value_sepal_wid_mean"),
                valueBoxOutput(width = 2, outputId = "value_petal_len_mean"),
                valueBoxOutput(width = 2, outputId = "value_petal_wid_mean")
              ),
              fluidRow(
                box(title = "Histogram of Sepal Length", 
                    plotlyOutput(outputId = "hist_sepal_len")),
                box(title = "Histogram of Sepal Width", 
                    plotlyOutput(outputId = "hist_sepal_wid")),
                box(title = "Histogram of Petal Length", 
                    plotlyOutput(outputId = "hist_petal_len")),
                box(title = "Histogram of Petal Width", 
                    plotlyOutput(outputId = "hist_petal_wid"))
              )
      ),
      
      tabItem(
        tabName = "scatter_plots",
        fluidRow(
          box(title = "Scatter Plot: Petal Length vs Sepal Length", 
              plotlyOutput(outputId = "scatter_1")),
          box(title = "Scatter Plot: Petal Width vs Sepal Width", 
              plotlyOutput(outputId = "scatter_2")),
          box(title = "Scatter Plot: Petal Length vs Petal Width", 
              plotlyOutput(outputId = "scatter_3")),
          box(title = "Scatter Plot: Sepal Length vs Petal Length", 
              plotlyOutput(outputId = "scatter_4")),
          box(title = "Scatter Plot: Sepal Width vs Petal Length", 
              plotlyOutput(outputId = "scatter_5")),
          box(title = "Scatter Plot: Sepal Width vs Petal Width", 
              plotlyOutput(outputId = "scatter_6"))
        )
      ),
      
      tabItem(
        tabName = "box_plots",
        fluidRow(
          box(title = "Box Plot of Sepal Length", 
              plotlyOutput(outputId = "box_sepal_len")),
          box(title = "Box Plot of Sepal Width", 
              plotlyOutput(outputId = "box_sepal_wid")),
          box(title = "Box Plot of Petal Length", 
              plotlyOutput(outputId = "box_petal_len")),
          box(title = "Box Plot of Petal Width", 
              plotlyOutput(outputId = "box_petal_wid"))
        )
      ),
      
      tabItem(
        tabName = "kmeans_clustering",
        fluidRow(
          box(width = 6, plotlyOutput(outputId = "kmeans_plot_1")),
          box(width = 6, plotlyOutput(outputId = "kmeans_plot_2")),
          box(width = 6, plotlyOutput(outputId = "kmeans_plot_3")),
          box(width = 6, plotlyOutput(outputId = "kmeans_plot_4")),
          box(width = 6, plotlyOutput(outputId = "kmeans_plot_5")),
          box(width = 6, plotlyOutput(outputId = "kmeans_plot_6"))
        )
      ),
      tabItem(
        tabName = "correlation_heatmap",
        fluidRow(
          box(title = "Correlation Heatmap", width = 12, plotlyOutput(outputId = "correlation_heatmap"))
        )
      ),
      tabItem(
        tabName = "violin_plots",
        fluidRow(
          box(title = "Violin Plot of Sepal Length", plotlyOutput(outputId = "violin_sepal_len")),
          box(title = "Violin Plot of Sepal Width", plotlyOutput(outputId = "violin_sepal_wid")),
          box(title = "Violin Plot of Petal Length", plotlyOutput(outputId = "violin_petal_len")),
          box(title = "Violin Plot of Petal Width", plotlyOutput(outputId = "violin_petal_wid"))
        )
      ),
      tabItem(
        tabName = "scatter_3d",
        fluidRow(
          box(title = "3D Scatter Plot", width = 12, plotlyOutput(outputId = "scatter_3d"))
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$hist_sepal_len <- renderPlotly({
    data <- df[df$Species == input$select_specie, ]
    plot <- plot.histogram(data, "Sepal.Length", "Sepal Length", "Frequency")
    ggplotly(plot)
  })
  
  output$hist_sepal_wid <- renderPlotly({
    data <- df[df$Species == input$select_specie, ]
    plot <- plot.histogram(data, "Sepal.Width", "Sepal Width", "Frequency")
    ggplotly(plot)
  })
  
  output$hist_petal_len <- renderPlotly({
    data <- df[df$Species == input$select_specie, ]
    plot <- plot.histogram(data, "Petal.Length", "Petal Length", "Frequency")
    ggplotly(plot)
  })
  
  output$hist_petal_wid <- renderPlotly({
    data <- df[df$Species == input$select_specie, ]
    plot <- plot.histogram(data, "Petal.Width", "Petal Width", "Frequency")
    ggplotly(plot)
  })
  
  output$value_observations <- renderValueBox({
    valueBox(
      nrow(df[df$Species == input$select_specie, ]), "Number of Observations", 
      color = "red"
    )
  })
  
  output$value_sepal_len_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_specie, "Sepal.Length"]),1), "Average Sepal Length",
      color = "yellow"
    )
  })
  
  output$value_sepal_wid_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_specie, "Sepal.Width"]),1), "Average Sepal Width", 
      color = "red"
    )
  })
  
  output$value_petal_len_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_specie, "Petal.Length"]),1), "Average Petal Length", 
      color = "yellow"
    )
  })
  
  output$value_petal_wid_mean <- renderValueBox({
    valueBox(
      round(mean(df[df$Species == input$select_specie, "Petal.Width"]),1), "Average Petal Width", 
      color = "red"
    )
  })
  
  output$scatter_1 <- renderPlotly({
    plot <- plot.scatter(df, "Petal.Length", "Sepal.Length", "Species", "Petal Length", "Sepal Length", "Species")
    ggplotly(plot)
  })
  
  output$scatter_2 <- renderPlotly({
    plot <- plot.scatter(df, "Petal.Width", "Sepal.Width", "Species", "Petal Width", "Sepal Width", "Species")
    ggplotly(plot)
  })
  
  output$scatter_3 <- renderPlotly({
    plot <- plot.scatter(df, "Petal.Length", "Petal.Width", "Species", "Petal Length", "Petal Width", "Species")
    ggplotly(plot)
  })
  
  output$scatter_4 <- renderPlotly({
    plot <- plot.scatter(df, "Sepal.Length", "Petal.Length", "Species", "Sepal Length", "Petal Length", "Species")
    ggplotly(plot)
  })
  
  output$scatter_5 <- renderPlotly({
    plot <- plot.scatter(df, "Sepal.Width", "Petal.Length", "Species", "Sepal Width", "Petal Length", "Species")
    ggplotly(plot)
  })
  
  output$scatter_6 <- renderPlotly({
    plot <- plot.scatter(df, "Sepal.Width", "Petal.Width", "Species", "Sepal Width", "Petal Width", "Species")
    ggplotly(plot)
  })
  
  # Box plot render functions
  output$box_sepal_len <- renderPlotly({
    plot <- plot.boxplot(df, "Sepal.Length", "Species", "Sepal Length")
    ggplotly(plot)
  })
  
  output$box_sepal_wid <- renderPlotly({
    plot <- plot.boxplot(df, "Sepal.Width", "Species", "Sepal Width")
    ggplotly(plot)
  })
  
  output$box_petal_len <- renderPlotly({
    plot <- plot.boxplot(df, "Petal.Length", "Species", "Petal Length")
    ggplotly(plot)
  })
  
  output$box_petal_wid <- renderPlotly({
    plot <- plot.boxplot(df, "Petal.Width", "Species", "Petal Width")
    ggplotly(plot)
  })
  # Generate K-means clustering plots for k = 3
  combinations <- combn(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 2, simplify = FALSE)
  
  for(i in 1:length(combinations)) {
    local({
      comb <- combinations[[i]]
      plot_id <- paste0("kmeans_plot_", i)
      
      output[[plot_id]] <- renderPlotly({
        # Apply K-means clustering with k = 3
        kmeans_result <- kmeans(df[, comb], centers = 3)
        df$Cluster <- as.factor(kmeans_result$cluster)
        
        plot <- ggplot(df, aes_string(x = comb[1], y = comb[2], color = "Cluster")) +
          geom_point() +
          stat_ellipse(aes(group = Cluster), level = 0.95) +  # Add ellipse boundary
          labs(x = comb[1], y = comb[2], title = paste("K-means clustering:", comb[1], "vs", comb[2])) +
          scale_color_discrete(name = "Cluster")
        
        ggplotly(plot)
      })
    })
  }
  output$correlation_heatmap <- renderPlotly({
    cor_matrix <- round(cor(df[, 1:4]), 2)
    cor_melt <- melt(cor_matrix)
    heatmap_plot <- ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap", x = "Features", y = "Features") +
      theme_minimal()
    ggplotly(heatmap_plot)
  })
  plot.violin <- function(df, var, ylab) {
    ggplot(df, aes_string(x = "Species", y = var, fill = "Species")) +
      # Add violin plot
      geom_violin(trim = FALSE, alpha = 0.7) +
      # Add median and interquartile range lines
      stat_summary(
        fun.data = "median_hilow", 
        geom = "pointrange", 
        color = "black", 
        position = position_dodge(width = 0.9),
      ) 
  }
  output$violin_sepal_len <- renderPlotly({
    plot <- plot.violin(df, "Sepal.Length", "Sepal Length")
    ggplotly(plot)
  })
  
  output$violin_sepal_wid <- renderPlotly({
    plot <- plot.violin(df, "Sepal.Width", "Sepal Width")
    ggplotly(plot)
  })
  
  output$violin_petal_len <- renderPlotly({
    plot <- plot.violin(df, "Petal.Length", "Petal Length")
    ggplotly(plot)
  })
  
  output$violin_petal_wid <- renderPlotly({
    plot <- plot.violin(df, "Petal.Width", "Petal Width")
    ggplotly(plot)
  })
  output$scatter_3d <- renderPlotly({
    plot_ly(
      data = df,
      x = ~Sepal.Length,
      y = ~Sepal.Width,
      z = ~Petal.Length,
      color = ~Species,
      colors = c("#636EFA", "#EF553B", "#00CC96"),
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 4)
    ) %>%
      layout(
        title = "3D Scatter Plot of Sepal and Petal Dimensions",
        scene = list(
          xaxis = list(title = "Sepal Length"),
          yaxis = list(title = "Sepal Width"),
          zaxis = list(title = "Petal Length")
        )
      )
  })
  
  
  
}

shinyApp(ui, server)
