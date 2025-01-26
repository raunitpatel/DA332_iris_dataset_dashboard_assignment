# Iris Dataset Shiny Dashboard

This is a Shiny dashboard that visualizes various aspects of the **Iris dataset**. It allows users to explore the data with histograms, scatter plots, box plots, K-means clustering, and correlation heatmaps. The dashboard also provides statistical information such as mean values for different features of the Iris dataset based on selected species.

## Features

- **Histograms**: Display histograms of Sepal Length, Sepal Width, Petal Length, and Petal Width.
- **Box Plots**: View box plots of various features grouped by species.
- **Scatter Plots**: Interactive scatter plots comparing different pairs of features.
- **K-Means Clustering**: K-means clustering with `k=3` on combinations of features.
- **Correlation Heatmap**: A heatmap visualizing the correlation between different features.
- **Violin Plots**: Interactive violin plots for visualizing the distribution of features.
- **3D Scatter Plot**: A 3D scatter plot visualizing relationships between three features.

## Requirements

To run this project locally, you need to install the following R packages:

- `shiny`
- `shinydashboard`
- `ggplot2`
- `plotly`
- `reshape2`
- `Hmisc`

You can install them by running the following command in R:

```R
install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", "reshape2", "Hmisc"))
