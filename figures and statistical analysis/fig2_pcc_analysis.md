fig2_icc_analysis
================
Yvonne Serhan
2024-11-04

``` r
library(ggsci)
library(cowplot)
library(ggthemes)
library(ggridges)
library(philentropy)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(psych)
library(ICC)
library(lattice)
library(lpSolve)
library(irr)
library(plot.matrix)
library(reshape)
library(plotly)
library(tidyverse)
library(svglite)
library(viridis)
library(ReX)
```

Load Gradients (G1,G2,G3) Data

``` r
G1 = read.csv('../Brainspace/G1_results_BS_p_20K_20K_Dan.csv')
unique_names = unique(G1$YeoNets)
G1$YeoNets = factor(G1$YeoNets, levels = unique_names)

G2 = read.csv('../Brainspace/G2_results_BS_p_20K_20K_Dan.csv')
unique_names = unique(G2$YeoNets)
G2$YeoNets = factor(G2$YeoNets, levels = unique_names)

G3 = read.csv('../Brainspace/G3_results_BS_p_20K_20K_Dan.csv')
unique_names = unique(G3$YeoNets)
G3$YeoNets = factor(G3$YeoNets, levels = unique_names)

# List of gradients to process
gradients <- c("G1", "G2", "G3")

# Create an empty data frame with columns
net_df <- data.frame(Gradient = character(),
                 Day = integer(),
                 ACC_col = numeric(),
                 ACC_row = numeric(),
                 ACC_both = numeric(),
                 stringsAsFactors = TRUE)

# Loop through each network
for (i in gradients) {
  # Insert data for each network
  net_df <- rbind(net_df, c(paste0(i),3,0,0,0))
  net_df <- rbind(net_df, c(paste0(i),30,0,0,0))
}
colnames(net_df)<- c("Gradient","Day","ACC_COL","ACC_ROW","ACC_BOTH")
```

``` r
# Define a function to calculate accuracy metrics
calculate_accuracy <- function(matrix_df) {
  AccuRow <- 0
  AccuCol <- 0
  AccuBoth <- 0
  for (k in 1:ncol(matrix_df)) {
    row <- matrix_df[k,]
    col <- matrix_df[,k]
    colMax <- which.max(col)
    rowMax <- which.max(row)
    if (rowMax == k) AccuRow <- AccuRow + 1
    if (colMax == k) AccuCol <- AccuCol + 1
    if (colMax == rowMax && rowMax == k) AccuBoth <- AccuBoth + 1
  }
  list(AccuRow = (AccuRow / 30) * 100, AccuCol = (AccuCol / 30) * 100, AccuBoth = (AccuBoth / 30) * 100)
}

# Define a function to create and save plot
create_save_plot <- function(data, x_label, y_label, title, filename) {
  p <- ggplot(data, aes(x = variable, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(limits = c(0, 0.95), colours = c("white", "lightblue", "red")) +
    labs(x = x_label, y = y_label, title = title) +
    theme(
      plot.title = element_text(face = 'bold', size = 10, hjust = 0.5),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8, angle = 45, hjust = 1)
    )
  ggsave(filename, p, path = "cole/", width = 6, height = 5, dpi = 400)
  ggplotly(p) # Convert to interactive plot if needed
}
```

``` r
# Outer loop for each gradient
for (gradient in gradients) {
  gradient_data <- get(gradient)
  
  # Filter data for each session based on the current gradient data
  Ses1 <- gradient_data %>% select(2:31)
  Ses2 <- gradient_data %>% select(32:61)
  Ses10 <- gradient_data %>% select(62:91)
  # Initialize ICC matrices
  iccMatrix02 <- matrix(ncol = ncol(Ses1), nrow = ncol(Ses2))
  iccMatrix10 <- matrix(ncol = ncol(Ses1), nrow = ncol(Ses10))
  
  # Calculate ICC for each pair of columns
  for (i in 1:ncol(Ses2)) {
    for (j in 1:ncol(Ses1)) {
      v_ses2 <- cor(Ses1[, i], Ses2[, j], method = 'pearson')
      v_ses10 <- cor(Ses1[, i], Ses10[, j], method = 'pearson')
      iccMatrix02[i, j] <- v_ses2
      iccMatrix10[i, j] <- v_ses10
    }
  }
  
  
  # Convert matrices to data frames
  matrix_df_2 <- as.data.frame(iccMatrix02)
  matrix_df_10 <- as.data.frame(iccMatrix10)
  subjName <- paste0("Sub", 1:30)
  colnames(matrix_df_2) <- subjName; rownames(matrix_df_2) <- subjName
  colnames(matrix_df_10) <- subjName; rownames(matrix_df_10) <- subjName

  y<- colnames(matrix_df_2)
  x<- rownames(matrix_df_2)
  y<-rep(y, times = 30)
  
  # Melt data for ggplot
  df_2 <- melt(matrix_df_2); df_10 <- melt(matrix_df_10)
  
  df_2$y <- factor(y,subjName)
  df_10$y <- factor(y,subjName)

  
  # Plot and save matrices with gradient name in title and filename
  plot_title_10 <- paste("PCC", gradient, "Day 1 vs Day 30", sep = "-")
  filename_10 <- paste(gradient,  "New_PCC_SES01-10_20k_20k.png", sep = "_")
  create_save_plot(df_10, "Day 30", "Day 1", plot_title_10, filename_10)
  
  plot_title_2 <- paste("PCC", gradient, "Day 1 vs Day 3", sep = "-")
  filename_2 <- paste(gradient,  "New_PCC_SES01-2_20k_20k.png", sep = "_")
  create_save_plot(df_2, "Day 3", "Day 1", plot_title_2, filename_2)
  
  # Calculate and print accuracy metrics for Day 3 and Day 30
  accuracy_2 <- calculate_accuracy(matrix_df_2)
  accuracy_10 <- calculate_accuracy(matrix_df_10)
  
  print(accuracy_2$AccuBoth); print(accuracy_2$AccuRow); print(accuracy_2$AccuCol)
  print(accuracy_10$AccuBoth); print(accuracy_10$AccuRow); print(accuracy_10$AccuCol)
  
  # Update net_df with accuracy metrics
  net_df[ net_df$Gradient == gradient & net_df$Day == 3, c("ACC_ROW", "ACC_COL","ACC_BOTH")] <- unlist(accuracy_2)
  net_df[net_df$Gradient == gradient & net_df$Day == 30 , c("ACC_ROW", "ACC_COL","ACC_BOTH")] <- unlist(accuracy_10)
  
  # Save matrices as CSV files with gradient name in filename
  write.csv(matrix_df_2, paste(gradient, "New_PCC_SES01-2_20k_20k.csv", sep = "_"), row.names = FALSE)
  write.csv(matrix_df_10, paste(gradient, "New_PCC_SES01-10_20k_20k.csv", sep = "_"), row.names = FALSE)
  
   write.csv(net_df, "New_PCC_ACC.csv", row.names = FALSE)
}
```

    ## [1] 83.33333
    ## [1] 83.33333
    ## [1] 86.66667
    ## [1] 73.33333
    ## [1] 80
    ## [1] 76.66667

    ## [1] 53.33333
    ## [1] 66.66667
    ## [1] 63.33333
    ## [1] 36.66667
    ## [1] 43.33333
    ## [1] 53.33333

    ## [1] 56.66667
    ## [1] 73.33333
    ## [1] 66.66667
    ## [1] 66.66667
    ## [1] 70
    ## [1] 73.33333
