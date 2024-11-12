Figure 1 Yeo Networks Gradients Cloud
================

Import Required Packages

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
library(cowplot) 
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
```

Prepare Data

``` r
# Define a function to filter and set factor levels for the YeoNets column
prepare_data <- function(data) {
  data %>%
    filter(YeoNets != 'LN') %>%
    mutate(YeoNets = factor(YeoNets, levels = c('DMN', 'FPN', 'SN', 'DAN', 'SMN', 'VN')))
}


calculate_all_day_means <- function(data, data_g2, data_g3) {
  data %>%
    mutate(
      # Calculate row means for G1 (columns 2-31, 32-61, 62-91)
      G1_Day1 = rowMeans(.[, 2:31]),
      G1_Day3 = rowMeans(.[, 32:61]),
      G1_Day30 = rowMeans(.[, 62:91]),
      
      # Calculate row means for G2 (columns 2-31, 32-61, 62-91 in data_g2)
      G2_Day1 = rowMeans(data_g2[, 2:31]),
      G2_Day3 = rowMeans(data_g2[, 32:61]),
      G2_Day30 = rowMeans(data_g2[, 62:91]),
      
      # Calculate row means for G3 (columns 2-31, 32-61, 62-91 in data_g3)
      G3_Day1 = rowMeans(data_g3[, 2:31]),
      G3_Day3 = rowMeans(data_g3[, 32:61]),
      G3_Day30 = rowMeans(data_g3[, 62:91])
    )
}

# Apply the function to the filtered data
```

Split the data, and create average per gradient per sessions

``` r
# Apply the function to each dataset
g1_filtered <- prepare_data(G1)
g2_filtered <- prepare_data(G2)
g3_filtered <- prepare_data(G3)  

data <- calculate_all_day_means(g1_filtered, g2_filtered, g3_filtered)
```

Gradient Cloud Plots

Gradient 1 vs Gradient 3 (For each sensstion (Day1/ Day3/ Day30))

``` r
filteredData<- data %>% filter(YeoNets == 'DMN' | YeoNets == 'SMN')
# Main plot
pmain <- ggplot(data,aes(G1_Day30,G3_Day30,color=YeoNets,fill=YeoNets))+
  geom_point()+
  ggpubr::color_palette(c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286'))+
  labs(title = " G1_Day30 - G3_Day30", x = "G1_Day30", y = "G3_Day30")+
  theme(plot.title = element_text(hjust = 0.5))
pmain
```

![](fig1_gradients_cloud_files/figure-gfm/plot_gradient_cloud-1.png)<!-- -->

``` r
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data= data,aes(G1_Day30,color=YeoNets,fill=YeoNets),alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette(c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286'))+
  ggpubr::color_palette(c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286'))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data=data,aes(G3_Day30,color=YeoNets,fill=YeoNets),
                alpha = 0.7, size = 0.2)+
  coord_flip()+
  ggpubr::fill_palette(c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286'))+
  ggpubr::color_palette(c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286'))
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")

p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)
```

![](fig1_gradients_cloud_files/figure-gfm/plot_gradient_cloud-2.png)<!-- -->

``` r
#ggsave(file="Paper_Cloud-G1_Day30-G3_Day30.png", p2, width=6, height=5, dpi=400)
```

``` r
# Reshape data to a long format to make it easier to plot Day 3 and Day 30 together
df_long <- data %>%
  pivot_longer(cols = starts_with("G"), names_to = "Gradient_Day", values_to = "Value") %>%
  separate(Gradient_Day, into = c("Gradient", "Day"), sep = "_")

# Filter for G1 and G3 gradients only, and reshape to wide format for plotting
df_filtered <- df_long %>%
  filter(Gradient %in% c("G1","G2", "G3")) %>%
  pivot_wider(names_from = "Gradient", values_from = "Value")
```

``` r
filteredData<- df_filtered %>% filter(YeoNets == 'VN')
p3 <- ggplot(df_filtered, aes(x = G1, y = G2, color = YeoNets)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Day, ncol = 3) +  # Specify the number of columns (optional, already in 3 columns here)
  labs(title = "Comparison Across Days", x = "G1", y = "G2") +
  theme_minimal() +
  scale_fill_manual(values = c('#CD3E4E', '#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286')) +
  scale_color_manual(values = c('#CD3E4E', '#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286')) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(2, "lines")  # Adjusts the space between facets
  )

# Save the plot
#ggsave(file = "Paper_comparison_across_days_G1_G2.png", p3, width = 15, height = 8, dpi = 400)  # Increased width for more space
p3
```

![](fig1_gradients_cloud_files/figure-gfm/Comparison%20Across%20Days-1.png)<!-- -->

``` r
# Define a helper function to process each gradient data frame
process_gradient <- function(data, col_names, gradient_label) {
  data %>%
    select(all_of(col_names)) %>%
    pivot_longer(cols = everything(), names_to = "Subject_Session", values_to = "Value") %>%
    mutate(Day = case_when(
      grepl("ses.1", Subject_Session) ~ "Day 1",
      grepl("ses.2", Subject_Session) ~ "Day 3",
      grepl("ses.3", Subject_Session) ~ "Day 30"
    ),
    Gradient = gradient_label)
}

# Define the main function to get the subject data for each gradient
get_subject_data <- function(subject_num) {
  # Define the column names dynamically based on the subject number
  col_names <- paste0("sub.", subject_num, ".ses.", 1:3)
  
  # Process each gradient data frame using the helper function
  df_g1 <- process_gradient(G1, col_names, "G1")
  df_g2 <- process_gradient(G2, col_names, "G2")
  df_g3 <- process_gradient(G3, col_names, "G3")
  
  # Combine the data frames
  combined_df <- bind_rows(df_g1, df_g2, df_g3)
  
  # Return the combined data frame
  return(combined_df)
}

# Example usage for subject 29
df_sub10 <- get_subject_data(10)
print(df_sub10)
```

    ## # A tibble: 168,435 × 4
    ##    Subject_Session Value Day    Gradient
    ##    <chr>           <dbl> <chr>  <chr>   
    ##  1 sub.10.ses.1    21.3  Day 1  G1      
    ##  2 sub.10.ses.2    13.2  Day 3  G1      
    ##  3 sub.10.ses.3     9.18 Day 30 G1      
    ##  4 sub.10.ses.1     8.04 Day 1  G1      
    ##  5 sub.10.ses.2     5.78 Day 3  G1      
    ##  6 sub.10.ses.3     2.08 Day 30 G1      
    ##  7 sub.10.ses.1     7.73 Day 1  G1      
    ##  8 sub.10.ses.2    17.8  Day 3  G1      
    ##  9 sub.10.ses.3     2.03 Day 30 G1      
    ## 10 sub.10.ses.1    18.6  Day 1  G1      
    ## # ℹ 168,425 more rows

``` r
filteredData_G3<- df_long %>% filter(Gradient == 'G3')

p2 <- ggplot(filteredData_G3, aes(x = Value, y = Gradient, linetype = Day)) +
  geom_density_ridges(color = "black", fill = NA, linewidth = 1.5) +  # Set line color to black
  scale_x_continuous(limits = c(-25, 35)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +  # Set different line types for each day
  coord_cartesian(clip = 'off') +
  labs(y = '', title = 'Gradient 3 Across Days Average') +
  theme_ridges() +
  theme(
    legend.position = 'right',
    plot.title = element_text(face = 'bold', size = 20, hjust = 0.5)
  )

p2
```

    ## Picking joint bandwidth of 0.808

![](fig1_gradients_cloud_files/figure-gfm/Gradient%203%20Across%20Days%20For%20Subj%2010-1.png)<!-- -->

``` r
ggsave(file="Paper_distribution_diff_All_G3_limited_axis_AVG.png", p2, width=6, height=5, dpi=400)
```

    ## Picking joint bandwidth of 0.808
