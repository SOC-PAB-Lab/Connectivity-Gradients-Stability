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
g1 = read.csv('../Brainspace/G1_results_BS_p_20K_20K_Dan.csv')
unique_names = unique(g1$YeoNets)
g1$YeoNets = factor(g1$YeoNets, levels = unique_names)

g2 = read.csv('../Brainspace/G2_results_BS_p_20K_20K_Dan.csv')
unique_names = unique(g2$YeoNets)
g2$YeoNets = factor(g2$YeoNets, levels = unique_names)

g3 = read.csv('../Brainspace/G3_results_BS_p_20K_20K_Dan.csv')
unique_names = unique(g3$YeoNets)
g3$YeoNets = factor(g3$YeoNets, levels = unique_names)
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
g1_filtered <- prepare_data(g1)
g2_filtered <- prepare_data(g2)
g3_filtered <- prepare_data(g3)  

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

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
#ggsave(file="Paper_Cloud-G1_Day30-G3_Day30.png", p2, width=6, height=5, dpi=400)
```

``` r
filteredData<-data %>% filter(YeoNets != 'LN')
p5<-ggplot(data, aes(G1_Day3,G3_Day3,color=YeoNets,fill=YeoNets)) +
  geom_point()+
  scale_fill_manual(values = c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286')) +
  scale_color_manual(values = c('#CD3E4E','#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286'))

p5
```

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Load your data (assuming it is saved as a CSV file)
# Replace 'your_data_file.csv' with your actual data file path
df <- data

# Calculate differences between Day 3 and Day 30 for each gradient
df <- df %>%
  mutate(G1_Diff = G1_Day30 - G1_Day3,
         G2_Diff = G2_Day30 - G2_Day3,
         G3_Diff = G3_Day30 - G3_Day3)

# 1. Scatter Plot to Compare G1 vs G3 for Day 3 and Day 30
ggplot(df, aes(x = G1_Day30, y = G3_Day30, color = YeoNets)) +
  geom_point(alpha = 0.6) +
  labs(title = "G1 vs G3 on Day 30", x = "G1 Day 30", y = "G3 Day 30") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
```

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# 2. Violin/Box Plot to Show Distribution of Differences
df_long <- df %>%
  select(YeoNets, G1_Diff, G2_Diff, G3_Diff) %>%
  pivot_longer(cols = starts_with("G"), names_to = "Gradient", values_to = "Difference")

ggplot(df_long, aes(x = Gradient, y = Difference, fill = YeoNets)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  labs(title = "Distribution of Gradient Differences (Day 30 - Day 3)", x = "Gradient", y = "Difference") +
  theme_minimal()
```

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# 3. Paired T-Tests (for each gradient)
t_test_results <- list(
  G1 = t.test(df$G1_Day3, df$G1_Day30, paired = TRUE),
  G2 = t.test(df$G2_Day3, df$G2_Day30, paired = TRUE),
  G3 = t.test(df$G3_Day3, df$G3_Day30, paired = TRUE)
)

# Print T-test results
print(t_test_results)
```

    ## $G1
    ## 
    ##  Paired t-test
    ## 
    ## data:  df$G1_Day3 and df$G1_Day30
    ## t = -7.2328, df = 17276, p-value = 4.931e-13
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08610360 -0.04938562
    ## sample estimates:
    ## mean difference 
    ##     -0.06774461 
    ## 
    ## 
    ## $G2
    ## 
    ##  Paired t-test
    ## 
    ## data:  df$G2_Day3 and df$G2_Day30
    ## t = -4.9576, df = 17276, p-value = 7.205e-07
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.05182600 -0.02245663
    ## sample estimates:
    ## mean difference 
    ##     -0.03714132 
    ## 
    ## 
    ## $G3
    ## 
    ##  Paired t-test
    ## 
    ## data:  df$G3_Day3 and df$G3_Day30
    ## t = 9.2712, df = 17276, p-value < 2.2e-16
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.08687243 0.13345358
    ## sample estimates:
    ## mean difference 
    ##        0.110163

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
p3 <- ggplot(df_filtered, aes(x = G1, y = G3, color = YeoNets)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Day, ncol = 3) +  # Specify the number of columns (optional, already in 3 columns here)
  labs(title = "Comparison Across Days", x = "G1", y = "G3") +
  theme_minimal() +
  scale_fill_manual(values = c('#CD3E4E', '#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286')) +
  scale_color_manual(values = c('#CD3E4E', '#E69422', '#BE3AFA', '#00760E', '#4682B4', '#781286')) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(2, "lines")  # Adjusts the space between facets
  )

# Save the plot
ggsave(file = "Paper_comparison_across_days.png", p3, width = 15, height = 8, dpi = 400)  # Increased width for more space
p3
```

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
df_filtered$YeoNets = factor(df_filtered$YeoNets, levels = c('DMN','FPN','SN','DAN','VN','SMN'))

p2 <- ggplot(df_filtered, aes(x = G2, y = YeoNets, color = Day, fill = Day)) +
  geom_density_ridges(alpha = 0.3, scale = 3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
scale_fill_manual(values = c('#1f78b4', '#33a02c', '#6a3d9a')) +
scale_color_manual(values = c('#1f78b4', '#33a02c', '#6a3d9a'))+
  coord_cartesian(clip = 'off') +
  labs(y = '', title = 'G2 - Yeo Networks Across Days') +
  theme_ridges() +
  theme(
    legend.position = 'right',
    plot.title = element_text(face = 'bold', size = 20, hjust = 0.5)
  )

p2
```

    ## Picking joint bandwidth of 0.693

![](fig1_gradients_cloud_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave(file="Paper_distribution_diff_G2.png", p2, width=6, height=5, dpi=400)
```

    ## Picking joint bandwidth of 0.693