library(tidyverse)
library(ggplot2)
library(randomForest)

setwd("D:/study/UNC_Biostatistics/2024 fall/BIOS 611/project")

# each rows is written in a cell in the csv files. need to use this function to import.
import <- function(file_path) {
  lines <- readLines(file_path)
  
  split_lines <- strsplit(lines, ";")
  
  max_columns <- length(split_lines[[1]])
  split_lines <- lapply(split_lines, function(row) {
    length(row) <- max_columns  
    row
  })


  df <- do.call(rbind, lapply(split_lines, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)))
  
  colnames(df) <- df[1, ]
  df <- df[-1, ]  

  df <- data.frame(lapply(df, function(x) {
    if (all(!is.na(as.numeric(x)))) as.numeric(x) else x
  }), stringsAsFactors = FALSE)
  
  return(df)
}


defend <- import("data/england_premier_league_squad_defensive_actions_22.csv")
passing <-import("data/england_premier_league_squad_passing_stats_22.csv")
possession <- import("data/england_premier_league_squad_possession_22.csv")
shooting <- import("data/england_premier_league_squad_shooting_22.csv")

data <- merge(defend, passing, by=common_columns)
data <- merge(data, possession, by= common_columns)
data <- merge(data, shooting, by = common_columns) 

data<-data %>% select(-NPl) %>% select(where(~ all(!is.na(.))))
data$Standing <- c(4, 14, 13, 9, 18, 3, 12, 16, 17, 8, 2, 1, 6, 11, 20, 15, 4, 19, 7, 10)

# Question 4

# Plot the teams on a scatter plot of the first 2 PCs
pca_data <- pca_data %>%
  select(where(~ var(.) > 0))

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x[, 1:2])  # Get PC1 and PC2
colnames(pca_scores) <- c("PC1", "PC2")
pca_scores <- pca_scores %>%
  mutate(Squad = data$Squad, standing = data$standing)

plot1 <- ggplot(pca_scores, aes(x = PC1, y = PC2, label = Squad, color = data$Standing)) +
  geom_point(size = 4) +  
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + 
  scale_color_gradient(low = "blue", high = "red") +  
  labs(
    title = "PCA Scatter Plot of Teams",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Standing"
  ) +
  theme_minimal()

ggsave("pca.png", plot1, width =10,height =6,dpi = 200 )

# Select the number of PCs for clustering
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)

variance_df <- data.frame(
  PC = seq_along(explained_variance),
  ExplainedVariance = explained_variance,
  CumulativeVariance = cumulative_variance
)

variance_df_long <- variance_df %>%
  pivot_longer(cols = c(ExplainedVariance, CumulativeVariance),
               names_to = "Type",
               values_to = "Variance")

plot2 <- ggplot(variance_df_long, aes(x = PC, y = Variance, color = Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c("ExplainedVariance" = "blue", "CumulativeVariance" = "red"),
    labels = c("Explained Variance", "Cumulative Variance")
  ) +
  labs(
    title = "Scree Plot with Explained and Cumulative Variance",
    x = "Principal Component",
    y = "Variance Explained (%)",
    color = "Variance Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("pca_var.png", plot2, width =5,height =3,dpi = 200 )


# KNN
set.seed(123) 
kmeans_result <- kmeans(pca_scores[, c("PC1", "PC2")], centers = 4, nstart = 25)

pca_scores <- pca_scores %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

plot3 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Cluster, label = data$Standing)) +
  geom_point(size = 4, alpha = 0.8) + 
  geom_text(vjust = -1, hjust = 0.5, size = 3) + 
  labs(
    title = "K-Means Clustering on PCA",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("knn.png", plot3, width =10,height =6,dpi = 200 )


# Question 5
defend <- defend %>% select(-NPl) %>% select(where(~ all(!is.na(.)))) %>% mutate(Standing = data$Standing)
passing <- passing %>% select(-NPl) %>% select(where(~ all(!is.na(.)))) %>% mutate(Standing = data$Standing)
possession <- possession %>% select(-NPl) %>% select(where(~ all(!is.na(.)))) %>% mutate(Standing = data$Standing)
shooting <- shooting %>% select(-NPl) %>% select(where(~ all(!is.na(.)))) %>% mutate(Standing = data$Standing)

extract_pc1 <- function(data, exclude_cols) {
  pca_data <- data %>% select(-all_of(exclude_cols)) %>% drop_na()
  pca_data <- pca_data[, apply(pca_data, 2, var) > 0]
  
  pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)
  pc1 <- pca_result$x[, 1]
  return(data.frame(PC1 = pc1))
}


defend_pc <- extract_pc1(defend, exclude_cols = c("Squad", "Standing"))
passing_pc <- extract_pc1(passing, exclude_cols = c("Squad", "Standing"))
possession_pc <- extract_pc1(possession, exclude_cols = c("Squad", "Standing"))
shooting_pc <- extract_pc1(shooting, exclude_cols = c("Squad", "Standing"))

combined_data <- data.frame(
  Squad = defend$Squad,
  Standing = defend$Standing,
  Defend_PC1 = defend_pc$PC1,
  Passing_PC1 = passing_pc$PC1,
  Possession_PC1 = possession_pc$PC1,
  Shooting_PC1 = shooting_pc$PC1
)

combined_data <- combined_data %>%
  mutate(Standing_Binary = if_else(as.numeric(Standing) <= 10, 1, 0)) %>%
  select(-Standing)  %>%
  mutate(Standing_Binary = as.factor(Standing_Binary))

# Random Forest 
set.seed(123) 
rf_model <- randomForest(
  Standing_Binary ~ . - Squad,
  data = combined_data,
  mtry = 2,
  importance = TRUE
)

importance_df <- as.data.frame(importance(rf_model))
importance_df <- importance_df %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(MeanDecreaseGini))

print(importance_df)

plot4 <- ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance (Random Forest)",
    x = "Feature",
    y = "Mean Decrease in Gini Index"
  ) +
  theme_minimal()
ggsave("importance.png", plot4, width =5,height =3,dpi = 200 )

