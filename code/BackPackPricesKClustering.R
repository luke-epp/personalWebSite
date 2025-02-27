library(data.table)
library(dplyr)
library(FNN)       # For KNN regression
library(cluster)   # For K-means clustering
library(ggplot2)   # For visualization

# Load Data
train <- fread('/Users/lukeepp/Documents/KaggleBackPack/train.csv')
test <- fread('/Users/lukeepp/Documents/KaggleBackPack/test.csv')

# Define target variable
y <- train$Price
train$Price <- NULL  # Remove response column


# 🔹 Step 1: Feature Engineering (Using previous function)
feature_engineering <- function(df) {
  df <- df %>%
    group_by(Brand) %>%
    mutate(
      Brand.Popularity = n(),
      Brand.Vowel.Count = str_count(Brand, "[AEIOUaeiou]"),
      Brand.Consonant.Count = nchar(Brand) - str_count(Brand, "[AEIOUaeiou ]")
    ) %>%
    ungroup() %>%
    
    group_by(Brand, Style) %>%
    mutate(Brand.Style.Popularity = n()) %>%
    ungroup() %>%
    
    mutate(
      `Laptop Compartment` = as.integer(`Laptop Compartment` == "Yes"),
      Waterproof = as.integer(Waterproof == "Yes")
    ) |> 
    mutate(`Weight Capacity (kg)` = if_else(is.na(`Weight Capacity (kg)`), 0, `Weight Capacity (kg)`))
  
  return(df)
}
one_hot_encode <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.integer))  # Convert factors to integers
  
  return(df)
}
# Apply feature engineering
train <- feature_engineering(train)
test <- feature_engineering(test)

train <- one_hot_encode(train)
test <- one_hot_encode(test)

# Standardize numerical features
train <- train %>% mutate(across(where(is.numeric), ~scale(.)))
test <- test %>% mutate(across(where(is.numeric), ~scale(.)))

# K-Means Clustering for Grouping Backpacks
set.seed(42)
num_clusters <- 10  # Set an appropriate number of clusters
km_model <- kmeans(train, centers = num_clusters, nstart = 10)
train$cluster <- km_model$cluster
assign_clusters <- function(test_data, cluster_centers) {
  apply(test_data, 1, function(x) which.min(colSums((t(cluster_centers) - x)^2)))
}

test$cluster <- assign_clusters(test, km_model$centers)


# Aggregate features within each cluster (mean, median, etc.)
grouped_features <- train %>%
  group_by(cluster) %>%
  summarise(across(everything(), list(mean = mean, median = median), .names = "{.col}_{.fn}"))

# Merge Aggregated Features Back
train <- left_join(train, grouped_features, by = "cluster")
test <- left_join(test, grouped_features, by = "cluster")

# KNN Regression - Train on Each Cluster Separately
predictions <- numeric(nrow(test))
for (i in 1:num_clusters) {
  cluster_train <- train %>% filter(cluster == i) %>% select(-cluster)
  cluster_test <- test %>% filter(cluster == i) %>% select(-cluster)
  
  if (nrow(cluster_train) > 5 & nrow(cluster_test) > 0) {
    knn_model <- knn.reg(train = cluster_train, y = y[train$cluster == i], test = cluster_test, k = 5)
    predictions[test$cluster == i] <- knn_model$pred
  }
}

# Save Predictions
output <- data.table(id = ids, Price = predictions)
fwrite(output, "/Users/lukeepp/Documents/KaggleBackPack/my_preds_knn_means.csv")
