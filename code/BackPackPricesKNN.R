library(dplyr)
library(readr)
library(stringr)
library(caret)  # For k-NN training & cross-validation
library(FNN)    # For fast k-NN prediction

# Load Data
train <- data.table::fread('/Users/lukeepp/Documents/KaggleBackPack/train.csv')
test <- data.table::fread('/Users/lukeepp/Documents/KaggleBackPack/test.csv')
ids = test$id
# Define the response variable
y <- train$Price
train <- train %>% select(-Price)  # Remove response from train set

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

# Apply feature engineering
train <- feature_engineering(train)
test <- feature_engineering(test)

# 🔹 Step 2: One-Hot Encoding for Categorical Variables
one_hot_encode <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.integer))  # Convert factors to integers
  
  return(df)
}

train <- one_hot_encode(train)
test <- one_hot_encode(test)

sum(is.na(train))

# 🔹 Step 3: Normalize Numeric Features
normalize <- function(df) {
  df <- as.data.frame(scale(df))  # Standardize all columns
  return(df)
}

train <- normalize(train)
test <- normalize(test)
colSums(is.na(train))
colSums(is.na(test))

# 🔹 Step 4: Train k-NN Model with Cross-Validation to Find Best k
set.seed(123)  # For reproducibility
# train_control <- trainControl(method = "cv", number = 5)  # 5-fold CV

# knn_model <- train(
#   x = train, 
#   y = y, 
#   method = "knn",
#   trControl = train_control,
#   tuneGrid = expand.grid(k = seq(3, 21, by = 2))  # Try k values from 3 to 21
# )

# # Best k value
# best_k <- knn_model$bestTune$k
# print(paste("Best k:", best_k))

# 🔹 Step 5: Predict on Test Set
predictions <- knn.reg(train = train, test = test, y = y, k = 60)$pred

# Save Predictions
output <- data.frame(id = ids, Price = predictions)
write_csv(output, "/Users/lukeepp/Documents/KaggleBackPack/knn_preds_n_5.csv")
