library(lightgbm)
library(magrittr)
library(data.table)
library(xgboost)
library(Matrix)
library(stringr)
library(readr)

feature_engineering <- function(df) {
  df <- df %>%
    
    # 1️⃣ Text Features
    dplyr::group_by(Brand) %>%
    dplyr::mutate(
      Brand.Popularity = dplyr::n(),  # Count occurrences of each Brand
      Brand.Vowel.Count = stringr::str_count(Brand, "[AEIOUaeiou]"),  # Count vowels in Brand
      Brand.Consonant.Count = nchar(Brand) - stringr::str_count(Brand, "[AEIOUaeiou ]")  # Count consonants
    ) %>%
    dplyr::ungroup() %>%
    
    # 2️⃣ Numeric Transformations
    dplyr::mutate(
      Log.Weight.Capacity = log1p(`Weight Capacity (kg)`),  # Log transform Weight Capacity
      Log.Compartments = log1p(Compartments)  # Log transform Compartments
    ) %>%
    
    # 3️⃣ Interaction Features
    dplyr::mutate(
      Avg.Compartment.Weight = `Weight Capacity (kg)` / pmax(Compartments, 1),  # Avoid division by zero
      Laptop.Protection.Level = as.integer(`Laptop Compartment` == "Yes") * as.integer(Waterproof == "Yes")
    ) %>%
    
    dplyr::group_by(Brand, Style) %>%
    dplyr::mutate(Brand.Style.Popularity = dplyr::n()) %>%  # Count occurrences of Brand-Style pairs
    dplyr::ungroup() %>%
    
    # 4️⃣ Binary Encoding
    dplyr::mutate(
      `Laptop Compartment` = as.integer(`Laptop Compartment` == "Yes"),
      Waterproof = as.integer(Waterproof == "Yes")
    )
  
  return(df)
}


test = data.table::fread('/Users/lukeepp/Documents/KaggleBackPack/test.csv')
train = data.table::fread('/Users/lukeepp/Documents/KaggleBackPack/train.csv')
train = feature_engineering(train)
test = feature_engineering(test)
# Define Response Variable
y = train$Price
train$Price = NULL  # Remove response column



# Convert to Matrix
train.matrix = as.matrix(train)
test.matrix = as.matrix(test)

# Create LightGBM Dataset
dtrain = lgb.Dataset(data = train.matrix, label = y)

# Define Parameters
param.grid = expand.grid(
  num_leaves = c(15, 31, 50),
  learning_rate = c(0.01, 0.05, 0.1),
  max_depth = c(3, 6, 9)
)

# Store Best Results
best.rmse = Inf
best.params = list()

# Loop Over Parameter Combinations
for(i in 1:nrow(param.grid)) {
  params = list(
    objective = "regression",
    metric = "rmse",
    num_leaves = param.grid$num_leaves[i],
    learning_rate = param.grid$learning_rate[i],
    max_depth = param.grid$max_depth[i]
  )
  
  # Perform Cross-Validation
  cv.results = lgb.cv(
    params = params,
    data = dtrain,
    nrounds = 500,
    nfold = 5,  # 5-Fold Cross Validation
    early_stopping_rounds = 10,
    verbose = -1
  )
  rmses = cv.results$record_evals$valid$rmse$eval
  mean(unlist(rmses))
  mean.rmse = mean(unlist(rmses))
  if (mean.rmse < best.rmse) {
    best.rmse = mean.rmse
    best.params = params
    print(paste("Best RMSE:", best.rmse))
  }
}

print(best.params)
print(paste("Best RMSE:", best.rmse))

# Train Final Model with Best Parameters
final.model = lgb.train(
  params = best.params,
  data = dtrain,
  nrounds = 500
)

# Make Predictions
predictions = predict(final.model, test.matrix)
output = data.table(id = test$id, Price = predictions)
data.table::fwrite(output, "/Users/lukeepp/Documents/KaggleBackPack/my_preds_lgbm.csv")
