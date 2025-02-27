
library(magrittr)
library(data.table)
library(xgboost)
library(Matrix)

encode_categorical = function(df, ref_levels = NULL) {
  cat.cols = names(df)[sapply(df, is.character)]  # Identify character columns
  
  if (is.null(ref_levels)) {
    ref_levels = lapply(df[, ..cat.cols], function(col) levels(factor(col)))  # Save levels from training set
  }
  
  for (col in cat.cols) {
    df[[col]] = as.integer(factor(df[[col]], levels = ref_levels[[col]]))  # Convert to numeric based on training levels
  }
  
  return(list(data = df, levels = ref_levels))
}

# Load Data
test = data.table::fread('/Users/lukeepp/Documents/KaggleBackPack/test.csv')
train = data.table::fread('/Users/lukeepp/Documents/KaggleBackPack/train.csv')

colnames(train)
# Define the response variable
y = train$Price
train$Price = NULL  # Remove response column from training data

train_encoded = encode_categorical(train)
train = train_encoded$data
ref_levels = train_encoded$levels  # Save reference levels for test set

# Encode Test Data Using the Same Levels
test_encoded = encode_categorical(test, ref_levels)
test = test_encoded$data

# Convert to Matrix
train.matrix = as.matrix(train)
test.matrix = as.matrix(test)

# Convert to DMatrix for XGBoost
dtrain = xgb.DMatrix(data = train.matrix, label = y)
dtest = xgb.DMatrix(data = test.matrix)
# Set Cross-Validation Parameters
param.grid = expand.grid(
  eta = c(0.1, 0.3),
  max_depth = c(3, 6, 9),
  min_child_weight = c(3),
  subsample = c(0.8),
  colsample_bytree = c(0.8)
)

# Perform k-Fold Cross Validation (5-fold)
best.rmse = Inf
best.params = list()

for(i in 1:nrow(param.grid)) {
  params = list(
    objective = "reg:squarederror",
    eta = param.grid$eta[i],
    max_depth = param.grid$max_depth[i],
    min_child_weight = param.grid$min_child_weight[i],
    subsample = param.grid$subsample[i],
    colsample_bytree = param.grid$colsample_bytree[i]
  )
  
  cv.results = xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 200,
    nfold = 5,
    metrics = "rmse",
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  mean.rmse = min(cv.results$evaluation_log$test_rmse_mean)
  print(mean.rmse)
  if(mean.rmse < best.rmse) {
    best.rmse = mean.rmse
    best.params = params
    best.nrounds = cv.results$best_iteration
  }
}

# Train the Final Model with Best Parameters
final.model = xgb.train(
  params = best.params,
  data = dtrain,
  nrounds = best.nrounds
)

# Save Model
xgb.save(final.model, "/Users/lukeepp/Documents/KaggleBackPack/best_xgb_model.model")

# Make Predictions on Test Data
test.matrix = as.matrix(test)
dtest = xgb.DMatrix(data = test.matrix)
predictions = predict(final.model, dtest)

# Save Predictions
output = data.table(id = test$id, Price = predictions)
fwrite(output, "/Users/lukeepp/Documents/KaggleBackPack/my_preds_xgb.csv")


library(lightgbm)

lgb.train <- lgb.Dataset(data = train.matrix, label = y)
params <- list(objective = "regression", metric = "rmse", num_leaves = 31, learning_rate = 0.05)

lgb.model <- lgb.train(params, lgb.train, nrounds = 500)

predictions <- predict(lgb.model, test.matrix)
