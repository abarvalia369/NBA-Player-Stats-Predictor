# Load necessary libraries
library(rpart)

# Load your dataset
data <- read.csv('all_seasons.csv')

# Handle NA values in draft_number by replacing them with the largest draft number or 165
data$draft_number <- as.numeric(as.character(data$draft_number))
max_draft_number <- max(max(data$draft_number, na.rm = TRUE), 165)
data$draft_number[is.na(data$draft_number)] <- max_draft_number

# Create the high_scorer and high_reb variable as a factor
data$high_scorer <- factor(ifelse(data$pts > 15, "Yes", "No"))
data$high_reb <- factor(ifelse(data$reb > 5, "Yes", "No"))

# Setting benchmark for numerical variable prediction with rpart using only draft_number
model_rpart_num_benchmark <- rpart(pts ~ ts_pct, data = data)
pred_rpart_num_benchmark <- predict(model_rpart_num_benchmark, data)
mse_rpart_num_benchmark <- mean((data$pts - pred_rpart_num_benchmark)^2)

cat('Benchmark MSE (numerical prediction):', mse_rpart_num_benchmark, '\n')


# Setting benchmark for categorical variable prediction with rpart using only draft_number
model_rpart_cat_benchmark <- rpart(high_reb ~ ts_pct, data = data, method = 'class')
pred_rpart_cat_benchmark <- predict(model_rpart_cat_benchmark, data, type = 'class')
accuracy_rpart_cat_benchmark <- mean(pred_rpart_cat_benchmark == data$high_reb)

cat('Benchmark accuracy (categorical prediction):', accuracy_rpart_cat_benchmark, '\n')


# Feature engineering and model improvement
# Let's add a squared term for 'gp' as a new feature
data$gp_squared <- data$gp^2

# Split the data into training and testing sets
set.seed(123) # Set a seed for reproducibility
train_rows <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_rows, ]
test_data <- data[-train_rows, ]

# Final model for numerical variable prediction using additional predictors
final_model_num <- rpart(pts ~ draft_number + ts_pct + high_scorer + net_rating + gp, data = train_data)
final_pred_num <- predict(final_model_num, test_data)
final_mse_num <- mean((test_data$pts - final_pred_num)^2)

# Final model for categorical variable prediction using additional predictors
final_model_cat <- rpart(high_reb ~ draft_number + ts_pct + net_rating + gp + oreb_pct + dreb_pct, data = train_data, method = 'class')
final_pred_cat <- predict(final_model_cat, test_data, type = 'class')
final_accuracy_cat <- mean(final_pred_cat == test_data$high_reb)

# Check if the final models meet the assignment requirements
# For the numerical variable prediction, we need to reduce the benchmark MSE by 50%
# For the categorical variable prediction, we need to increase the accuracy by 10% from the benchmark

# Print the results
cat('Benchmark MSE (numerical prediction):', mse_rpart_num_benchmark, '\n')
cat('Final MSE after feature engineering (numerical prediction):', final_mse_num, '\n')
if (final_mse_num < (0.5 * mse_rpart_num_benchmark)) {
  cat('Successfully reduced MSE by more than 50% from the benchmark.\n')
} else {
  cat('Did not reduce MSE by more than 50% from the benchmark.\n')
}

cat('Benchmark accuracy (categorical prediction):', accuracy_rpart_cat_benchmark, '\n')
cat('Final accuracy after feature engineering (categorical prediction):', final_accuracy_cat, '\n')
if (final_accuracy_cat > (1.1 * accuracy_rpart_cat_benchmark)) {
  cat('Successfully increased accuracy by more than 10% from the benchmark.\n')
} else {
  cat('Did not increase accuracy by more than 10% from the benchmark.\n')
}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
