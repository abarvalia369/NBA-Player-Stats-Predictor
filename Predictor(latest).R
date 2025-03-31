# Load necessary libraries
library(rpart)

# Load your dataset
data <- read.csv('all_seasons.csv')

# Handling NA values in draft_number
data$draft_number <- as.numeric(as.character(data$draft_number))
data$draft_number[is.na(data$draft_number)] <- max(data$draft_number, na.rm = TRUE) + 1

# Create the high_scorer variable as a factor
data$high_scorer <- factor(ifelse(data$pts > 15, "Yes", "No"))
data$high_reb <- factor(ifelse(data$reb > 5, "Yes", "No"))
#--------------------------------------------------------------------------------------------------------------------------------------Benchmarks
# Setting benchmark for numerical variable prediction with lm() using only ts_pct
lm_benchmark <- lm(pts ~ ts_pct, data = data)
mse_lm_benchmark <- mean((data$pts - predict(lm_benchmark, data))^2)
head(mse_lm_benchmark)
# Setting benchmark for numerical variable prediction with rpart() using only ts_pct(Measure of the player's shooting efficiency)
rpart_benchmark <- rpart(pts ~ ts_pct, data = data)
mse_rpart_benchmark <- mean((data$pts - predict(rpart_benchmark, data))^2)
head(mse_rpart_benchmark)
# Choose the lower MSE as the benchmark for numerical prediction
benchmark_mse_num <- min(mse_lm_benchmark, mse_rpart_benchmark)

# Setting benchmark for categorical variable prediction with rpart() using only oreb_pct(Percentage of available offensive rebounds)
rpart_cat_benchmark <- rpart(high_reb ~ oreb_pct, data = data, method = 'class')
pred_cat_benchmark <- predict(rpart_cat_benchmark, data, type = 'class')
accuracy_cat_benchmark <- mean(pred_cat_benchmark == data$high_reb)
head(accuracy_cat_benchmark)
#--------------------------------------------------------------------------------------------------------------------------------------
# Feature engineering for the final models
data$gp_squared <- data$gp^2

# Running 1-step cross-validation for lm() model for numerical prediction
#set.seed(123)
train_rows <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_rows, ]
test_data <- data[-train_rows, ]
lm_final <- lm(pts ~ ts_pct + gp_squared, data = train_data)
pred_lm_final <- predict(lm_final, test_data)
mse_lm_final <- mean((test_data$pts - pred_lm_final)^2)

# Running 1-step cross-validation for rpart() model for numerical prediction
#set.seed(123)
rpart_final <- rpart(pts ~ draft_number + ts_pct + high_scorer + net_rating + gp + gp_squared, data = train_data)
pred_rpart_final <- predict(rpart_final, test_data)
mse_rpart_final <- mean((test_data$pts - pred_rpart_final)^2)

# Choose the best model for final MSE
final_mse_num <- min(mse_lm_final, mse_rpart_final)

# Running 1-step cross-validation for rpart() model for categorical prediction
#set.seed(123)
rpart_cat_final <- rpart(high_reb ~ draft_number + ts_pct + net_rating + gp + oreb_pct + dreb_pct + gp_squared, data = train_data, method = 'class')
pred_cat_final <- predict(rpart_cat_final, test_data, type = 'class')
accuracy_cat_final <- mean(pred_cat_final == test_data$high_reb)

# Print results for numerical prediction
cat('Benchmark MSE (numerical prediction):', benchmark_mse_num, '\n')
cat('Final MSE after feature engineering (numerical prediction):', final_mse_num, '\n')
if (final_mse_num < (0.5 * benchmark_mse_num)) {
  cat('Successfully reduced MSE by more than 50% from the benchmark.\n')
} else {
  cat('Did not reduce MSE by more than 50% from the benchmark.\n')
}

# Print results for categorical prediction
cat('Benchmark accuracy (categorical prediction):', accuracy_cat_benchmark, '\n')
cat('Final accuracy after feature engineering (categorical prediction):', accuracy_cat_final, '\n')
if (accuracy_cat_final > (1.1 * accuracy_cat_benchmark)) {
  cat('Successfully increased accuracy by more than 10% from the benchmark.\n')
} else {
  cat('Did not increase accuracy by more than 10% from the benchmark.\n')
}

