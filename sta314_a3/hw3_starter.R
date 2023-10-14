rm(list = ls())

setwd("/Users/pendovka/Downloads/hw3_starter")

## Load utils.R and penalized_logistic_regression.R

source("utils.R")
source("penalized_logistic_regression.R")

## load data sets

train <- Load_data("./data/train.csv")
valid <- Load_data("./data/valid.csv")
test <- Load_data("./data/test.csv")

x_train <- train$x
y_train <- train$y

x_valid <- valid$x
y_valid <- valid$y

x_test <- test$x
y_test <- test$y

#####################################################################
#                           Part a.                                 #
# TODO: Find the best choice of the hyperparameters:                #
#     - stepsize (i.e. the learning rate)                           #
#     - max_iter (the maximal number of iterations)                 #
#   The regularization parameter, lbd, should be set to 0           #
#   Draw plot of training losses and training 0-1 errors            #
#####################################################################

lbd = 0

# Hyperparameters tuning
stepsizes <- seq(0.001, 0.1, by=0.01)
max_iters <- seq(10, 100, by=10)


results <- data.frame(Stepsize=numeric(), MaxIter=numeric(), Loss=numeric(), Error=numeric())

for (stepsize in stepsizes) {
  for (max_iter in max_iters) {
    result <- Penalized_Logistic_Reg(x_train, y_train, lbd, stepsize, max_iter)
    new_row <- data.frame(Stepsize=stepsize, MaxIter=max_iter, Loss=tail(result$loss, 1), Error=tail(result$error, 1))
    results <- rbind(results, new_row)
  }
}


colnames(results) <- c("Stepsize", "MaxIter", "Loss", "Error")

# Visualization with ggplot2
library(ggplot2)
ggplot(results, aes(x=Stepsize, y=MaxIter)) +
  geom_tile(aes(fill=Loss), color="white") +
  scale_fill_gradient(low="white", high="red") +
  theme_minimal() +
  labs(title="Training Losses")

ggplot(results, aes(x=Stepsize, y=MaxIter)) +
  geom_tile(aes(fill=Error), color="white") +
  scale_fill_gradient(low="white", high="blue") +
  theme_minimal() +
  labs(title="Training 0-1 Errors")

#####################################################################
#                       END OF YOUR CODE                            #
#####################################################################




#####################################################################
#                           Part b.                                 #
# TODO: Identify the best stepsize and max_iter for each lambda     #
#       from the given grid. Draw the plots of training and         #
#       validation 0-1 errors versus different values of lambda     #
#####################################################################

library(ggplot2)

# Lambda grid
lbd_grid <- c(0, 0.01, 0.05, 0.1, 0.5, 1)

# Define possible stepsizes and max_iters for search
stepsizes <- c(0.001, 0.01, 0.1)  # Just example values, add your possible stepsizes here
max_iters <- c(100, 500, 1000)    # Add possible max_iters

# Placeholder for errors and optimal hyperparameters
training_errors <- numeric(length(lbd_grid))
validation_errors <- numeric(length(lbd_grid))
best_hyperparameters <- list()

# Iterate over the lambda grid
for (i in 1:length(lbd_grid)) {
  
  lbd <- lbd_grid[i]
  
  best_error <- Inf
  
  # Hyperparameter search
  for (step in stepsizes) {
    for (iter in max_iters) {
      
      # Train the model
      model <- Penalized_Logistic_Reg(x_train, y_train, lbd, step, iter)
      
      # Predictions and error calculation for validation set
      valid_predictions <- predict_custom(model, x_valid)
      current_error <- mean(valid_predictions != y_valid)
      
      # Check and update best error and hyperparameters
      if (current_error < best_error) {
        best_error <- current_error
        best_hyperparameters[[i]] <- list(stepsize=step, max_iter=iter)
      }
    }
  }
  
  # Re-train with best hyperparameters to get training error
  
  best_step <- best_hyperparameters[[i]]$stepsize
  best_iter <- best_hyperparameters[[i]]$max_iter
  model <- Penalized_Logistic_Reg(x_train, y_train, lbd, best_step, best_iter)
  train_predictions <- predict_custom(model, x_train)
  training_errors[i] <- mean(train_predictions != y_train)
  
  # Assign the best validation error
  validation_errors[i] <- best_error
}

# Plotting
df <- data.frame(
  Lambda = rep(lbd_grid, 2),
  Error = c(training_errors, validation_errors),
  Type = factor(c(rep("Training", length(lbd_grid)), rep("Validation", length(lbd_grid))))
)

ggplot(df, aes(x=Lambda, y=Error, color=Type)) +
  geom_line() +
  labs(title="Error vs. Lambda for Training and Validation Sets", x="Lambda", y="Error") +
  theme_minimal()


#####################################################################
#                       END OF YOUR CODE                            #
#####################################################################







#####################################################################
#                           Part c.                                 #
# TODO: using the best stepsize,  max_iter and lbd you found, fit   # 
#  the penalized logistic regression and compute its test 0-1 error #
#####################################################################

# Replace these values with the best hyperparameters you found from parts a and b.

best_lambda_index <- which.min(validation_errors)
lbd_best <- lbd_grid[best_lambda_index]
best_step <- best_hyperparameters[[best_lambda_index]]$stepsize
best_iter <- best_hyperparameters[[best_lambda_index]]$max_iter
# Fit the penalized logistic regression on training data using best hyperparameters.
model <- Penalized_Logistic_Reg(x_train, y_train, lbd_best, best_step, best_iter)

# Use the trained model to make predictions on the test dataset.
test_predictions <- Predict_logis(x_test, model$beta, model$beta0, "class")

# Compute the 0-1 error on the test dataset.
test_error <- Evaluate(y_test, test_predictions)
print(paste("Test 0-1 Error:", test_error))

#####################################################################
#                       END OF YOUR CODE                            #
#####################################################################




