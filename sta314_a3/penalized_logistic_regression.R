Evaluate <- function(true_label, pred_label) {
  #  Compute the 0-1 loss between two vectors
  # 
  #  @param true_label: A vector of true labels with length n
  #  @param pred_label: A vector of predicted labels with length n
  #  @return: fraction of points get misclassified
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
 {
    error <- sum(true_label != pred_label) / length(true_label)
    return(error)
  }
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  return(error)
}


Predict_logis <- function(data_feature, beta, beta0, type) {
  # Predict by the logistic classifier.
  # 
  # Note: n is the number of examples
  #       p is the number of features per example
  # 
  # @param data_feature: A matrix with dimension n x p, where each row corresponds to
  #   one data point.
  # @param beta: A vector of coefficients with length equal to p.
  # @param beta0: the intercept.
  # @param type: a string value within {"logit", "prob", "class"}.
  # @return: A vector with length equal to n, consisting of 
  #   predicted logits,         if type = "logit";
  #   predicted probabilities,  if type = "prob";
  #   predicted labels,         if type = "class". 
  
  n <- nrow(data_feature)
  pred_vec <- rep(0, n)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
    
  # Compute the logits
  logits <- data_feature %*% beta + beta0
  
  # If type is logit, return the logits directly
  if(type == "logit") {
    return(logits)
  }
  
  # Compute the probabilities using the logistic function
  probs <- 1 / (1 + exp(-logits))
  
  # If type is prob, return the probabilities directly
  if(type == "prob") {
    return(probs)
  }
  
  # If type is class, return the class labels based on a threshold of 0.5
  if(type == "class") {
    class_labels <- ifelse(probs > 0.5, 1, 0)
    return(class_labels)
  }
  
  # If none of the above types, return a warning or error
  stop("Invalid type specified. Please choose between 'logit', 'prob', and 'class'.")
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(pred_vec)
}    
  

Comp_gradient <- function(data_feature, data_label, beta, beta0, lbd) {

  # Compute and return the gradient of the penalized logistic regression 
  # 
  # Note: n is the number of examples
  #       p is the number of features per example
  # 
  # @param data_feature: A matrix with dimension n x p, where each row corresponds to
  #   one data point.
  # @param data_label: A vector of labels with length equal to n.
  # @param beta: A vector of coefficients with length equal to p.
  # @param beta0: the intercept.
  # @param lbd: the regularization parameter
  # 
  # @return: a (p+1) x 1 vector of gradients, the first coordinate is the gradient
  #   w.r.t. the intercept.
  
  n <- nrow(data_feature)
  p <- ncol(data_feature)
  grad <- numeric(1 + p)
  
  #####################################################################
  # TODO:                                                             #
  #####################################################################
  
  # Compute the predicted values using the logistic function
  logits <- data_feature %*% beta + beta0
  predictions <- 1 / (1 + exp(-logits))
  
  # Compute the gradient for beta0
  grad[1] <- sum(predictions - data_label)
  
  # Compute the gradient for beta, including the regularization term
  for (j in 1:p) {
    grad[j+1] <- sum((predictions - data_label) * data_feature[, j]) + lbd * beta[j]
  }
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(grad)
}
  

Comp_loss <- function(data_feature, data_label, beta, beta0, lbd) {
  # Compute and return the loss of the penalized logistic regression 
  # 
  # Note: n is the number of examples
  #       p is the number of features per example
  # 
  # @param data_feature: A matrix with dimension n x p, where each row corresponds to
  #   one data point.
  # @param data_label: A vector of labels with with length equal to n.
  # @param beta: A vector of coefficients with length equal to p.
  # @param beta0: the intercept.
  # @param lbd: the regularization parameter
  # 
  # @return: a value of the loss function
  
  #####################################################################
  # TODO:                                                             #
  #####################################################################
  
  n <- nrow(data_feature)
  
  # Compute the logistic regression losses
  logits <- data_feature %*% beta + beta0
  log_losses <- -data_label * logits + log(1 + exp(logits))
  
  # Average log loss
  avg_log_loss <- mean(log_losses)
  
  # Regularization loss
  reg_loss <- 0.5 * lbd * sum(beta^2)
  
  # Total loss
  loss <- avg_log_loss + reg_loss
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(loss)
}


Penalized_Logistic_Reg <- function(x_train, y_train, lbd, stepsize, max_iter) {
  # This is the main function to fit the Penalized Logistic Regression
  #
  # Note: n is the number of examples
  #       p is the number of features per example
  #
  # @param x_train: A matrix with dimension n x p, where each row corresponds to
  #   one training point.
  # @param y_train: A vector of labels with length equal to n.
  # @param lbd: the regularization parameter.
  # @param stepsize: the learning rate.
  # @param max_iter: a positive integer specifying the maximal number of 
  #   iterations.
  # 
  # @return: a list containing four components:
  #   loss: a vector of loss values at each iteration
  #   error: a vector of 0-1 errors at each iteration
  #   beta: the estimated p coefficient vectors
  #   beta0: the estimated intercept.
  
  p <- ncol(x_train)
  
  # Initialize parameters to 0
  beta_cur <- rep(0, p)
  beta0_cur <- 0
  
  # Create the vectors for recording values of loss and 0-1 error during 
  # the training procedure
  loss_vec <- rep(0, max_iter)
  error_vec <- rep(0, max_iter)
  
  #####################################################################
  # TODO:                                                             #
  # Modify this section to perform gradient descent and to compute    #
  # losses and 0-1 errors at each iterations.                         #
  #####################################################################
  
  for (i in 1:max_iter) {
    # Compute gradient
    grad <- Comp_gradient(x_train, y_train, beta_cur, beta0_cur, lbd)
    
    # Update beta and beta0 using the gradients and stepsize
    beta0_cur <- beta0_cur - stepsize * grad[1]
    beta_cur <- beta_cur - stepsize * grad[-1]
    
    # Compute and store the loss and error at the current iteration
    loss_vec[i] <- Comp_loss(x_train, y_train, beta_cur, beta0_cur, lbd)
    pred_labels <- Predict_logis(x_train, beta_cur, beta0_cur, "class")
    error_vec[i] <- Evaluate(y_train, pred_labels)
  }
  
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(list("loss" = loss_vec, "error" = error_vec,
              "beta" = beta_cur, "beta0" = beta0_cur))
}
