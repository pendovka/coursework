library(MASS)
library(mvtnorm)

Comp_priors <- function(train_labels) {
  #' Compute the priors of each class label 
  #' 
  #' @param train_labels a vector of labels with length equal to n
  #' @return a probability vector of length K = 10
  
  
  K <- 10
  pi_vec <- rep(0, K)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  for (k in 0:(K-1)) {
    pi_vec[k + 1] <- sum(train_labels == k) / length(train_labels)
  }
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(pi_vec)
}

Comp_cond_means <- function(train_data, train_labels) {
  #' Compute the conditional means of each class 
  #' 
  #' @param train_data a n by p matrix containing p features of n training points
  #' @param train_labels a vector of labels with length equal to n
  #' 
  #' @return a p by 10 matrix, each column represents the conditional mean given
  #'   each class.
  
  K <- 10
  p <- ncol(train_data)
  mean_mat <- matrix(0, p, K)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  for (k in 0:(K-1)) {
    mean_mat[, k + 1] <- colMeans(train_data[train_labels == k, ])
  }
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(mean_mat)
}

Comp_cond_covs <- function(train_data, train_labels, cov_equal = FALSE) {
  #' Compute the conditional covariance matrix of each class
  #' 
  #' @param train_data a n by p matrix containing p features of n training points
  #' @param train_labels a vector of labels with length equal to n
  #' @param cov_equal TRUE if all conditional covariance matrices are equal, 
  #'   otherwise, FALSE 
  #' 
  #' @return 
  #'  if \code{cov_equal} is FALSE, return an array with dimension (p, p, K),
  #'    containing p by p covariance matrices of each class;
  #'  else, return a p by p covariance matrix. 
  
  K <- 10
  p <- ncol(train_data)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  cov_arr <- array(0, dim = c(p, p, K))
  for (k in 0:(K-1)) {
    cov_arr[, , k + 1] <- cov(train_data[train_labels == k, ])
  }
  
  if (cov_equal) {
    cov_mat <- matrix(0, p, p)
    for (k in 0:(K-1)) {
      cov_mat <- cov_mat + cov_arr[, , k + 1]
    }
    cov_mat <- cov_mat / K
    return(cov_mat)
  }
  
  return(cov_arr)
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
}

Predict_posterior <- function(test_data, priors, means, covs, cov_equal) {
  
  #' Predict the posterior probabilities of each class 
  #'
  #' @param test_data a n_test by p feature matrix 
  #' @param priors a vector of prior probabilities with length equal to K
  #' @param means a p by K matrix containing conditional means given each class
  #' @param covs covariance matrices of each class, depending on \code{cov_equal}
  #' @param cov_equal TRUE if all conditional covariance matrices are equal; 
  #'   otherwise FALSE.
  #'   
  #' @return a n_test by K matrix: each row contains the posterior probabilities 
  #'   of each class.
  
  n_test <- nrow(test_data)
  K <- length(priors)
  posteriors <- matrix(0, n_test, K)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  for (k in 1:K) {
    if (cov_equal) {
      likelihood <- dmvnorm(test_data, mean = means[,k], sigma = covs)
    } else {
      likelihood <- dmvnorm(test_data, mean = means[,k], sigma = covs[,,k])
    }
    posteriors[,k] <- likelihood * priors[k]
  }
  
  # Normalize the posteriors
  row_sums <- rowSums(posteriors)
  posteriors <- sweep(posteriors, 1, row_sums, FUN = "/")
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(posteriors)
}

Predict_labels <- function(posteriors) {
  
  #' Predict labels based on the posterior probabilities over K classes
  #' 
  #' @param posteriors A n by K posterior probabilities
  #' 
  #' @return A vector of predicted labels with length equal to n
  
  n_test <- nrow(posteriors)
  pred_labels <- rep(NA, n_test)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  pred_labels <- apply(posteriors, 1, which.max) - 1
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(pred_labels)
}


