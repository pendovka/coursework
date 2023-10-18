rm(list = ls())  # remove the existing environment

## You should set the working directory to the folder of hw3_starter by
## uncommenting the following and replacing YourDirectory by what you have
## in your local computer / laptop

setwd("/Users/pendovka/Downloads/hw4_starter")

## Load utils.R and discriminant_analysis.R

source("utils.R")
source("discriminant_analysis.R")

## Load the training and test data
train <- Load_data("./data/digits_train.txt")
test <- Load_data("./data/digits_test.txt")

x_train <- train$x
y_train <- train$y

x_test <- test$x
y_test <- test$y

#####################################################################
#                           Part a.                                 #
# TODO:  estimate the priors, conditional means and conditional     #
#        covariance matrices under LDA,                             #
#        predict the labels of test data by using the fitted LDA    #
#        compute its misclassification error rate                   #
#####################################################################


# Estimate priors, conditional means, and covariance for LDA
priors <- Comp_priors(y_train)
means <- Comp_cond_means(x_train, y_train)
covs <- Comp_cond_covs(x_train, y_train, cov_equal = TRUE)

# Predict labels of test data
posteriors_lda <- Predict_posterior(x_test, priors, means, covs, TRUE)
pred_labels_lda <- Predict_labels(posteriors_lda)

# Compute misclassification error for LDA

error_rate_lda <- sum(pred_labels_lda != y_test) / length(y_test)
cat("Misclassification error for LDA:", error_rate_lda, "\n")


#####################################################################
#                       END OF YOUR CODE                            #
#####################################################################



#####################################################################
#                           Part b.                                 #
# TODO:  estimate the priors, conditional means and conditional     #
#        covariance matrices under QDA,                             #
#        predict the labels of test data by using the fitted LDA    #
#        compute its misclassification error rate                   #
#####################################################################


# Estimate priors, conditional means, and covariance for QDA
priors <- Comp_priors(y_train)
means <- Comp_cond_means(x_train, y_train)
covs <- Comp_cond_covs(x_train, y_train, cov_equal = FALSE)

# Predict labels of test data
posteriors_qda <- Predict_posterior(x_test, priors, means, covs, FALSE)
pred_labels_qda <- Predict_labels(posteriors_qda)

# Compute misclassification error for QDA
error_rate_qda <- sum(pred_labels_qda != y_test) / length(y_test)
cat("Misclassification error for QDA:", error_rate_qda, "\n")


#####################################################################
#                       END OF YOUR CODE                            #
#####################################################################



#####################################################################
#                           Part c.                                 #
# TODO:  fit LDA and QDA by using the R package                     #
#        report their test errors                                   #
#####################################################################


# LDA using MASS package
fit_lda <- lda(x_train, grouping = y_train)
pred_lda <- predict(fit_lda, x_test)
error_rate_lda_mass <- sum(pred_lda$class != y_test) / length(y_test)
cat("Misclassification error using MASS package (LDA):", error_rate_lda_mass, "\n")

# QDA using MASS package
fit_qda <- qda(x_train, grouping = y_train)
pred_qda <- predict(fit_qda, x_test)
error_rate_qda_mass <- sum(pred_qda$class != y_test) / length(y_test)
cat("Misclassification error using MASS package (QDA):", error_rate_qda_mass, "\n")


#####################################################################
#                       END OF YOUR CODE                            #
#####################################################################



