#' Tune Random Forest Model Hyperparameters
#'
#' This function performs hyperparameter tuning for a Random Forest model using grid search.
#' It searches over the grid of `mtry` (number of variables to consider at each split) and `ntree`
#' (number of trees in the forest) to find the best model based on training accuracy.
#'
#' @param train_matrix A sparse matrix (class `dgCMatrix`) representing the training feature data.
#' @param train_labels A factor vector representing the training labels.
#' @param mtry_grid A vector of values to search for the `mtry` parameter (number of variables to consider at each split). Default is `c(5, 10, 20)`.
#' @param ntree_grid A vector of values to search for the `ntree` parameter (number of trees in the forest). Default is `c(100, 200, 300)`.
#' @param seed A seed value for reproducibility. Default is `123`.
#' @param verbose A logical indicating whether to print progress information during the grid search. Default is `TRUE`.
#'
#' @return A list containing the best hyperparameters (`mtry`, `ntree`, and `accuracy`):
#' \itemize{
#'   \item `mtry`: The best number of variables to consider at each split.
#'   \item `ntree`: The best number of trees in the forest.
#'   \item `accuracy`: The accuracy achieved by the model with the best hyperparameters.
#' }
#'
#' @details
#' The function trains multiple Random Forest models using different combinations of `mtry` and `ntree`
#' values, and evaluates their performance based on training accuracy. The hyperparameters that give
#' the highest accuracy are returned as the best parameters. The process uses the `ranger` package for
#' training the Random Forest model.
#'
#' @import ranger
#' @import parallel
#' @importFrom stats predict
#' @export
tune_rf_model <- function(train_matrix, train_labels,
                          mtry_grid = c(5, 10, 20),
                          ntree_grid = c(100, 200, 300),
                          seed = 123, verbose = TRUE) {

  # Check if the train_matrix is of correct type
  if (!inherits(train_matrix, "dgCMatrix")) {
    stop("train_matrix must be of class dgCMatrix (sparse matrix).")
  }

  # Ensure train_labels is a factor
  if (!is.factor(train_labels)) train_labels <- factor(train_labels)

  # Convert train_matrix to data frame
  train_df <- as.data.frame(as.matrix(train_matrix), check.names = FALSE)
  train_df$label <- train_labels

  # Initialize best accuracy and parameter list
  best_accuracy <- 0
  best_params <- list()

  # If verbose is TRUE, print the progress
  if (verbose) cat("Starting grid search...\n")

  # Grid search for optimal mtry and ntree values
  for (mtry in mtry_grid) {
    for (ntree in ntree_grid) {
      if (verbose) cat(sprintf("train model: mtry = %d, ntree = %d\n", mtry, ntree))

      # Set seed for reproducibility
      set.seed(seed)

      # Train the Random Forest model using the ranger package
      model <- ranger(
        dependent.variable.name = "label",
        data = train_df,
        mtry = mtry,
        num.trees = ntree,
        classification = TRUE,
        probability = FALSE,
        seed = seed,
        num.threads = parallel::detectCores()
      )

      # Calculate training accuracy
      preds <- predict(model, data = train_df)$predictions
      acc <- mean(preds == train_labels)

      # If verbose, print the accuracy
      if (verbose) cat(sprintf("model accuracy: %.2f%%\n", acc * 100))

      # Update best accuracy and parameters if current model is better
      if (acc > best_accuracy) {
        best_accuracy <- acc
        best_params <- list(mtry = mtry, ntree = ntree, accuracy = acc)
      }
    }
  }

  if (verbose) {
    cat("Tuning complete! Best hyperparameters:\n")
    print(best_params)
  }

  return(best_params)
}
