test_that("tune_rf_model returns a list with correct components", {
  # Create a small dummy sparse matrix for testing
  library(Matrix)
  set.seed(123)
  dummy_matrix <- Matrix(matrix(rnorm(100), nrow = 10, ncol = 10), sparse = TRUE)
  dummy_labels <- factor(sample(c("positive", "negative"), 10, replace = TRUE))

  result <- tune_rf_model(dummy_matrix, dummy_labels, mtry_grid = c(2, 3), ntree_grid = c(10, 20), verbose = FALSE)

  # Check if the output is a list
  expect_type(result, "list")

  # Check if the list contains mtry, ntree, and accuracy
  expect_true(all(c("mtry", "ntree", "accuracy") %in% names(result)))

  # Check if accuracy is a numeric value between 0 and 1
  expect_true(is.numeric(result$accuracy))
  expect_gte(result$accuracy, 0)
  expect_lte(result$accuracy, 1)
})

test_that("tune_rf_model throws an error with incorrect train_matrix type", {
  dummy_matrix_wrong <- matrix(rnorm(100), nrow = 10, ncol = 10)  # Not sparse
  dummy_labels <- factor(sample(c("positive", "negative"), 10, replace = TRUE))

  expect_error(tune_rf_model(dummy_matrix_wrong, dummy_labels, verbose = FALSE),
               "train_matrix must be of class dgCMatrix")
})

test_that("tune_rf_model automatically coerces non-factor labels", {
  library(Matrix)
  dummy_matrix <- Matrix(matrix(rnorm(100), nrow = 10, ncol = 10), sparse = TRUE)
  dummy_labels <- sample(c("positive", "negative"), 10, replace = TRUE)  # character not factor

  result <- tune_rf_model(dummy_matrix, dummy_labels, mtry_grid = c(2), ntree_grid = c(10), verbose = FALSE)

  # Should still work and return expected output
  expect_type(result, "list")
  expect_true(all(c("mtry", "ntree", "accuracy") %in% names(result)))
})
