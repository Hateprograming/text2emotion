# Test 1: Normal training returns a ranger model
test_that("train_rf_model returns a ranger model object", {
  # Create a simple sparse training matrix and labels
  set.seed(123)
  mat <- Matrix::Matrix(matrix(rbinom(20, 1, 0.5), nrow = 5), sparse = TRUE)
  labels <- factor(c("happy", "sad", "happy", "sad", "happy"))

  # Call the function
  model <- train_rf_model(mat, labels, ntree = 10, verbose = FALSE)

  # Check if the returned object is of class "ranger"
  expect_s3_class(model, "ranger")

  # Clean up cached RDS files
  unlink("train_df_cached.rds")
  unlink("trained_rf_ranger_model.rds")
})

# Test 2: Error if input is not dgCMatrix
test_that("train_rf_model errors when input is not dgCMatrix", {
  mat <- matrix(rbinom(20, 1, 0.5), nrow = 5)  # Normal dense matrix
  labels <- factor(c("happy", "sad", "happy", "sad", "happy"))

  # Should produce an error
  expect_error(train_rf_model(mat, labels), "train_matrix must be of class dgCMatrix")
})

# Test 3: Auto-trims when train_matrix and train_labels mismatch
test_that("train_rf_model auto-trims when rows mismatch labels", {
  # 6 rows but only 5 labels
  set.seed(123)
  mat <- Matrix::Matrix(matrix(rbinom(30, 1, 0.5), nrow = 6), sparse = TRUE)
  labels <- factor(c("happy", "sad", "happy", "sad", "happy"))

  # Should produce a warning but still train successfully
  expect_warning({
    model <- train_rf_model(mat, labels, ntree = 10, verbose = FALSE)
    expect_s3_class(model, "ranger")
  }, regexp = "mismatch")

  # Clean up cached RDS files
  unlink("train_df_cached.rds")
  unlink("trained_rf_ranger_model.rds")
})
