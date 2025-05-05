test_that("train_tfidf_model returns correct structure", {
  sample_text <- c("I'm feeling great!", "It's a sunny day and I'm so happy.")

  result <- train_tfidf_model(sample_text)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the list contains expected elements
  expect_true(all(c("tfidf_model", "vectorizer", "tfidf_matrix") %in% names(result)))

  # Check if tfidf_matrix is a sparse matrix
  expect_true(inherits(result$tfidf_matrix, "dgCMatrix"))
})

test_that("train_tfidf_model errors on non-character input", {
  # Expect an error when input is not a character vector
  expect_error(train_tfidf_model(12345), "Input must be a character vector")
})

test_that("train_tfidf_model creates a non-empty tfidf matrix", {
  sample_text <- c("Happy moments bring happy thoughts.", "Sad stories make people emotional.")

  result <- train_tfidf_model(sample_text)

  # Check that the tfidf_matrix has rows and columns (i.e., is not empty)
  expect_gt(nrow(result$tfidf_matrix), 0)
  expect_gt(ncol(result$tfidf_matrix), 0)
})
