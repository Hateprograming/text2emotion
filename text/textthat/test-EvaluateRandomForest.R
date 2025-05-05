test_that("evaluate_rf_model returns correct output structure", {
  # Create a simple mock dataset
  set.seed(123)
  train_mat <- Matrix::Matrix(matrix(rbinom(40, 1, 0.5), nrow = 8), sparse = TRUE)
  train_labels <- factor(c("happy", "sad", "happy", "sad", "happy", "sad", "happy", "sad"))

  # Train a simple RF model first
  rf_model <- train_rf_model(train_mat, train_labels, ntree = 10, verbose = FALSE)

  # Create simple test data (texts and labels)
  test_texts <- c("I am happy today", "I am so sad")
  test_labels <- factor(c("happy", "sad"))

  # Fake TF-IDF and vectorizer (simulate a minimal working example)
  tokens <- text2vec::word_tokenizer(tolower(test_texts))
  it <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(it)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  dtm <- text2vec::create_dtm(it, vectorizer)
  tfidf_model <- text2vec::TfIdf$new()
  tfidf_model$fit_transform(dtm)

  stopwords <- c("i", "am", "so", "today")  # Basic stopword list

  # Call the evaluation function
  result <- evaluate_rf_model(
    rf_model = rf_model,
    test_texts = test_texts,
    test_labels = test_labels,
    tfidf_model = tfidf_model,
    vectorizer = vectorizer,
    stopwords = stopwords,
    verbose = FALSE
  )

  # Check if the output is a list
  expect_type(result, "list")

  # Check if the list has expected elements
  expect_true(all(c("test_accuracy", "macro_f1", "confusion", "precision", "recall", "f1", "test_pred") %in% names(result)))

  # Check if test_accuracy is numeric
  expect_type(result$test_accuracy, "double")

  # Check if macro_f1 is numeric
  expect_type(result$macro_f1, "double")

  # Check if confusion is a table (matrix or array)
  expect_true(is.matrix(result$confusion) || is.table(result$confusion))
})
