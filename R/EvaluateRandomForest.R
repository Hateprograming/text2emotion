#' Evaluate a Random Forest Model on Test Data
#'
#' @param rf_model A trained `ranger` model object.
#' @param test_texts A vector of raw test texts.
#' @param test_labels A factor vector of true labels.
#' @param tfidf_model The TF-IDF transformer used for training.
#' @param vectorizer The vectorizer used to build DTM.
#' @param stopwords A character vector of stopwords.
#' @param verbose Whether to print progress. Default TRUE.
#'
#' @return A list with test accuracy, test predictions, and aligned test data.
#'
#' @import caret
#' @importFrom text2vec word_tokenizer itoken create_dtm
#' @importFrom stats predict
#' @export
evaluate_rf_model <- function(rf_model, test_texts, test_labels,
                              tfidf_model, vectorizer,
                              stopwords, verbose = TRUE) {

  if (!is.factor(test_labels)) test_labels <- factor(test_labels)

  if (verbose) cat("Processing and vectorizing test data...\n")
  test_tokens <- text2vec::word_tokenizer(tolower(test_texts))
  test_tokens <- lapply(test_tokens, function(x) x[!x %in% stopwords])
  test_tokens <- lapply(test_tokens, handle_negation)
  test_tokens <- Filter(function(x) length(x) > 0, test_tokens)

  it_test    <- text2vec::itoken(test_tokens, progressbar = FALSE)
  dtm_test   <- text2vec::create_dtm(it_test, vectorizer)
  tfidf_test <- tfidf_model$transform(dtm_test)
  tfidf_test <- as.matrix(tfidf_test)

  if (nrow(tfidf_test) != length(test_labels)) {
    warning("Test data and labels mismatch. Trimming to min length.")
    min_len <- min(nrow(tfidf_test), length(test_labels))
    tfidf_test <- tfidf_test[1:min_len, ]
    test_labels <- test_labels[1:min_len]
  }

  test_df <- as.data.frame(tfidf_test, check.names = FALSE)

  # Align test features with training
  needed_by_model <- rf_model$forest$independent.variable.names
  missing_cols <- setdiff(needed_by_model, colnames(test_df))
  for (col in missing_cols) {
    test_df[[col]] <- 0
  }
  test_df <- test_df[, needed_by_model, drop = FALSE]

  if (verbose) cat("Predicting and evaluating...\n")
  pred_test <- predict(rf_model, data = test_df)$predictions
  # Align levels before comparison
  pred_test <- factor(pred_test, levels = levels(test_labels))
  test_acc <- mean(pred_test == test_labels)

  if (verbose) cat(sprintf("Test Accuracy: %.2f%%\n", test_acc * 100))

  # Confusion matrix and metrics
  cm <- caret::confusionMatrix(pred_test, test_labels)

  # Handle both single-class and multi-class output
  if (is.null(dim(cm$byClass))) {
    # Only one class — byClass is a named vector
    precision <- cm$byClass["Precision"]
    recall    <- cm$byClass["Recall"]
    f1        <- cm$byClass["F1"]
  } else {
    # Multiple classes — byClass is a matrix
    precision <- cm$byClass[, "Precision"]
    recall    <- cm$byClass[, "Recall"]
    f1        <- cm$byClass[, "F1"]
  }

  avg_f1 <- mean(f1, na.rm = TRUE)  # macro-F1

  if (verbose) {
    print(cm$table)
    cat(sprintf("Macro F1-score: %.3f\n", avg_f1))
  }

  list(
    test_accuracy = test_acc,
    macro_f1      = avg_f1,
    confusion     = cm$table,
    precision     = precision,
    recall        = recall,
    f1            = f1,
    test_pred     = pred_test
  )
}


