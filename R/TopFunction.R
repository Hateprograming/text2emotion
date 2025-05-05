#' Train a full model pipeline including text preprocessing, TF-IDF vectorization,
#' random forest tuning, and training.
#'
#' @param custom_slang A named list for custom slang replacements (optional).
#' @param max_features Maximum number of features for TF-IDF vectorizer (default 10000).
#' @param min_df Minimum document frequency for TF-IDF (default 2).
#' @param max_df Maximum document frequency for TF-IDF (default 0.8).
#' @param mtry_grid Grid of values for `mtry` parameter to tune in random forest (default: c(5, 10, 20)).
#' @param ntree_grid Grid of values for `ntree` parameter to tune in random forest (default: c(100, 200, 300)).
#' @param stopwords_file Path to the stopwords RDS file (default: "final_stopwords.rds").
#' @param vectorizer_file Path to save the trained vectorizer (default: "trained_vectorizer.rds").
#' @param tfidf_model_file Path to save the trained TF-IDF model (default: "trained_tfidf_model.rds").
#' @param rf_model_file Path to save the trained random forest model (default: "trained_rf_ranger_model.rds").
#' @param train_df_cache_path Path to cache the training data frame (default: "train_df_cached.rds").
#'
#' @return A list containing the trained TF-IDF model, vectorizer, random forest model, and test accuracy.
#' @import ranger
#' @name train_full_model
#' @export

library(ranger)
train_full_model <- function(custom_slang = NULL, max_features = 10000,
                             min_df = 2, max_df = 0.8,
                             mtry_grid = c(5, 10, 20), ntree_grid = c(100, 200, 300),
                             stopwords_file = "final_stopwords.rds",
                             vectorizer_file = "trained_vectorizer.rds",
                             tfidf_model_file = "trained_tfidf_model.rds",
                             rf_model_file = "trained_rf_ranger_model.rds",
                             train_df_cache_path = "train_df_cached.rds") {


  train_texts <- readLines(system.file("extdata", "go_emotions_train_text_processed.csv", package = "text2emotion"), warn = FALSE)
  test_texts <- readLines(system.file("extdata", "go_emotions_test_text_processed.csv", package = "text2emotion"), warn = FALSE)
  train_labels <- readLines(system.file("extdata", "go_emotions_train_label.csv", package = "text2emotion"), warn = FALSE)
  test_labels <- readLines(system.file("extdata", "go_emotions_test_label.csv", package = "text2emotion"), warn = FALSE)

  # Data Preprocessing
  preprocessed_texts <- preprocess_text(train_texts, custom_slang = custom_slang)
  preprocessed_test_texts <- preprocess_text(test_texts, custom_slang = custom_slang)

  # Train TF-IDF Model
  tfidf_result <- train_tfidf_model(preprocessed_texts, max_features, min_df, max_df)
  saveRDS(tfidf_result$tfidf_model, tfidf_model_file)
  saveRDS(tfidf_result$vectorizer, vectorizer_file)

  # Tune Random Forest Model Hyperparameters
  best_params <- tune_rf_model(
    train_matrix = tfidf_result$tfidf_matrix,
    train_labels = train_labels,
    mtry_grid = mtry_grid,
    ntree_grid = ntree_grid,
    seed = 123,
    verbose = TRUE
  )

  train_df_cache_path <- file.path(getwd(), "train_df_cached.rds")
  rf_model <- train_rf_model(
    train_matrix = tfidf_result$tfidf_matrix,
    train_labels = train_labels,
    ntree = best_params$ntree,
    mtry = best_params$mtry,
    seed = 123,
    verbose = TRUE,
    train_df_cache_path = train_df_cache_path
  )

  eval_result <- evaluate_rf_model(
    rf_model = rf_model,
    test_texts = preprocessed_test_texts,
    test_labels = test_labels,
    tfidf_model = tfidf_result$tfidf_model,
    vectorizer = tfidf_result$vectorizer,
    stopwords = readRDS(system.file("extdata", "final_stopwords.rds", package="text2emotion")),
    verbose = TRUE
  )


  # Return models and evaluation results
  return(list(
    tfidf_model = tfidf_result$tfidf_model,
    vectorizer = tfidf_result$vectorizer,
    rf_model = rf_model,
    test_accuracy = eval_result$test_accuracy,
    macro_f1 = eval_result$macro_f1,
    confusion = eval_result$confusion
  ))
}
