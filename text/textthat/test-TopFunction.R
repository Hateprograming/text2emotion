test_that("train_full_model runs successfully and returns expected structure", {
  # Skip test if required data files are not available
  skip_if_not(file.exists(system.file("extdata", "go_emotions_train_text_processed.csv", package = "text2emotion")))
  skip_if_not(file.exists(system.file("extdata", "go_emotions_test_text_processed.csv", package = "text2emotion")))
  skip_if_not(file.exists(system.file("extdata", "go_emotions_train_label.csv", package = "text2emotion")))
  skip_if_not(file.exists(system.file("extdata", "go_emotions_test_label.csv", package = "text2emotion")))
  skip_if_not(file.exists(system.file("extdata", "final_stopwords.rds", package = "text2emotion")))

  # Run training (suppress verbose printing)
  result <- train_full_model(
    mtry_grid = c(2),    # small grid to make test fast
    ntree_grid = c(10)  # small grid to make test fast
  )

  # Check return type
  expect_type(result, "list")

  # Check list contents
  expect_true(all(c("tfidf_model", "vectorizer", "rf_model", "test_accuracy", "macro_f1", "confusion") %in% names(result)))

  # Clean up any generated model files
  unlink("train_df_cached.rds")
  unlink("trained_tfidf_model.rds")
  unlink("trained_vectorizer.rds")
  unlink("trained_rf_ranger_model.rds")
})
