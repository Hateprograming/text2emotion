# Unified path check
model_path <- system.file("extdata", "trained_rf_ranger_model.rds", package = "text2emotion")
tfidf_path <- system.file("extdata", "trained_tfidf_model.rds", package = "text2emotion")
vectorizer_path <- system.file("extdata", "trained_vectorizer.rds", package = "text2emotion")

skip_if(model_path == "", "Trained model file missing")
skip_if(tfidf_path == "", "TF-IDF model file missing")
skip_if(vectorizer_path == "", "Vectorizer file missing")

test_that("predict_emotion_with_emoji returns emotion label", {
  result <- predict_emotion_with_emoji("I am feeling great", output_type = "emotion")

  # Expect a character output
  expect_type(result, "character")

  # Should match one of known emotion labels
  expect_true(result %in% c("positive", "angry", "sad", "fear", "neutral"))
})

test_that("predict_emotion_with_emoji returns emoji output", {
  result <- predict_emotion_with_emoji("I am angry", output_type = "emoji")

  expect_type(result, "character")

  expect_true(result %in% c("😊", "😡", "😢", "😨", "😐"))
})

test_that("predict_emotion_with_emoji returns text + emoji output", {
  input_text <- "I feel a bit scared"
  result <- predict_emotion_with_emoji(input_text, output_type = "textemoji")

  expect_true(grepl(input_text, result))
  expect_true(grepl("[😊😡😢😨😐]", result))
})

test_that("predict_emotion_with_emoji errors on invalid output_type", {
  expect_error(
    predict_emotion_with_emoji("Feeling neutral", output_type = "wrongtype"),
    "Invalid output_type"
  )
})
