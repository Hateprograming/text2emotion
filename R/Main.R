#' Predict Emotion with Emoji Representation
#'
#' This function takes input text, preprocesses it, extracts TF-IDF features using a pre-trained model,
#' predicts the emotion using a trained classifier, and returns the result with optional emoji representation.
#'
#' @param text Character string containing the text to analyze.
#' @param output_type Type of output to return. Must be one of:
#'   \itemize{
#'     \item "emotion" - returns only the predicted emotion label
#'     \item "emoji" - returns only the corresponding emoji
#'     \item "textemoji" - returns original text appended with predicted emoji (default)
#'   }
#'
#' @return Depending on output_type:
#'   \itemize{
#'     \item For "emotion": character string of predicted emotion
#'     \item For "emoji": character string of corresponding emoji
#'     \item For "textemoji": original text with appended emoji
#'   }
#'   The function also prints the result to console.
#'
#' @examples
#' \dontrun{
#' predict_emotion_with_emoji("I'm so happy today!")
#' predict_emotion_with_emoji("This makes me angry", "emoji")
#' predict_emotion_with_emoji("I feel scared", "emotion")
#' }
#'
#' @importFrom stats predict
#' @export
predict_emotion_with_emoji <- function(text, output_type = "textemoji") {
  # 1. Load pre-trained models
  # Note: These files should be in the working directory or provide full paths
  trained_model <- readRDS(system.file("extdata", "trained_rf_ranger_model.rds", package = "text2emotion"))
  tfidf_model   <- readRDS(system.file("extdata", "trained_tfidf_model.rds", package = "text2emotion"))
  vectorizer    <- readRDS(system.file("extdata", "trained_vectorizer.rds", package = "text2emotion"))

  # 2. Preprocess input text
  preprocessed_text <- preprocess_text(text)

  # Validate preprocessing output
  if (!is.character(preprocessed_text)) {
    stop("Preprocessing failed, the output is not a character string.")
  }

  # 3. Create TF-IDF features
  # Using text2vec pipeline: tokenize -> create document-term matrix -> apply TF-IDF
  it <- text2vec::itoken(preprocessed_text, progressbar = FALSE)
  dtm <- text2vec::create_dtm(it, vectorizer)

  tfidf_matrix <- tfidf_model$transform(dtm)

  # 4. Predict emotion using trained model
  pred <- predict(trained_model, tfidf_matrix)$predictions

  # 5. Map predictions to emotions and emojis
  pred_class <- as.integer(pred)

  # 6. map emotion to emoji
  emoji_mapping <- c(
    "positive" = "\U0001F60A",   # 1
    "angry" = "\U0001F621",      # 2
    "sad" = "\U0001F622",        # 3
    "fear" = "\U0001F628",       # 4
    "neutral" = "\U0001F610"     # 5
  )

  # Get predicted emotion and corresponding emoji
  emotion_labels <- c("positive", "angry", "sad", "fear", "neutral")
  predicted_emotion <- emotion_labels[pred_class]
  predicted_emoji <- emoji_mapping[predicted_emotion]

  # 6. Return output based on requested type
  if (output_type == "emotion") {
    cat("emotion:", predicted_emotion, "\n")
    return(predicted_emotion)
  } else if (output_type == "emoji") {
    cat("emoji:", predicted_emoji, "\n")
    return(predicted_emoji)
  } else if (output_type == "textemoji") {
    result_text <- paste0(text, " ", predicted_emoji)
    cat(result_text, "\n")
    return(result_text)
  } else {
    stop("Invalid output_type. Please choose 'emotion', 'emoji', or 'textemoji'.")
  }
}
