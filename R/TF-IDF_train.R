#' Train a TF-IDF Model (for Training Phase)
#'
#' @description
#' Train a TF-IDF model with customizable tokenization and vocabulary pruning.
#'
#' @param preprocessed_text A character vector containing the preprocessed text.
#' @param max_features The maximum number of features (terms) to include in the vocabulary. Default is 10000.
#' @param min_df Minimum document frequency for terms. Default is 2 (terms must appear in at least 2 documents).
#' @param max_df Maximum document frequency as a proportion of documents. Default is 0.8 (terms must appear in less than 80\% of documents).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{tfidf_model}{The trained TF-IDF model object.}
#'   \item{vectorizer}{The vocabulary vectorizer used in training.}
#'   \item{tfidf_matrix}{The TF-IDF sparse matrix representing the text data.}
#' }
#'
#' @details
#' This function performs the following steps:
#'
#' 1. Tokenizes the preprocessed text into words and removes stopwords.
#' 2. Defines custom stopwords and retains important emotional function words.
#' 3. Creates a vocabulary based on unigrams and trigrams, pruning terms based on document frequency and term count.
#' 4. Builds the TF-IDF sparse matrix for the input text.
#'
#' @examples
#' preprocessed_text <- c("I'm feeling so happy today!", "I feel really excited and hopeful!")
#' result <- train_tfidf_model(preprocessed_text)
#' result$tfidf_model  # Access the trained TF-IDF model
#'
#' @importFrom text2vec itoken create_vocabulary prune_vocabulary vocab_vectorizer TfIdf create_dtm word_tokenizer
#' @importFrom magrittr %>%
#' @export
train_tfidf_model <- function(preprocessed_text,
                              max_features = 10000,
                              min_df = 2,
                              max_df = 0.8) {
  if (!is.character(preprocessed_text)) {
    stop("Input must be a character vector.")
  }

  # Define custom stopwords (including function words for sentiment retention)
  custom_stopwords <- c(
    "a", "about", "above", "after", "again", "all", "am",
    "an", "and", "any", "are", "aren't", "as", "at", "be", "because",
    "been", "before", "being", "below", "between", "both", "but", "by",
    "can", "can't", "cannot", "could", "couldn't", "did", "didn't", "do",
    "does", "doesn't", "doing", "don't", "down", "during", "each", "few",
    "for", "from", "further", "had", "hadn't", "has", "hasn't", "have",
    "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here",
    "hers", "herself", "him", "himself", "his", "how", "i", "i'd", "i'll",
    "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its",
    "itself", "let's", "me", "more", "most", "mustn't", "my", "myself",
    "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other",
    "ought", "our", "ours", "ourselves", "out", "over", "own", "same",
    "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some",
    "such", "than", "that", "that's", "the", "their", "theirs", "them",
    "themselves", "then", "there", "there's", "these", "they", "they'd",
    "they'll", "they're", "they've", "this", "those", "through", "to",
    "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd",
    "we'll", "we're", "we've", "were", "weren't", "what", "what's",
    "when", "when's", "where", "where's", "which", "while", "who", "who's",
    "whom", "why", "why's", "will", "with", "won't", "would", "wouldn't",
    "you", "you'd", "you'll", "you're", "you've", "your", "yours",
    "yourself", "yourselves", "movie", "product","name", "just","people","get",
    "one","know","think", "now", "loud", "s","time", "see", "ve", "much", "got",
    "go", "way", "tongue" ,"still", "really", "very","d","make", "man", "e", "going",
    "back", "thing" , "us", "also", "t", "someone", "say", "something", "day", "look",
    "first", "year", "work","looks", "makes", "years", "take", "made", "said",
    "life", "post", "things", "find", "getting"
  )

  # Keep sentiment-bearing words
  keep_words <- c("not", "no", "never")
  final_stopwords <- setdiff(custom_stopwords, keep_words)

  # Tokenize text and remove stopwords
  tokens <- text2vec::word_tokenizer(tolower(preprocessed_text))
  tokens_filtered <- lapply(tokens, function(x) {
    x[!x %in% final_stopwords]
  })

  # Create itoken iterator
  it <- text2vec::itoken(tokens_filtered, progressbar = FALSE)

  # Create and prune vocabulary (unigrams and trigrams)
  vocab <- text2vec::create_vocabulary(it, ngram = c(1L, 3L)) %>%
    text2vec::prune_vocabulary(
      term_count_min = min_df,
      doc_proportion_max = max_df,
      vocab_term_max = max_features
    )

  # Create vectorizer and TF-IDF model
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  tfidf <- text2vec::TfIdf$new()

  # Create TF-IDF sparse matrix
  dtm <- text2vec::create_dtm(it, vectorizer)
  tfidf_matrix <- tfidf$fit_transform(dtm)

  # Return results
  return(list(
    tfidf_model = tfidf,
    vectorizer = vectorizer,
    tfidf_matrix = tfidf_matrix
  ))
}
