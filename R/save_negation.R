#' Handle Negation in Token List
#'
#' This function processes a character vector of tokens and handles negations by
#' combining the word "not" with the immediately following word (e.g., "not happy" becomes "not_happy").
#' This technique helps to better preserve sentiment polarity during text analysis.
#'
#' @param tokens A character vector of tokens (individual words).
#'
#' @return A character vector of tokens with negations handled by combining "not" with the next word.
#'
#' @details
#' The negation handling procedure follows these steps:
#' \itemize{
#'   \item Iterate through each token.
#'   \item If a token is "not" and followed by another token, merge them into a single token separated by an underscore (e.g., "not_happy").
#'   \item Skip the next token after merging to avoid duplication.
#'   \item Otherwise, keep the token unchanged.
#' }
#'
#' This method is especially useful in sentiment analysis tasks where the presence of negations
#' can invert the sentiment polarity of words.
#'
#' @examples
#' handle_negation(c("i", "am", "not", "happy"))
#' # Returns: c("i", "am", "not_happy")
#'
#' handle_negation(c("this", "is", "not", "good", "but", "not", "terrible"))
#' # Returns: c("this", "is", "not_good", "but", "not_terrible")
#'
#' handle_negation(c("nothing", "to", "worry", "about"))
#' # Returns: c("nothing", "to", "worry", "about")
#'
#' @export
handle_negation <- function(tokens) {
  if (is.null(tokens) || length(tokens) == 0) {
    return(tokens)  # Return original tokens if NULL or empty
  }

  new_tokens <- c()    # Initialize an empty vector to store new tokens
  skip_next <- FALSE   # Flag to skip the next word if it has been combined

  for (i in seq_along(tokens)) {
    if (skip_next) {
      skip_next <- FALSE  # Reset skip flag
      next                # Skip this iteration
    }

    # If current token is "not" and there is a next token
    if (tokens[i] == "not" && i < length(tokens)) {
      # Combine "not" with the next word
      new_tokens <- c(new_tokens, paste0("not_", tokens[i + 1]))
      skip_next <- TRUE  # Set flag to skip the next token
    } else {
      new_tokens <- c(new_tokens, tokens[i])  # Otherwise, keep the token as is
    }
  }

  return(new_tokens)  # Return the modified token vector
}
