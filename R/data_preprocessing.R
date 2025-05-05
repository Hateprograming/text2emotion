#' Preprocess Text with Slang Handling
#'
#' This function performs multi-stage text preprocessing, including lowercasing,
#' HTML cleaning, punctuation normalization, contraction expansion, internet slang replacement,
#' emoticon replacement, and final standardization.
#'
#' @param text A character vector of input texts.
#' @param use_textclean Logical. Whether to use \code{textclean} for internet slang and emoticon replacement. Default is \code{TRUE}.
#' @param custom_slang A named character vector providing user-defined slang mappings. Optional.
#'
#' @return  A character vector of cleaned and normalized text.
#'
#' @details
#' The preprocessing pipeline includes:
#' \itemize{
#'   \item Lowercasing the text.
#'   \item Replacing HTML entities and non-ASCII characters.
#'   \item Expanding common English contractions (e.g., "I'm" -> "I am").
#'   \item Replacing internet slang and emoticons if \code{use_textclean} is \code{TRUE}.
#'   \item Handling additional slang defined by the user.
#'   \item Normalizing repeated punctuations and whitespace.
#' }
#'
#' @examples
#' preprocess_text("I'm feeling lit rn!!!")
#' preprocess_text("I can't believe it... lol :)", use_textclean = TRUE)
#'
#' @importFrom stringr str_to_lower str_replace_all str_squish
#' @importFrom textclean replace_html replace_non_ascii replace_internet_slang replace_emoticon
#' @importFrom magrittr %>%
#' @export
preprocess_text <- function(text,
                            use_textclean = TRUE,
                            custom_slang = NULL) {
  # Stage 1: Basic cleaning
  text <- text %>%
    stringr::str_to_lower() %>%
    # Replace HTML entities (e.g., &nbsp;)
    textclean::replace_html() %>%
    # Standardize punctuation (e.g., full-width to half-width)
    textclean::replace_non_ascii()

  # Stage 2: Contraction expansion
  contractions <- c(
    # Prioritize "ll" forms first
    "i'll" = "i will",
    "he'll" = "he will",
    "she'll" = "she will",
    "we'll" = "we will",
    "you'll" = "you will",
    "they'll" = "they will",

    # Then expand other contractions
    "i'm" = "i am",
    "you're" = "you are",
    "he's" = "he is",
    "she's" = "she is",
    "it's" = "it is",
    "we're" = "we are",
    "they're" = "they are",
    "that's" = "that is",
    "there's" = "there is",
    "here's" = "here is",
    "let's" = "let us",

    # Finally, handle negations
    "can't" = "can not",
    "won't" = "will not",
    "don't" = "do not",
    "doesn't" = "does not",
    "didn't" = "did not",
    "isn't" = "is not",
    "aren't" = "are not",
    "wasn't" = "was not",
    "weren't" = "were not",
    "haven't" = "have not",
    "hasn't" = "has not",
    "hadn't" = "had not",
    "shan't" = "shall not"
  )
  text <- text %>%
    stringr::str_replace_all(contractions)

  # Stage 3: Slang handling
  if (use_textclean) {
    text <- text %>%
      textclean::replace_internet_slang() %>%
      textclean::replace_emoticon()
  }

  # Define common slang terms
  all_slang <- c(
    "lit" = "extremely amazing",
    "af" = "as fuck",
    "u" = "you",
    "ur" = "your",
    "rn" = "right now",
    "gg" = "good game",
    "gl" = "good luck",
    "hf" = "have fun",
    "tbh" = "to be honest",
    "idc" = "i don't care",
    "op" = "overpowered",
    "nerf" = "weaken",
    "buff" = "strengthen",
    "4" = "for",
    "2" = "to",
    "2nite" = "tonight",
    "@" = "at",
    "abt" = "about",
    "f" = "fuck",
    custom_slang
  )

  # Replace slang terms with their full forms
  for (i in seq_along(all_slang)) {
    text <- stringr::str_replace_all(
      text,
      stringr::regex(
        paste0("\\b", names(all_slang)[i], "\\b"),
        ignore_case = TRUE
      ),
      all_slang[i]
    )
  }

  if (!is.null(custom_slang)) {
    for (i in seq_along(custom_slang)) {
      text <- stringr::str_replace_all(
        text,
        stringr::regex(
          paste0("\\b", names(custom_slang)[i], "\\b"),
          ignore_case = TRUE
        ),
        custom_slang[[i]]
      )
    }
  }


  # Stage 4: Final standardization
  text %>%
    # Normalize repeated punctuation (preserve emotional intensity)
    stringr::str_replace_all("([!?.]){2,}", " \\1") %>%
    # Standardize punctuation spacing
    stringr::str_replace_all("([[:punct:]])", " \\1 ") %>%
    stringr::str_squish()
}

