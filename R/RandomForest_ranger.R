#' Train a Random Forest Model with TF-IDF Features
#'
#' @param train_matrix A sparse matrix (`dgCMatrix`) of training features.
#' @param train_labels A factor vector of training labels.
#' @param ntree Number of trees. Default 300.
#' @param mtry Variables to consider at each split. If NULL, auto-selected.
#' @param seed Random seed. Default 123.
#' @param verbose Whether to print progress. Default TRUE.
#' @param train_df_cache_path Path to cache the train data frame. Default "train_df_cached.rds".
#'
#' @return A trained `ranger` model object.
#' @import ranger
#' @export
train_rf_model <- function(train_matrix, train_labels,
                           ntree = 300, mtry = NULL,
                           seed = 123, verbose = TRUE,
                           train_df_cache_path = "train_df_cached.rds") {
  if (!inherits(train_matrix, "dgCMatrix")) {
    stop("train_matrix must be of class dgCMatrix (sparse matrix).")
  }
  if (!is.factor(train_labels)) train_labels <- factor(train_labels)

  if (nrow(train_matrix) != length(train_labels)) {
    warning("Training data and labels mismatch. Trimming to minimum length.")
    min_len <- min(nrow(train_matrix), length(train_labels))
    train_matrix <- train_matrix[1:min_len, ]
    train_labels <- train_labels[1:min_len]
  }

  # Load or create training data frame
  if (file.exists(train_df_cache_path)) {
    if (verbose) cat("Loading cached training data...\n")
    train_df <- readRDS(train_df_cache_path)
  } else {
    if (verbose) cat("Creating training data frame and saving cache...\n")
    train_df <- as.data.frame(as.matrix(train_matrix), check.names = FALSE, stringsAsFactors = FALSE)
    train_df$label <- train_labels
    saveRDS(train_df, train_df_cache_path)
  }

  if (verbose) cat("Training Random Forest model with ranger...\n")
  set.seed(seed)
  rf_model <- ranger::ranger(
    dependent.variable.name = "label",
    data            = train_df,
    num.trees       = ntree,
    mtry            = mtry,
    classification  = TRUE,
    write.forest    = TRUE,
    seed            = seed,
    num.threads     = parallel::detectCores()
  )

  saveRDS(rf_model, "trained_rf_ranger_model.rds")
  return(rf_model)
}
