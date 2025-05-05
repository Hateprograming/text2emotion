test_that("handle_negation combines 'not' with next word correctly", {
  tokens <- c("i", "am", "not", "happy")
  result <- handle_negation(tokens)

  # Check if 'not' and next word are combined
  expect_equal(result, c("i", "am", "not_happy"))
})

test_that("handle_negation handles multiple negations correctly", {
  tokens <- c("this", "is", "not", "good", "but", "not", "terrible")
  result <- handle_negation(tokens)

  # Check if multiple 'not' phrases are combined
  expect_equal(result, c("this", "is", "not_good", "but", "not_terrible"))
})

test_that("handle_negation returns same tokens if no 'not' present", {
  tokens <- c("everything", "is", "awesome")
  result <- handle_negation(tokens)

  # If no 'not' is present, output should be identical to input
  expect_equal(result, tokens)
})

test_that("handle_negation handles empty input gracefully", {
  tokens <- character(0)
  result <- handle_negation(tokens)

  # Empty input should return empty output
  expect_equal(result, character(0))
})

test_that("handle_negation handles NULL input gracefully", {
  result <- handle_negation(NULL)

  # NULL input should return NULL
  expect_null(result)
})
