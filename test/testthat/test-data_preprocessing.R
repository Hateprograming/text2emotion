test_that("preprocess_text basic lowercasing and punctuation", {
  input <- "I'M so HAPPY!!!"
  output <- preprocess_text(input)
  expect_true(grepl("i am so happy !", output))
})

test_that("preprocess_text handles contractions", {
  input <- "She's not here. He'll come later."
  output <- preprocess_text(input)
  expect_true(grepl("she is not here", output))
  expect_true(grepl("he will come later", output))
})

test_that("preprocess_text handles internet slang and emoticons when use_textclean = TRUE", {
  input <- "2nite I'm happy)"
  output <- preprocess_text(input, use_textclean = TRUE)
  expect_true(grepl("tonight i am happy", output))
  expect_false(grepl(":\\)", output))
})

test_that("preprocess_text respects custom slang", {
  custom <- c("hbu" = "how about you")
  input <- "hbu tbh"
  output <- preprocess_text(input, use_textclean = FALSE, custom_slang = custom)
  expect_true(grepl("how about you to be honest", output))
})

test_that("preprocess_text output is character", {
  input <- "Simple text"
  output <- preprocess_text(input)
  expect_type(output, "character")
})
