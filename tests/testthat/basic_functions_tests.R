library(dt.basics)
library(testthat)
library(data.table)

example_data <- data.table(A = c(1, 2, 3), B = c("a", "b", "c"), C = c(10, 20, 30))

test_that("select_columns selects columns correctly", {

  selected_data_one_col <- select_columns(example_data, "A")
  expect_equal(selected_data_one_col, c(1, 2, 3))

  selected_data_multiple_cols <- select_columns(example_data, c("A", "C"))
  expect_equal(selected_data_multiple_cols, data.table(A = c(1, 2, 3), C = c(10, 20, 30)))

  selected_data_all_cols <- select_columns(example_data, c("A", "B", "C"))
  expect_equal(selected_data_all_cols, example_data)
})

test_that("select_columns handles invalid input", {

  expect_error(select_columns(data.frame(A = c(1, 2, 3)), "A"),
               "Input 'data' must be a data.table.")

  expect_error(select_columns(example_data, c("A", "Nonexistent")),
               "Not all specified columns exist in the table.")
})
