#' Load Data from Different Formats
#'
#' The \code{load_data} function reads data from files in different formats using the \code{data.table} package in R.
#'
#' @param file_path A character string specifying the path to the file to be loaded.
#' @param file_format A character string specifying the format of the file. Supported formats are 'csv' and 'excel'.
#'
#' @return A \code{data.table} containing the loaded data.
#'
#' @export
#'
#' @import data.table
#'
#' @examples
#' # Example 1: Load data from a CSV file
#' file_path_csv <- "path/to/your/data.csv"
#' data_csv <- load_data(file_path_csv, "csv")
#'
#' # Example 2: Load data from an Excel file
#' file_path_excel <- "path/to/your/data.xlsx"
#' data_excel <- load_data(file_path_excel, "excel")
#'
load_data <- function(file_path, file_format) {
  if (file_format == "csv") {
    data <- fread(file_path)
  } else if (file_format == "excel") {
    data <- fread(file_path, sheet = 1)
  } else {
    stop("Unsupported format. Supported formats are 'csv' and 'excel'")
  }

  return(data)
}


#' Select Columns from a Data.Table
#'
#' The \code{select_columns} function is designed to extract specific columns from a data.table in R.
#'
#' @param data A data.table from which columns are to be selected.
#' @param columns_to_select A character vector containing the names of the columns to be selected.
#'
#' @return A new data.table containing only the selected columns.
#'
#' @export
#' @import data.table
#'
#' @examples
#' # Example 1: Select specific columns from a data.table
#' data_example <- data.table(ID = 1:5, Name = c("John", "Alice", "Bob", "Eva", "Mike"), Age = c(25, 30, 22, 28, 35))
#' selected_columns <- select_columns(data_example, c("ID", "Name"))
#'
#' # Example 2: Select all columns from a data.table (no change)
#' selected_all_columns <- select_columns(data_example, colnames(data_example))
#'
select_columns <- function(data, columns_to_select) {
  if (!is.data.table(data)) {
    stop("Input 'data' must be a data.table.")
  }

  if (any(!columns_to_select %in% colnames(data))) {
    stop("Not all specified columns exist in the table.")
  }

  selected_data <- data[, columns_to_select, with = F]

  return(selected_data)
}
