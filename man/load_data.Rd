% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_functions.R
\name{load_data}
\alias{load_data}
\title{Load Data from Different Formats}
\usage{
load_data(file_path, file_format)
}
\arguments{
\item{file_path}{A character string specifying the path to the file to be loaded.}

\item{file_format}{A character string specifying the format of the file. Supported formats are 'csv' and 'excel'.}
}
\value{
A \code{data.table} containing the loaded data.
}
\description{
The \code{load_data} function reads data from files in different formats using the \code{data.table} package in R.
}
\examples{
# Example 1: Load data from a CSV file
file_path_csv <- "path/to/your/data.csv"
data_csv <- load_data(file_path_csv, "csv")

# Example 2: Load data from an Excel file
file_path_excel <- "path/to/your/data.xlsx"
data_excel <- load_data(file_path_excel, "excel")

}
