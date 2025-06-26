#' @title s3.summarize.data
#' @author Maiolino_Aurelio
#'
#' @description
#' Summarize rows of a data.table by summing across columns
#'
#' @param data A \code{data.table} with an 'id' column and one or more numeric columns to be summed.
#'
#' @return A named numeric vector containing the row sums, with names taken from the 'id' column.
#'
#'
#' @export
s3.summarize.data <- function(
    data
) {
  summary <- data[, rowSums(.SD), .SDcols = -"id"]
  names(summary) <- data$id
  return(summary)
}
