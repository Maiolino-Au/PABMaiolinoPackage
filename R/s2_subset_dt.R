#' @title s2.subset.dt
#' @author Maiolino_Aurelio
#'
#' @description
#' Subset a data.table by matching a character with the "id" column.
#'
#' @param string A character string to be mached to the \code{id} column. Examples: \code{ENSG} to select genes (if Ensembl ids are used), \code{chr} for ATAC.seq peaks.
#' @param data A \code{data.table} with an \code{id} column.
#'
#' @return A \code{data.table} containing only the rows where the 'id' column matches \code{string}.
#'
#'
#' @export
s2.subset.dt <- function(
    string,
    data
) {
  # Use grepl to create a logic that indicates which element of the data.table to take
  sub <- data[grepl(string, data$id)]
  return(sub)
}
