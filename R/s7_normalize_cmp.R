#' @title s7.normalize.cmp
#' @author Maiolino_Aurelio
#'
#' @description
#' Normalize and Log-Transform Count Data
#'
#' Normalizes numeric data to counts per million (CPM) and applies a log2 transformation.
#' This is commonly used for normalizing and transforming count data such as gene expression counts.
#'
#' @param data A numeric vector of counts to be normalized and log-transformed.
#'
#' @return A numeric vector of normalized and log2-transformed values.
#'
#'
#' @export
s7.normalize.cmp <- function(data) {
  log2((data / sum(data) * 1e6) + 1)
}

