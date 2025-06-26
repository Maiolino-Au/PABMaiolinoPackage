#' @title s7.summary.table.peaks
#' @author Maiolino_Aurelio
#'
#' @description
#' Generate Summary Table of Unmerged ATAC Peaks
#'
#' Creates a summary table of ATAC-seq peaks that do not overlap any gene. The output table includes peak ID, chromosome, start, end, and CPM for each unmerged peak.
#'
#' @param atac_data A \code{GRanges} object or data.table containing ATAC-seq peak data, with columns \code{peak_id}, \code{seqnames}, \code{start}, \code{end}, \code{peak_cpm}, and \code{overlapping_genes}. Defaults to \code{gr_atac}.
#'
#' @return A \code{data.table} listing unmerged peaks with columns: \code{peak_id}, \code{chr}, \code{start}, \code{end}, and \code{peak_cpm}.
#'
#'
#' @importFrom data.table as.data.table
#' @export
s7.summary.table.peaks <- function(
    atac_data = gr_atac
) {
  unmerged <- as.data.table(atac_data)[is.na(overlapping_genes)]
  unmerged<- unmerged[, .(peak_id, seqnames, start, end, peak_cpm)]
  colnames(unmerged)[colnames(unmerged) == "seqnames"] <- "chr"

  return(unmerged)
}


