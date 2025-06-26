#' @title s7.summary.table.genes
#' @author Maiolino_Aurelio
#'
#' @description
#' Generate Summary Table of Genes Without ATAC Signal
#'
#' Creates a summary table of genes that do not have overlapping ATAC-seq signal or have a peak CPM of zero.
#' The resulting table includes gene ID, gene symbol, and gene CPM for each gene without corresponding ATAC signal.
#'
#' @param exp_data A \code{GRanges} object or data.table containing gene expression data, with columns \code{gene_id}, \code{gene_symbol}, and \code{gene_cpm}. Defaults to \code{gr_genes}.
#' @param merged A \code{data.table} containing merged gene expression and ATAC-seq data, with columns \code{gene_id}, \code{gene_symbol}, and \code{peak_cpm}. Defaults to \code{merged_data}.
#'
#' @return A \code{data.table} of genes without associated ATAC peaks, with columns: \code{gene_id}, \code{gene_symbol}, and \code{gene_cpm}.
#'
#'
#'
#' @importFrom data.table .SD
#' @export
s7.summary.table.genes <- function(
    exp_data = gr_genes,
    merged = merged_data
) {
  genes_no_atac <- merged[is.na(peak_cpm) | peak_cpm == 0]
  genes_no_atac <- genes_no_atac[, .(gene_id, gene_symbol, gene_cpm)]

  return(genes_no_atac)
}


