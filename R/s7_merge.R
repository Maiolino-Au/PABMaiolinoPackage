#' @title s7.merge
#' @author Maiolino_Aurelio
#'
#' @description
#' Merge Expression and ATAC Data by Gene ID
#'
#' Merges gene expression data and ATAC-seq data by gene identifier, combining expression and chromatin accessibility metrics into a single data.table.
#'
#' @param exp_data A \code{GRanges} object or data.table containing gene expression data, with columns \code{gene_id}, \code{gene_symbol}, \code{seqnames}, and \code{gene_cpm}. Defaults to \code{gr_genes}.
#' @param atac_data A \code{GRanges} object or data.table containing ATAC-seq data, with columns \code{overlapping_genes} and \code{peak_cpm}. Defaults to \code{gr_atac}.
#'
#' @return A \code{data.table} object with merged gene expression and ATAC-seq data.
#'
#'
#' @importFrom data.table as.data.table merge.data.table
#' @importFrom GenomicRanges seqnames
#' @export
s7.merge <- function(
    exp_data = gr_genes,
    atac_data = gr_atac
) {
  merged <- merge(
    as.data.table(exp_data)[, .(gene_id, gene_symbol, chr = seqnames, gene_cpm)],
    as.data.table(atac_data)[, .(overlapping_genes, peak_cpm)],
    by.x = "gene_id", by.y = "overlapping_genes", all.x = TRUE
  )

  merged$chr <- factor(merged$chr, levels = paste0("chr", c(1:22, "X", "Y")))

  return(merged)
}


