#' @title s7.summary.chr.wise
#' @author Maiolino_Aurelio
#'
#' @description
#' Chromosome-wise Summary of Unmerged Peaks and Genes
#'
#' Summarizes the count of unmerged ATAC-seq peaks and genes without ATAC signal for each chromosome.
#'
#' @return A \code{data.table} with columns \code{chr}, \code{N_unmerged_genes}, and \code{N_unmerged_peaks}, representing the chromosome, number of genes without ATAC peaks, and number of unmerged ATAC peaks, respectively.
#'
#'
#' @importFrom data.table as.data.table merge.data.table .N
#' @export
s7.summary.chr.wise <- function(

) {
  peak_chr_summary <- unmerged_peaks[, .N, by = chr]
  colnames(peak_chr_summary) <- c("chr", "N_unmerged_peaks")

  gr_genes_dt <- as.data.table(gr_genes)
  genes_no_atac <- merge(genes_no_atac, gr_genes_dt[, .(gene_id, seqnames)], by = "gene_id", all.x = TRUE)
  gene_chr_summary <- genes_no_atac[, .N, by = seqnames]
  colnames(gene_chr_summary) <- c("chr", "N_unmerged_genes")

  chr_wise_summary <- merge(gene_chr_summary,peak_chr_summary, all = T)

  return(chr_wise_summary)
}


