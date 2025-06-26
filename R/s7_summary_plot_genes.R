#' @title s7.summary.plot.genes
#' @author Maiolino_Aurelio
#'
#' @description
#' Plot Gene Expression by Chromosome for Genes With and Without ATAC Peaks
#'
#' Generates and saves boxplots of gene expression (CPM) distributions per chromosome for genes associated with ATAC-seq peaks and for genes without such peaks.
#' The resulting plots are saved as PNG files in a specified results directory.
#'
#' @param data A \code{data.table} or data frame containing merged gene expression and ATAC-seq peak data, with columns \code{chr}, \code{gene_cpm}, and \code{peak_cpm}. Defaults to \code{merged_data}.
#'
#' @return A list with two ggplot objects: the first for genes with ATAC peaks, the second for genes without ATAC peaks.
#'
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot theme element_text labs ggsave
#' @export
s7.summary.plot.genes <- function(
    data = merged_data
) {
  plot <- list()

  # atac
  data_atac <- data[peak_cpm > 0 & grepl("chr", data$chr)]
  merged <- ggplot(data_atac, aes(x=chr, y=gene_cpm)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle=90)) +
    labs(title = "Expression of Genes with ATAC Peaks")

  # no atac
  data_no_atac <- data[(is.na(peak_cpm)| peak_cpm == 0) & grepl("chr", data$chr)]
  unmerged <- ggplot(data_no_atac, aes(x=chr, y=gene_cpm)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle=90)) +
    labs(title = "Expression of Genes without ATAC Peaks")

  #Save
  dir_results <- "/sharedFolder/Results"
  if (!dir.exists(dir_results)) {dir.create(dir_results)}

  ggsave("/sharedFolder/Results/s7_gene_expression_merged.png", plot = merged)
  ggsave("/sharedFolder/Results/s7_gene_expression_unmerged.png", plot = unmerged)

  return(list(merged, unmerged))
}


