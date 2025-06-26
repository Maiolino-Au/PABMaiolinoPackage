#' @title s8.scatter.plots
#' @author Maiolino_Aurelio
#'
#' @description
#' Scatter Plot of Gene Expression vs ATAC Peak Intensity for a Chromosome
#'
#' Generates a scatter plot showing the relationship between gene expression (CPM) and ATAC-seq peak intensity (CPM) for a specified chromosome.
#' Only genes with non-missing ATAC peak CPM values are plotted.
#'
#' @param chr_num An integer or character indicating the chromosome number (e.g., 1, 2, ..., 22, "X", "Y").
#'
#' @return A ggplot object representing the scatter plot for the selected chromosome.
#'
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlim ylim theme_bw theme element_text
#' @export
s8.scatter.plots <- function(chr_num) {
  plot <- ggplot(
    merged_data[!is.na(peak_cpm) & merged_data$chr == paste0("chr", chr_num)],
    aes(x=gene_cpm, y=peak_cpm)
  ) +
    geom_point(alpha=0.5) +
    labs(
      x = "Gene Expression CPM (log2)",
      y = "ATAC Peaks CPM (log2)",
      title = paste("Chromosme", chr_num)
    ) +
    xlim(0, 13) +
    ylim(0, 9) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot)
}


