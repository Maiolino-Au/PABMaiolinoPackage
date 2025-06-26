#' @title s8.save.plots
#' @author Maiolino_Aurelio
#'
#' @description
#' Save Scatter Plots of Gene Expression vs ATAC Intensity for All Chromosomes
#'
#' Generates and saves combined scatter plots for each chromosome (1–22, X, Y), grouping 12 chromosomes per image.
#' Each plot shows gene expression (CPM) vs ATAC-seq peak intensity (CPM) for genes on that chromosome.
#'
#' @return Invisibly returns \code{NULL}. Saves PNG files to the /sharedFolder/Results directory.
#'
#'
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggsave
#' @export
s8.save.plots <- function() {
  plot_list <- lapply(c(as.character(1:22), "X", "Y"), s8.scatter.plots)

  dir_results <- "/sharedFolder/Results"
  if (!dir.exists(dir_results)) {dir.create(dir_results)}

  for (i in 1:(length(plot_list)/12)) {
    n <- 12*(i-1)+1
    combined <- cowplot::plot_grid(plotlist = plot_list[(n):(n+11)], ncol = 3)
    ggsave(paste0(
      "/sharedFolder/Results/s8_plot_",
      i,
      ".png"
    ), plot = combined, width = 1920*2, height = 1080*3.6, units = "px")
  }
}


