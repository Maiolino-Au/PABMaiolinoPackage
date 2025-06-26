#' @title s7.summary.plot.peaks
#' @author Maiolino_Aurelio
#'
#' @description
#' Plot Peak Intensity Distribution by Chromosome
#'
#' Generates and saves boxplots of ATAC-seq peak intensity (CPM) distributions per chromosome for both merged (gene-associated) and unmerged peaks.
#' The resulting plots are saved as PNG files in a results directory.
#'
#' @param data_merged A \code{data.table} or data frame containing merged gene-peak data, including columns \code{chr} and \code{peak_cpm}. Defaults to \code{merged_data}.
#' @param data_unmerged A \code{data.table} or data frame containing unmerged peaks, including columns \code{chr} and \code{peak_cpm}. Defaults to \code{unmerged_peaks}.
#'
#' @return A list with two ggplot objects: the first for merged peaks, the second for unmerged peaks.
#'
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot theme element_text labs ggsave
#' @export
s7.summary.plot.peaks <- function(
    data_merged = merged_data,
    data_unmerged = unmerged_peaks
) {
  # Provide "a plot of peak intensity distribution chromosome by chromosome"
  # For peaks that could be merged or for the ones that could not?

  chr_levels <- paste0("chr", c(1:22, "X", "Y"))
  unmerged_peaks$chr <- factor(unmerged_peaks$chr, levels = chr_levels)

  merged <- ggplot(data_merged[!is.na(peak_cpm)], aes(x=chr, y=peak_cpm)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle=90)) +
    labs(title = "Peak intensity distribution for merged peaks")

  unmerged <- ggplot(data_unmerged[!is.na(peak_cpm)], aes(x=chr, y=peak_cpm)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle=90)) +
    labs(title = "Peak intensity distribution for unmerged peaks")

  #Save
  dir_results <- "/sharedFolder/Results"
  if (!dir.exists(dir_results)) {dir.create(dir_results)}

  ggsave("/sharedFolder/Results/s7_peaks_intesnisty_merged.png", plot = merged)
  ggsave("/sharedFolder/Results/s7_peaks_intesnisty_unmerged.png", plot = unmerged)

  return(list(merged, unmerged))
}


