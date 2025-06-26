#' @title s5.remap.atac
#' @author Maiolino_Aurelio
#'
#' @description
#' #' Remap ATAC Peaks to Overlapping Coding Genes
#'
#' Finds overlaps between ATAC-seq peaks and coding genes, and annotates each peak
#' with the gene(s) it overlaps. If a peak overlaps multiple genes, the gene IDs are
#' concatenated in a comma-separated list.
#'
#' @param atac_data A \code{GRanges} object containing ATAC-seq peaks. Defaults to \code{gr_atac}.
#'
#'
#' @return A \code{GRanges} object with an additional column \code{overlapping_genes} listing overlapping gene IDs.
#'
#'
#'
#' @importFrom GenomicRanges findOverlaps
#' @importFrom S4Vectors subjectHits queryHits
#' @export
s5.remap.atac <- function(
    atac_data = gr_atac
) {
  # Find the atac peaks that overlap with coding genes
  overlaps <- findOverlaps(atac_data, h38_coding)

  # Create a list of the genes for which there is an overlap
  gene_list <- split(h38_coding$gene_id[subjectHits(overlaps)], queryHits(overlaps))

  # Assign the genes to the corresponding peaks
  # In the case a peak is overlapping multiple genes they are added as a comma separated list
  atac_data$overlapping_genes <- NA
  atac_data$overlapping_genes[as.integer(names(gene_list))] <- sapply(gene_list, paste, collapse = ",")

  return(atac_data)
}


