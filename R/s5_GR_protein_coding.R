#' @title s5.GR.protein.coding
#' @author Maiolino_Aurelio
#'
#' @description
#' Imports a GTF file and returns a \code{GRanges} object containing only protein-coding genes.
#' Standardizes the chromosome names to include "chr"
#'
#' @param path Path to the GTF annotation file (default: \code{"/sharedFolder/Data/Homo_sapiens.GRCh38.114.gtf.gz"}).
#'
#' @return A \code{GRanges} object containing only rows where \code{type == "gene"} and \code{gene_biotype == "protein_coding"}.
#'
#'
#' @importFrom rtracklayer import
#' @importFrom GenomeInfoDb seqlevels<-
#' @importFrom GenomeInfoDb seqlevels
#' @export
s5.GR.protein.coding <- function(
    path = "/sharedFolder/Data/Homo_sapiens.GRCh38.114.gtf.gz"
) {
  # Import the GTF file
  data <- rtracklayer::import(path)

  # Standardize the names (in the files the chromosomes are called by number/letter)
  seqlevels(data) <- ifelse(
    grepl(
      "^([1-9]|1[0-9]|2[0-2]|X|Y)$", # [1-22] would not work but just check from 1 to 2, with the second 2 ignored
      seqlevels(data)
    ),
    paste0(
      "chr",
      seqlevels(data)
    ),
    seqlevels(data)
  )

  # Filter for protein coding genes
  data_coding <- data[data$type == "gene" & data$gene_biotype == "protein_coding"]

  return(data_coding)
}
