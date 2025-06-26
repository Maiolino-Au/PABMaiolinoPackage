#' @title s4.create.GenomicRanges
#' @author Maiolino_Aurelio
#'
#' @description
#' Generates a GenomicRanges object for either genes or atac peaks starting from a features file and the a summary vector (created with \code{s3.summarize.data}). Assumes that the data has already been summarized
#'
#' @param type Character string specifying the feature type to extract. Must be one of \code{"Genes"} or \code{"Peaks"} (case-insensitive).
#' @param path_f Path to the features file (default: \code{"/sharedFolder/Data/matrix/features.tsv.gz"}).
#'
#' @return A GenomicRanges object with the gene/peak id and/or name and the summary in the metadata.
#'
#'
#' @importFrom readr read_tsv
#' @importFrom data.table as.data.table
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @export
# Step 4 ___________________________________________________________________________
s4.create.GenomicRanges <- function(
    type,
    path_f = "/sharedFolder/Data/matrix/features.tsv.gz"
) {
  # Load Features
  features <- as.data.table(
    read_tsv(
      file = path_f,
      col_names = c("id", "name", "type", "chr", "start", "end"),
      show_col_types = F
    )
  )

  # Subset features for genes or peaks
  if (type %in% c("Genes", "genes")) {
    if (type != "Genes") {type = "Genes"}

    features <- features[grepl("Gene", features$type)]
    features$chr[is.na(features$chr)] <- "unknown"

    summary = genes_summary
  } else if (type %in% c("Peaks", "peaks", "ATAC", "Atac", "atac")) {
    if (type != "Peaks") {type = "Peaks"}

    features <- features[grepl("Peaks", features$type) & grepl("chr", features$chr)]
    features$chr[is.na(features$chr)] <- "unknown"

    summary = atac_summary
  } else {
    print("Error, invalid value for type: must be 'Gene' or 'Peaks'")
    return ("")
  }

  # Create GRanges object
  gr <- GRanges(
    seqnames = features$chr,
    ranges = IRanges(
      start = features$start,
      end = features$end
    )
  )

  # Add metadata
  if (type == "Genes") {
    gr$gene_sum = summary
    gr$gene_id <- features$id
    gr$gene_name <- features$name
  } else if (type == "Peaks") {
    gr$peak_sum = summary
    gr$peak_id <- features$id
  }

  return(gr)
}
