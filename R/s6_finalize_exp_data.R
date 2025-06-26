#' @title s6.finalize.exp.data
#' @author Maiolino_Aurelio
#'
#' @description
#' Finalize Expression Data with Gene Symbols
#'
#' Annotates expression data by adding gene symbols based on gene IDs, using a reference annotation.
#' Rows without a matching gene symbol are removed from the returned dataset.
#'
#' @param exp_data A \code{GRanges} object containing gene expression data, with a \code{gene_id} column. Defaults to \code{gr_genes}.
#'
#' @return A \code{GRanges} object with an added \code{gene_symbol} column and filtered rows where gene symbol could not be assigned.
#'
#'
#' @importFrom S4Vectors mcols
#' @export
s6.finalize.exp.data <- function(
    exp_data = gr_genes
) {
  # Add a column to with gene symbols by matching the gene_id to the annotation from the GTF file
  exp_data$gene_symbol <- h38_coding$gene_name[match(exp_data$gene_id, h38_coding$gene_id)]

  # # Remove rows where gene_symbol is NA
  exp_data <- exp_data[!is.na(exp_data$gene_symbol)]

  return(exp_data)
}


