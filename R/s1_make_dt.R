#' @title s1.make.dt
#' @author Maiolino_Aurelio
#'
#' @description
#' Load a sparse matrix and transforms it in a data.table with barcodes as columns, features as rows and add an "id" column with containing features identifiers.
#'
#' @param data_path Path to the directory containing \code{matrix.mtx.gz}, \code{features.tsv.gz}, and \code{barcodes.tsv.gz} files (default: \code{"/sharedFolder/Data/matrix/"}).
#'
#' @return A \code{data.table} with features as rows, barcodes as columns, and an \code{id} column containing feature identifiers.
#'
#'
#' @importFrom Matrix readMM
#' @importFrom data.table as.data.table setcolorder
#' @importFrom readr read_tsv
#' @export
s1.make.dt <- function(
    data_path = "/sharedFolder/Data/matrix/"
) {
    # Load the sparse matrix
    matrix <- readMM(file = paste0(data_path, "matrix.mtx.gz"))
    features <- read_tsv(file = paste0(data_path, "features.tsv.gz"),
                         col_names = c("id", "name", "type", "chr", "start", "end"),
                         show_col_types = F)
    barcodes <- read_tsv(file = paste0(data_path, "barcodes.tsv.gz"),
                         col_names = F,
                         show_col_types = F)

    # Assign names to colums and rows and transform the object "matrix" in a matrix
    colnames(matrix) <- barcodes$X1
    rownames(matrix) <- features$id
    matrix <- as.matrix(matrix)

    # Genearate a data.table, a id columns containg the ids of the various genes/peaks is added
    dt <- as.data.table(matrix, row.names = rownames(matrix))
    dt[, id := rownames(matrix)]
    setcolorder(dt, c("id", setdiff(names(dt), "id"))) # id is put as the first column


    return(dt)
}
