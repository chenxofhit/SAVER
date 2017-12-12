
clean.data <- function(x) {
  if (!is.matrix(x) & !bigmemory::is.big.matrix(x)) {
    x <- as.matrix(x)
    message("Converting x to matrix.")
    if (!is.numeric(x)) {
      stop("Make sure x is numeric.")
    }
  }
  np <- dim(x)
  if (is.null(np) | (np[2] <= 1))
    stop("x should be a matrix with 2 or more columns")
  if (min(colSums(x)) == 0) {
    nzerocells <- sum(colSums(x) == 0)
    x <- x[, colSums(x) != 0]
    message("Removing ", nzerocells, " cells with zero expression.")
  }
  nonzero <- which(rowSums(x) != 0)
  if (remove.zero.genes) {
    x <- x[nonzero, ]
    message("Removing ", length(nonzero), " genes with zero expression.")
  }
}
