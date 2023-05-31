#' Calculate the entropy for scRNA-seq Data
#'
#' This will look at a matrix of counts data from an scRNA-seq experiment and
#' calculate the entropy of each gene.
#'
#' @param counts The counts matrix
#'
#' @return The entropy per gene
#' @export
#'
#' @examples
#'
#' mat <- matrix(1:12, ncol = 3)
#' calc_entropy()
#'
calc_entropy <- function(counts) {

  if (missing(counts)) {
    stop("\n \u2716 Counts matrix not supplied")
  }

  norm_counts <- apply(
    counts,
    2,
    \(x){x/sum(x)}
  )

  entr_counts <- apply(
    norm_counts,
    2,
    \(x){entropy::entropy(x)}
  )

  entr_counts

}

#' Threshold Genes Based on Their Entropy
#'
#' This is a description
#'
#' @param counts Counts matrix
#' @param threshold The threshold
#'
#' @return A vector
#' @export
#'
#' @examples
cut_entropy <- function(counts, threshold) {

  is_valid <- TRUE
  error_message <- ""

  if (missing(counts)) {
    is_valid <- FALSE
    error_message <- paste0(error_message, "\n \u2716 Counts matrix is missing")
  }
  if (missing(threshold)) {
    is_valid <- FALSE
    error_message <- paste0(error_message, "\n \u2716 Threshold is missing")
  }

  if (!is_valid) {
    stop(error_message)
  }

  entr <- calc_entropy(counts = counts)

  entr < threshold

}
