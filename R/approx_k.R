#' Approximate Number of Clusters for a Text Matrix
#'
#' Can & Ozkarahan (1990) formula for approximating the number of clusters for
#' a text matrix: \eqn{(m * n)/t} where \eqn{m} and \eqn{n} are the dimensions
#' of the matrix and \eqn{t} is the length of the non-zero elements in matrix
#' \eqn{A}.
#'
#' @param x A matrix.
#' @param verbose logical.  If \code{TRUE} the k determination is printed.
#' @return Returns an integer.
#' @references Can, F., Ozkarahan, E. A. (1990). Concepts and effectiveness of
#' the cover-coefficient-based clustering methodology for text databases.
#' ACM Transactions on Database Systems 15 (4): 483. doi:10.1145/99935.99938. \cr
#' @rdname approx_k
#' @export
#' @examples
#' library(gofastr)
#' library(dplyr)
#'
#' presidential_debates_2012 %>%
#'     with(q_dtm(dialogue)) %>%
#'     approx_k()
approx_k <- function(x, verbose = TRUE){

    UseMethod("approx_k")

}

#' @export
#' @rdname approx_k
#' @method approx_k TermDocumentMatrix
approx_k.TermDocumentMatrix <- function(x, verbose = TRUE) {
    m <- round(do.call("*", as.list(dim(x)))/length(x[["v"]]))
    if (verbose) cat(sprintf("\nk approximated to: %s\n", m))
    m
}

#' @export
#' @rdname approx_k
#' @method approx_k DocumentTermMatrix
approx_k.DocumentTermMatrix <- function(x, verbose = TRUE) {
    m <- round(do.call("*", as.list(dim(x)))/length(x[["v"]]))
    if (verbose) cat(sprintf("\nk approximated to: %s\n", m))
    m
}



