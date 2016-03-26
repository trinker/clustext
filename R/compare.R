#' Adjusted Rand Index Comaprison Between Algorithms
#'
#' An Adjusted Rand Index comparison of the assignments between different
#' clustering algorithms.
#'
#' @param \ldots A series of outputs from \code{assign_cluster} for various
#' cluster algorithmns.
#' @return Returns a pair-wise comparison matrix of Adjusted Rand Indices for
#' algorithm.  Higher Adjusted Rand Index scores indicate higher cluster
#' assignment agreement.
#' @references \url{http://faculty.washington.edu/kayee/pca/supp.pdf}
#' @export
#' @examples
#' compare(
#'     assignments$hierarchical_assignment,
#'     assignments$kmeans_assignment,
#'     assignments$skmeans_assignment,
#'     assignments$nmf_assignment
#' )
#'
#' ## Understanding the ARI
#' set.seed(10)
#' w <- sample(1:10, 40, TRUE)
#' x <- 11-w
#' set.seed(20)
#' y <- sample(1:10, 40, TRUE)
#' set.seed(50)
#' z <- sample(1:10, 40, TRUE)
#'
#' data.frame(w, x, y, z)
#'
#' library(mclust)
#'
#' mclust::adjustedRandIndex(w, x)
#' mclust::adjustedRandIndex(x, y)
#' mclust::adjustedRandIndex(x, z)
compare <- function(...) {
    nms <- unlist(lapply(list(...), function(x) attributes(x)[['algorithm']]))
    vouter(stats::setNames(as.data.frame(list(...)), nms), mclust::adjustedRandIndex)

}


#comare(assignments[[1]], assignments[[2]], assignments[[3]])

vouter <- function(x, FUN, ...){

    nc <- ncol(x)
    mat <- matrix(rep(NA, nc^2), nc)
    for (i in 1:nc) {
        for (j in 1:nc) {
            mat[i, j] <- FUN(.subset2(x, i), .subset2(x, j))
        }
    }
    dimnames(mat) <- list(colnames(x), colnames(x))
    class(mat) <- c("compare", class(mat))
    mat
}


#' Prints a compare Object.
#'
#' Prints a compare object.
#'
#' @param x The compare object
#' @param digits Number of decimal places to print.
#' @param \ldots ignored
#' @method print compare
#' @export
print.compare <- function(x, digits = 3, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    y <- unclass(x)
    if (is.numeric(y) & !is.null(digits)) {
        y <- round(y, digits = digits)
    }
    print(y)
    options(width=WD)
}

