#' Optimized Computation of Jaccard Distance
#'
#' Utilizes the \pkg{slam} package to efficiently calculate jaccard distance
#' on large sparse matrices.
#'
#' @param x A data type (e.g., \code{\link[tm]{DocumentTermMatrix}} or
#' \code{\link[tm]{TermDocumentMatrix}}).
#' @param \ldots ignored.
#' @return Returns a jaccard distance object of class \code{"dist"}.
#' @references \url{http://stackoverflow.com/a/36373333/1000343}
#' \url{http://stats.stackexchange.com/a/89947/7482}
#' @keywords jaccard dissimilarity
#' @rdname jaccard_distance
#' @export
#' @author user41844 of StackOverflow, Dmitriy Selivanov, and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' library(gofastr)
#' library(dplyr)
#'
#' out <- presidential_debates_2012 %>%
#'     with(q_dtm(dialogue)) %>%
#'     jaccard_distance()
jaccard_distance <- function(x, ...){
    UseMethod("jaccard_distance")
}


#' @export
#' @rdname jaccard_distance
#' @method jaccard_distance DocumentTermMatrix
jaccard_distance.DocumentTermMatrix <- function(x, ...){
    mat <-sign(x)
    A <- slam::tcrossprod_simple_triplet_matrix(mat)
    im <- which(A > 0, arr.ind=TRUE)
    b <- slam::row_sums(mat)
    Aim <- A[im]

    stats::as.dist(1 - Matrix::sparseMatrix(
          i = im[,1],
          j = im[,2],
          x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
          dims = dim(A)
    ))
}


#' @export
#' @rdname jaccard_distance
#' @method jaccard_distance TermDocumentMatrix
jaccard_distance.TermDocumentMatrix <- function(x, ...){
    mat <-sign(x)
    A <- slam::crossprod_simple_triplet_matrix(mat)
    im <- which(A > 0, arr.ind=TRUE)
    b <- slam::col_sums(mat)
    Aim <- A[im]

    stats::as.dist(1 - Matrix::sparseMatrix(
          i = im[,1],
          j = im[,2],
          x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
          dims = dim(A)
    ))
}

