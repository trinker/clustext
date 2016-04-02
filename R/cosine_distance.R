#' Optimized Computation of Cosine Distance
#'
#' Utilizes the \pkg{slam} package to efficiently calculate cosine distance
#' on large sparse matrices.
#'
#' @param x A data type (e.g., \code{\link[tm]{DocumentTermMatrix}} or
#' \code{\link[tm]{TermDocumentMatrix}}).
#' @param \ldots ignored.
#' @return Returns a cosine distance object of class \code{"dist"}.
#' @references \url{http://stackoverflow.com/a/29755756/1000343}
#' @keywords cosine dissimilarity
#' @rdname cosine_distance
#' @export
#' @author Michael Andrec and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' library(gofastr)
#' library(dplyr)
#'
#' out <- presidential_debates_2012 %>%
#'     with(q_dtm(dialogue)) %>%
#'     cosine_distance()
cosine_distance <- function(x, ...){
    UseMethod("cosine_distance")
}


#' @export
#' @rdname cosine_distance
#' @method cosine_distance DocumentTermMatrix
cosine_distance.DocumentTermMatrix <- function(x, ...){
    x <- t(slam::as.simple_triplet_matrix(x))
    stats::as.dist(1 - slam::crossprod_simple_triplet_matrix(x)/(sqrt(slam::col_sums(x^2) %*% t(slam::col_sums(x^2)))))
}


#' @export
#' @rdname cosine_distance
#' @method cosine_distance TermDocumentMatrix
cosine_distance.TermDocumentMatrix <- function(x, ...){
    x <- slam::as.simple_triplet_matrix(x)
    stats::as.dist(1 - slam::crossprod_simple_triplet_matrix(x)/(sqrt(slam::col_sums(x^2) %*% t(slam::col_sums(x^2)))))
}


