#' Optimized Computation of jaccard Distance
#'
#' Utilizes the \pkg{slam} package to efficiently calculate jaccard distance
#' on large sparse matrices.
#'
#' @param x A data type (e.g., \code{\link[tm]{DocumentTermMatrix}} or
#' \code{\link[tm]{TermDocumentMatrix}}).
#' @return Returns a jaccard distance object of class \code{"dist"}.
#' @references \url{http://stackoverflow.com/a/29755756/1000343}
#' @keywords jaccard dissimilarity
#' @rdname jaccard_distance
#' @export
#' @author Michael Andrec and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' library(gofastr)
#' library(dplyr)
#'
#' presidential_debates_2012 %>%
#'     with(q_dtm(dialogue)) %>%
#'     jaccard_distance()
jaccard_distance <- function(x){
    UseMethod("jaccard_distance")
}


#' @export
#' @rdname jaccard_distance
#' @method jaccard_distance DocumentTermMatrix
jaccard_distance.DocumentTermMatrix <- function(x){
    x <- t(slam::as.simple_triplet_matrix(x))
    stats::as.dist(1 - slam::crossprod_simple_triplet_matrix(x)/(sqrt(slam::col_sums(x^2) %*% t(slam::col_sums(x^2)))))
}


#' @export
#' @rdname jaccard_distance
#' @method jaccard_distance TermDocumentMatrix
jaccard_distance.TermDocumentMatrix <- function(x){
    x <- slam::as.simple_triplet_matrix(x)
    stats::as.dist(1 - slam::crossprod_simple_triplet_matrix(x)/(sqrt(slam::col_sums(x^2) %*% t(slam::col_sums(x^2)))))
}


