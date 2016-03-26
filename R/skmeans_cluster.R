#' Fit a skmean Cluster
#'
#' Fit a skmean cluster to text data.  Prior to distance measures being
#' calculated the tf-idf (see \code{\link[tm]{weightTfIdf}}) is applied to the
#' \code{\link[tm]{DocumentTermMatrix}}.  Cosine dissimilarity is used to generate
#' the distance matrix supplied to \code{\link[skmeans]{skmeans}}.
#'
#' @param x A data store object (see \code{\link[clustext]{data_store}}).
#' @param k The number of clusters.
#' @param \ldots Other arguments passed to \code{\link[skmeans]{skmeans}}.
#' @return Returns an object of class \code{"skmean"}.
#' @export
#' @rdname skmeans_cluster
#' @examples
#' library(dplyr)
#'
#' x <- with(
#'     presidential_debates_2012,
#'     data_store(dialogue, paste(person, time, sep = "_"))
#' )
#'
#'
#' ## 6 topic model
#' skmeans_cluster(x, k=6)
#'
#' skmeans_cluster(x, k=6) %>%
#'     assign_cluster()
#'
#' skmeans_cluster(x, k=6) %>%
#'     assign_cluster() %>%
#'     summary()
#'
#' \dontrun{
#' x2 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue))
#'
#' myfit2 <- skmeans_cluster(x2, 55)
#'
#' assign_cluster(myfit2)
#'
#' assign_cluster(myfit2) %>%
#'     summary()
#' }
skmeans_cluster <- function(x, k, ...){

    UseMethod("skmeans_cluster")

}


#' @export
#' @rdname skmeans_cluster
#' @method skmeans_cluster data_store
skmeans_cluster.data_store <- function(x, k, ...){


    fit <- skmeans::skmeans(as.matrix(cosine_distance(x[["dtm"]])), k=k, ...)

    text_data_store <- new.env(FALSE)
    text_data_store[["data"]] <- x

    class(fit) <- c("skmeans_cluster", class(fit))
    attributes(fit)[["text_data_store"]] <- text_data_store
    fit
}






