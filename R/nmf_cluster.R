#' Fit a Non-Negative Matrix Factorization Cluster
#'
#' Fit a robust non-negative matrix factorization cluster to text data via
#' \code{\link[rNMF]{rnmf}}.  Prior to distance measures being
#' calculated the tf-idf (see \code{\link[tm]{weightTfIdf}}) is applied to the
#' \code{\link[tm]{DocumentTermMatrix}}.
#'
#' @param x A data store object (see \code{\link[clustext]{data_store}}).
#' @param k The number of clusters.
#' @param \ldots Other arguments passed to \code{\link[rNMF]{rnmf}}.
#' @return Returns an object of class \code{"hclust"}.
#' @export
#' @rdname nmf_cluster
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
#' model6 <- nmf_cluster(x, k=6)
#'
#' model6 %>%
#'     assign_cluster()
#'
#' model6 %>%
#'     assign_cluster() %>%
#'     summary()
#' \dontrun{
#' x2 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue))
#'
#' myfit2 <- nmf_cluster(x2, 55)
#'
#' assign_cluster(myfit2)
#'
#' assign_cluster(myfit2) %>%
#'     summary()
#' }
nmf_cluster <- function(x, k = k, ...){

    UseMethod("nmf_cluster")

}


#' @export
#' @rdname nmf_cluster
#' @method nmf_cluster data_store
nmf_cluster.data_store <- function(x, k,  ...){

    fit <- rNMF::rnmf(as.matrix(x[["dtm"]]), k = k, ...)


    text_data_store <- new.env(FALSE)
    text_data_store[["data"]] <- x

    class(fit) <- c("nmf_cluster", class(fit))
    attributes(fit)[["text_data_store"]] <- text_data_store
    fit
}







