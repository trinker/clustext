#' Fit a Kmeans Cluster
#'
#' Fit a kmeans cluster to text data.  Prior to distance measures being
#' calculated the tf-idf (see \code{\link[tm]{weightTfIdf}}) is applied to the
#' \code{\link[tm]{DocumentTermMatrix}}.
#'
#' @param x A data type (e.g., \code{\link[tm]{DocumentTermMatrix}} or
#' \code{\link[tm]{TermDocumentMatrix}}).
#' @param k The number of clusters.  Defaults to use \code{approx_k} of the
#' \code{\link[tm]{DocumentTermMatrix}} produced by \code{data_storage}.
#' @param \ldots Other arguments passed to \code{\link[stats]{kmeans}}.
#' @return Returns an object of class \code{"kmeans"}.
#' @export
#' @rdname kmeans_cluster
#' @examples
#' library(dplyr)
#'
#' x <- with(
#'     presidential_debates_2012,
#'     data_store(dialogue, paste(person, time, sep = "_"))
#' )
#'
#' ## K predicted
#' kmeans_cluster(x)
#'
#' ## 6 topic model
#' kmeans_cluster(x, k=6)
#'
#' kmeans_cluster(x, k=6) %>%
#'     assign_cluster()
#'
#' kmeans_cluster(x, k=6) %>%
#'     assign_cluster() %>%
#'     summary()
#'
#' x2 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue))
#'
#' myfit2 <- kmeans_cluster(x2, 55)
#'
#' assign_cluster(myfit2)
#'
#' assign_cluster(myfit2) %>%
#'     summary()
kmeans_cluster <- function(x, k = hclustext::approx_k(get_dtm(x)), ...){

    UseMethod("kmeans_cluster")

}


#' @export
#' @rdname kmeans_cluster
#' @method kmeans_cluster data_store
kmeans_cluster.data_store <- function(x, k = hclustext::approx_k(get_dtm(x)), ...){


    fit <- stats::kmeans(x[["dtm"]], centers=k)

    text_data_store <- new.env(FALSE)
    text_data_store[["data"]] <- x

    class(fit) <- c("kmeans_cluster", class(fit))
    attributes(fit)[["text_data_store"]] <- text_data_store
    fit
}



# #' Plots a kmeans_cluster Object
# #'
# #' Plots a kmeans_cluster object
# #'
# #' @param x A kmeans_cluster object.
# #' @param k The number of clusters (can supply \code{h} instead).  Defaults to
# #' use \code{approx_k} of the \code{\link[tm]{DocumentTermMatrix}} produced
# #' by \code{data_storage}.  Boxes are drawn around the clusters.
# #' @param h The height at which to cut the dendrograms (determines number of
# #' clusters).  If this argument is supplied \code{k} is ignored. A line is drawn
# #' showing the cut point on the dendrogram.
# #' @param color The color to make the cluster boxes (\code{k}) or line (\code{h}).
# #' @param \ldots Other arguments passed to \code{\link[stats]{rect.hclust}} or
# #' \code{\link[graphics]{abline}}.
# #' @method plot kmeans_cluster
# #' @export
# plot.kmeans_cluster <- function(x, k = approx_k(get_dtm(x)), h = NULL,
#     color = "red", ...){
#
#     if (is.null(h)) y <- k
#     class(x) <- "hclust"
#     graphics::plot(x)
#     if (is.null(h) && !is.null(k)) stats::rect.hclust(x, k = y, border = color, ...)
#     if (!is.null(h)) graphics::abline(h = h, col = color, ...)
# }




