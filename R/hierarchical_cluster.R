#' Fit a Hierarchical Cluster
#'
#' Fit a hierarchical cluster to text data.  Prior to distance measures being
#' calculated the tf-idf (see \code{\link[tm]{weightTfIdf}}) is applied to the
#' \code{\link[tm]{DocumentTermMatrix}}.  Cosine dissimilarity is used to generate
#' the distance matrix supplied to \code{\link[fastcluster]{hclust}}.  \code{method}
#' defaults to \code{"ward.D2"}.  A faster cosine dissimilarity calculation is used
#' under the hood (see \code{\link[clustext]{cosine_distance}}).  Additionally,
#' \code{\link[fastcluster]{hclust}} is used to quickly calculate the fit.
#' Essentially, this is a wrapper function optimized for clustering text data.
#'
#' @param x A data store object (see \code{\link[clustext]{data_store}}).
#' @param distance A distance measure ("cosine" or "jaccard").
#' @param method The agglomeration method to be used. This must be (an
#' unambiguous abbreviation of) one of \code{"single"}, \code{"complete"},
#' \code{"average"}, \code{"mcquitty"}, \code{"ward.D"}, \code{"ward.D2"},
#' \code{"centroid"}, or \code{"median"}.
#' @param \ldots ignored.
#' @return Returns an object of class \code{"hclust"}.
#' @export
#' @rdname hierarchical_cluster
#' @examples
#' library(dplyr)
#'
#' x <- with(
#'     presidential_debates_2012,
#'     data_store(dialogue, paste(person, time, sep = "_"))
#' )
#'
#' hierarchical_cluster(x) %>%
#'     plot(k=4)
#'
#' hierarchical_cluster(x) %>%
#'     plot(h=.7, lwd=2)
#'
#' hierarchical_cluster(x) %>%
#'     assign_cluster(h=.7)
#'
#' hierarchical_cluster(x, method="complete") %>%
#'     plot(k=6)
#'
#' hierarchical_cluster(x) %>%
#'     assign_cluster(k=6)
#'
#' x2 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue))
#'
#' myfit2 <- hierarchical_cluster(x2)
#'
#' plot(myfit2)
#' plot(myfit2, 55)
#'
#' assign_cluster(myfit2, k = 55)
hierarchical_cluster <- function(x, distance = 'cosine', method = "ward.D2", ...){

    UseMethod("hierarchical_cluster")

}


#' @export
#' @rdname hierarchical_cluster
#' @method hierarchical_cluster data_store
hierarchical_cluster.data_store <- function(x, distance = 'cosine', method = "ward.D", ...){

    distmes <- switch(distance,
        'cosine' = cosine_distance,
        'jaccard' = jaccard_distance,
        stop('provide a valid `distance` type')
    )
    fit <- fastcluster::hclust(distmes(x[["dtm"]]), method = method)

    text_data_store <- new.env(FALSE)
    text_data_store[["data"]] <- x

    class(fit) <- c("hierarchical_cluster", class(fit))
    attributes(fit)[["text_data_store"]] <- text_data_store
    fit
}



#' Plots a hierarchical_cluster Object
#'
#' Plots a hierarchical_cluster object
#'
#' @param x A hierarchical_cluster object.
#' @param k The number of clusters (can supply \code{h} instead).  Defaults to
#' use \code{approx_k} of the \code{\link[tm]{DocumentTermMatrix}} produced
#' by \code{data_storage}.  Boxes are drawn around the clusters.
#' @param h The height at which to cut the dendrograms (determines number of
#' clusters).  If this argument is supplied \code{k} is ignored. A line is drawn
#' showing the cut point on the dendrogram.
#' @param color The color to make the cluster boxes (\code{k}) or line (\code{h}).
#' @param \ldots Other arguments passed to \code{\link[stats]{rect.hclust}} or
#' \code{\link[graphics]{abline}}.
#' @method plot hierarchical_cluster
#' @export
plot.hierarchical_cluster <- function(x, k = approx_k(get_dtm(x)), h = NULL,
    color = "red", ...){

    if (is.null(h)) y <- k
    class(x) <- "hclust"
    graphics::plot(x)
    if (is.null(h) && !is.null(k)) stats::rect.hclust(x, k = y, border = color, ...)
    if (!is.null(h)) graphics::abline(h = h, col = color, ...)
}




