#' Assign Clusters to Documents/Text Elements
#'
#' Assign clusters to documents/text elements.
#'
#' @param x a \code{xxx_cluster} object.
#' @param k The number of clusters (can supply \code{h} instead).  Defaults to
#' use \code{approx_k} of the \code{\link[tm]{DocumentTermMatrix}} produced
#' by \code{data_storage}.
#' @param h The height at which to cut the dendrograms (determines number of
#' clusters).  If this argument is supplied \code{k} is ignored.
#' @param cut The type of cut method to use for \code{hierarchical_cluster}; one
#' of \code{'static'}, \code{'dynamic'} or \code{'iterative'}.
#' @param deepSplit logical.  See \code{\link[dynamicTreeCut]{cutreeDynamic}}.
#' @param minClusterSize The minimum cluster size.  See
#' \code{\link[dynamicTreeCut]{cutreeDynamic}}.
#' @param \ldots ignored.
#' @return Returns an \code{assign_cluster} object; a named vector of cluster
#' assignments with documents as names.  The object also contains the original
#' \code{data_storage} object and a \code{join} function.  \code{join} is a
#' function (a closure) that captures information about the \code{assign_cluster}
#' that makes rejoining to the original data set simple.  The user simply
#' supplies the original data set as an argument to \code{join}
#' (\code{attributes(FROM_ASSIGN_CLUSTER)$join(ORIGINAL_DATA)}).
#' @rdname assign_cluster
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' x <- with(
#'     presidential_debates_2012,
#'     data_store(dialogue, paste(person, time, sep = "_"))
#' )
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
#'
#' x2 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     hierarchical_cluster()
#'
#' ca2 <- assign_cluster(x2, k = 55)
#' summary(ca2)
#'
#' ## Dynamic cut
#' ca3 <- assign_cluster(x2, cut = 'dynamic', minClusterSize = 5)
#' get_text(ca3)
#'
#' ## add to original data
#' attributes(ca2)$join(presidential_debates_2012)
#'
#' ## split text into clusters
#' get_text(ca2)
#'
#' ## Kmeans Algorithm
#' kmeans_cluster(x, k=6) %>%
#'     assign_cluster()
#'
#' x3 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     kmeans_cluster(55)
#'
#' ca3 <- assign_cluster(x3)
#' summary(ca3)
#'
#' ## split text into clusters
#' get_text(ca3)
#' }
assign_cluster <- function(x, k = approx_k(get_dtm(x)), h = NULL, ...){
     UseMethod("assign_cluster")
}


#' @export
#' @rdname assign_cluster
#' @method assign_cluster hierarchical_cluster
assign_cluster.hierarchical_cluster <- function(x, k = approx_k(get_dtm(x)),
    h = NULL, cut = 'static', deepSplit = TRUE, minClusterSize = 1, ...){

    id_temporary <- n <- NULL


    switch(cut,

        static = {
            if (!is.null(h)){
                out <- stats::cutree(x, h=h)
            } else {
                out <- stats::cutree(x, k=k)
            }
        },
        dynamic = {
            y <- x
            attributes(y)[['text_data_store']] <- NULL
            class(y) <- 'hclust'
            out <- dynamicTreeCut::cutreeDynamic(
                dendro = y,
                cutHeight = NULL,
                minClusterSize = minClusterSize,
                method = "tree",
                deepSplit = deepSplit,
                ...
            )
            names(out) <- y[['labels']]
        },
        iterative = {
            stop("'iterative', method not implemented yet;\n Use \'static\' or \'dynamic\'")
        },
        stop('`cut` must be one of: \'static\', \'dynamic\' or \'iterative\'')
    )

    orig <- attributes(x)[['text_data_store']][['data']]
    lens <- length(orig[['text']]) + length(orig[['removed']])

    class(out) <- c("assign_cluster_hierarchical", "assign_cluster", class(out))

    attributes(out)[["data_store"]] <- attributes(x)[["text_data_store"]]
    attributes(out)[["model"]] <- x
    attributes(out)[["algorithm"]] <- 'hierarchical'
    vect <- c(out)
    attributes(out)[["join"]] <- function(x) {

        if (nrow(x) != lens) warning(sprintf("original data had %s elements, `x` has %s", lens, nrow(x)))

        dplyr::select(
            dplyr::left_join(
                dplyr::mutate(x, id_temporary = as.character(1:n())),
                dplyr::tbl_df(textshape::tidy_vector(vect, 'id_temporary', 'cluster') ),
                by = 'id_temporary'
            ),
            -id_temporary
        )
    }
    out

}


#' @export
#' @rdname assign_cluster
#' @method assign_cluster kmeans_cluster
assign_cluster.kmeans_cluster <- function(x, ...){

    out <- x[['cluster']]
    n <- id_temporary <- NULL
    orig <- attributes(x)[['text_data_store']][['data']]
    lens <- length(orig[['text']]) + length(orig[['removed']])

    class(out) <- c("assign_cluster_kmeans","assign_cluster", class(out))

    attributes(out)[["data_store"]] <- attributes(x)[["text_data_store"]]
    attributes(out)[["model"]] <- x
    attributes(out)[["algorithm"]] <- 'kmeans'
    vect <- c(out)
    attributes(out)[["join"]] <- function(x) {

        if (nrow(x) != lens) warning(sprintf("original data had %s elements, `x` has %s", lens, nrow(x)))

        dplyr::select(
            dplyr::left_join(
                dplyr::mutate(x, id_temporary = as.character(1:n())),
                dplyr::tbl_df(textshape::tidy_vector(vect, 'id_temporary', 'cluster') )
            ),
            -id_temporary
        )
    }
    out

}



#' @export
#' @rdname assign_cluster
#' @method assign_cluster skmeans_cluster
assign_cluster.skmeans_cluster <- function(x, ...){

    out <- x[['cluster']]
    n <- id_temporary <- NULL
    orig <- attributes(x)[['text_data_store']][['data']]
    lens <- length(orig[['text']]) + length(orig[['removed']])

    class(out) <- c("assign_cluster_skmeans","assign_cluster", class(out))

    attributes(out)[["data_store"]] <- attributes(x)[["text_data_store"]]
    attributes(out)[["model"]] <- x
    attributes(out)[["algorithm"]] <- 'skmeans'
    vect <- c(out)
    attributes(out)[["join"]] <- function(x) {

        if (nrow(x) != lens) warning(sprintf("original data had %s elements, `x` has %s", lens, nrow(x)))

        dplyr::select(
            dplyr::left_join(
                dplyr::mutate(x, id_temporary = as.character(1:n())),
                dplyr::tbl_df(textshape::tidy_vector(vect, 'id_temporary', 'cluster') )
            ),
            -id_temporary
        )
    }
    out

}


#' @export
#' @rdname assign_cluster
#' @method assign_cluster nmf_cluster
assign_cluster.nmf_cluster <- function(x, ...){

    out <- unlist(apply(x[['W']], 1, which.max))

    n <- id_temporary <- NULL
    orig <- attributes(x)[['text_data_store']][['data']]
    lens <- length(orig[['text']]) + length(orig[['removed']])

    class(out) <- c("assign_cluster_nmf","assign_cluster", class(out))

    attributes(out)[["data_store"]] <- attributes(x)[["text_data_store"]]
    attributes(out)[["model"]] <- x
    attributes(out)[["algorithm"]] <- 'nmf'
    vect <- c(out)
    attributes(out)[["join"]] <- function(x) {

        if (nrow(x) != lens) warning(sprintf("original data had %s elements, `x` has %s", lens, nrow(x)))

        dplyr::select(
            dplyr::left_join(
                dplyr::mutate(x, id_temporary = as.character(1:n())),
                dplyr::tbl_df(textshape::tidy_vector(vect, 'id_temporary', 'cluster') )
            ),
            -id_temporary
        )
    }
    out

}


#' Prints an assign_cluster Object
#'
#' Prints an assign_cluster object
#'
#' @param x An assign_cluster object.
#' @param \ldots ignored.
#' @method print assign_cluster
#' @export
print.assign_cluster <- function(x, ...){
    print(stats::setNames(as.integer(x), names(x)))
}


#' Summary of an assign_cluster Object
#'
#' Summary of an assign_cluster object
#'
#' @param object An assign_cluster object.
#' @param plot logical.  If \code{TRUE} an accompanying bar plot is produced a
#' well.
#' @param print logical.  If \code{TRUE} data.frame counts are printed.
#' @param \ldots ignored.
#' @method summary assign_cluster
#' @export
summary.assign_cluster <- function(object, plot = TRUE, print = TRUE, ...){
    count <- NULL
    out <- textshape::tidy_table(table(as.integer(object)), "cluster", "count")
    if (isTRUE(plot)) print(termco::plot_counts(as.integer(object), item.name = "Cluster"))
    if (isTRUE(print)) dplyr::arrange(as.data.frame(out), dplyr::desc(count))
}





