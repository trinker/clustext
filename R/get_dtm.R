#' Get a \code{\link[tm]{DocumentTermMatrix}} Stored in a \code{hierarchical_cluster} Object
#'
#' Extract the \code{\link[tm]{DocumentTermMatrix}} supplied to/produced by a
#' \code{\link[hclustext]{hierarchical_cluster}} object.
#'
#' @param x A \code{\link[hclustext]{hierarchical_cluster}} object.
#' @param \ldots ignored.
#' @return Returns a \code{\link[tm]{DocumentTermMatrix}}.
#' @export
#' @rdname get_dtm
#' @examples
#' library(dplyr)
#'
#' presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     hierarchical_cluster() %>%
#'     get_dtm()
get_dtm <- function(x, ...){
    UseMethod("get_dtm")
}

#' @export
#' @rdname get_dtm
#' @method get_dtm data_store
get_dtm.data_store <- function(x, ...){
    x[["dtm"]]
}


#' @export
#' @rdname get_dtm
#' @method get_dtm hierarchical_cluster
get_dtm.hierarchical_cluster <- function(x, ...){
    get_dtm(attributes(x)[["text_data_store"]][["data"]])
}


#' @export
#' @rdname get_dtm
#' @method get_dtm kmeans_cluster
get_dtm.kmeans_cluster <- function(x, ...){
    get_dtm(attributes(x)[["text_data_store"]][["data"]])
}

#' @export
#' @rdname get_dtm
#' @method get_dtm skmeans_cluster
get_dtm.skmeans_cluster <- function(x, ...){
    get_dtm(attributes(x)[["text_data_store"]][["data"]])
}



#' @export
#' @rdname get_dtm
#' @method get_dtm nmf_cluster
get_dtm.nmf_cluster <- function(x, ...){
    get_dtm(attributes(x)[["text_data_store"]][["data"]])
}



