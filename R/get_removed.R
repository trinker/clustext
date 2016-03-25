#' Get a Text Stored in a \code{hierarchical_cluster} Object
#'
#' Extract the text supplied to the
#' \code{\link[hclustext]{hierarchical_cluster}} object.
#'
#' @param x A \code{\link[hclustext]{hierarchical_cluster}} object.
#' @param \ldots ignored.
#' @return Returns a vector of text strings.
#' @export
#' @rdname get_removed
#' @examples
#' library(dplyr)
#'
#' presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     hierarchical_cluster() %>%
#'     get_removed()
get_removed <- function(x, ...){
    UseMethod("get_removed")
}

#' @export
#' @rdname get_removed
#' @method get_removed hierarchical_cluster
get_removed.hierarchical_cluster <- function(x, ...){
    get_removed(attributes(x)[["text_data_store"]][["data"]])
}

#' @export
#' @rdname get_removed
#' @method get_removed kmeans_cluster
get_removed.kmeans_cluster <- function(x, ...){
    get_removed(attributes(x)[["text_data_store"]][["data"]])
}



#' @export
#' @rdname get_removed
#' @method get_removed data_store
get_removed.data_store <- function(x, ...){
    x[["removed"]]
}


