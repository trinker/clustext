#' Get Documents Based on Cluster Assignment in \code{assign_cluster}
#'
#' Get the documents associated with each of the k clusters .
#'
#' @param x A \code{\link[clustext]{assign_cluster}} object.
#' @param \ldots ignored.
#' @return Returns a list of \code{\link[base]{vector}}s of document names.
#' @export
#' @rdname get_documents
#' @examples
#' library(dplyr)
#'
#' mydocuments1 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue, paste(person, time, sep="-"))) %>%
#'     hierarchical_cluster() %>%
#'     assign_cluster(k = 6) %>%
#'     get_documents()
#'
#' mydocuments1
#'
#' mydocuments2 <- presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     hierarchical_cluster() %>%
#'     assign_cluster(k = 55) %>%
#'     get_documents()
#'
#' mydocuments2
get_documents <- function(x, ...){
    UseMethod("get_documents")
}

#' @export
#' @rdname get_documents
#' @method get_documents assign_cluster
get_documents.assign_cluster <- function(x, ...){

    desc <- topic <- n <- NULL
    out <- split(attributes(x)[["names"]], x)

    class(out) <- c("get_documents", class(out))
    out
}

#' Prints a get_documents Object
#'
#' Prints a get_documents object
#'
#' @param x A get_documents object.
#' @param \ldots ignored.
#' @method print get_documents
#' @export
print.get_documents <- function(x, ...){
    class(x) <- "list"
    print(x)
}
