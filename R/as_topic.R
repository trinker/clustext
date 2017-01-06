#' Convert \code{get_terms} to Topics
#'
#' View important terms as a comma separated string (a topic).
#'
#' @param x A \code{get_terms} object.
#' @param max.n The max number of words to show before truncation.
#' @param sort logical.  If \code{TRUE} the cluster topics are sorted by size
#' (number of documents) otherwise the topics are sorted by cluster number.
#' @param \ldots ignored.
#' @return Returns a \code{\link[base]{data.frame}} of \code{"cluster"},
#' \code{"count"}, and \code{"terms"}.  Pretty prints as clusters, number of
#' documents, and associated important terms.
#' @export
#' @examples
#' library(dplyr)
#'
#' myfit5 <- presidential_debates_2012 %>%
#'     mutate(tot = gsub("\\..+$", "", tot)) %>%
#'     textshape::combine() %>%
#'     filter(person %in% c("ROMNEY", "OBAMA")) %>%
#'     with(data_store(dialogue, stopwords = tm::stopwords("english"), min.char = 3)) %>%
#'     hierarchical_cluster()
#'
#' ca5 <- assign_cluster(myfit5, k = 50)
#'
#' get_terms(ca5, .4) %>%
#'     as_topic()
#'
#' get_terms(ca5, .4) %>%
#'     as_topic(sort=FALSE)
#'
#' get_terms(ca5, .95) %>%
#'     as_topic()
as_topic <- function(x, max.n = 8, sort = TRUE, ...){
    UseMethod("as_topic")
}


#' @export
#' @rdname as_topic
#' @method as_topic get_terms
as_topic.get_terms <- function(x, max.n = 8, sort = TRUE, ...){

    cluster <- NULL

    terms <- lapply(x, function(x) {
        if (is.null(x)) return(NA)
        trms <- x[['term']]
        if (length(trms) > max.n) {
            paste0(paste(trms[1:max.n], collapse= ", "), "...")
        } else {
            paste(trms, collapse= ", ")
        }
    })

    dat <- dplyr::left_join(
        summary(attributes(x)[["assignment"]], plot=FALSE),
        textshape::tidy_list(terms, "cluster", "terms"),
        by = 'cluster'
    )
    if (!isTRUE(sort)){
        dat <- dplyr::arrange(dat, as.numeric(cluster))
    }
    class(dat) <- c('as_topic', class(dat))
    dat
}


#' Prints an as_topic Object
#'
#' Prints an as_topic object
#'
#' @param x An as_topic object.
#' @param \ldots ignored.
#' @method print as_topic
#' @export
print.as_topic <- function(x, ...){
    cat(paste(sprintf("Cluster %s (n=%s): %s", x[['cluster']], x[['count']],
        x[['terms']]), collapse="\n"), "\n")
}




