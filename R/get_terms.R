#' Get Terms Based on Cluster Assignment in \code{assign_cluster}
#'
#' Get the terms weighted (either by tf-idf or returned from the model) and
#' min/max scaling associated with each of the k clusters .
#'
#' @param x A \code{\link[hclustext]{assign_cluster}} object.
#' @param min.weight The lowest min/max scaled tf-idf weighting to consider
#' as a document's salient term.
#' @param nrow The max number of rows to display in the returned
#' \code{\link[base]{data.frame}}s.
#' @param \ldots ignored.
#' @return Returns a list of \code{\link[base]{data.frame}}s of top weighted terms.
#' @export
#' @rdname get_terms
#' @examples
#' library(dplyr)
#' library(textshape)
#'
#' myterms <- presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     hierarchical_cluster() %>%
#'     assign_cluster(k = 55) %>%
#'     get_terms()
#'
#' myterms
#' textshape::bind_list(myterms[!sapply(myterms, is.null)], "Topic")
#' \dontrun{
#' library(ggplot2)
#' library(gridExtra)
#' library(dplyr)
#' library(textshape)
#' library(wordcloud)
#'
#' max.n <- max(textshape::bind_list(myterms)[["n"]])
#'
#' myplots <- Map(function(x, y){
#'     x %>%
#'         mutate(term = factor(term, levels = rev(term))) %>%
#'         ggplot(aes(term, weight=n)) +
#'             geom_bar() +
#'             scale_y_continuous(expand = c(0, 0),limits=c(0, max.n)) +
#'             ggtitle(sprintf("Topic: %s", y)) +
#'             coord_flip()
#' }, myterms, names(myterms))
#'
#' myplots[["ncol"]] <- 10
#'
#' do.call(gridExtra::grid.arrange, myplots[!sapply(myplots, is.null)])
#'
#' ##wordclouds
#' par(mfrow=c(5, 11), mar=c(0, 4, 0, 0))
#' Map(function(x, y){
#'     wordcloud::wordcloud(x[[1]], x[[2]], scale=c(1,.25),min.freq=1)
#'     mtext(sprintf("Topic: %s", y), col = "blue", cex=.55, padj = 1.5)
#' }, myterms, names(myterms))
#' }
get_terms <- function(x, min.weight = .6, nrow = NULL, ...){
    UseMethod("get_terms")
}



#' @export
#' @rdname get_terms
#' @method get_terms assign_cluster_hierarchical
get_terms.assign_cluster_hierarchical <- function(x, min.weight = .6, nrow = NULL, ...){

    assignment <- x
    desc <- topic <- n <- term <- weight <- NULL
    dat <- attributes(x)[["data_store"]][["data"]][['dtm']]
    clusters <-split(names(x), x)

    out <- stats::setNames(lapply(clusters, function(y){
        vals <- min_max(sort(slam::col_sums(dat[y,]), decreasing=TRUE))
        as.data.frame(textshape::bind_vector(vals, 'term', 'weight'), stringsAsFactors = FALSE)
    }), names(clusters))


    out2 <- lapply(out, function(x) {
        rownames(x) <- NULL
        x <- dplyr::filter(x, weight >= min.weight)
        if (!is.null(nrow)) {
            x <- x[1:nrow, ]
        }
        x <- dplyr::filter(x, !is.na(term))

        if (nrow(x) == 0) return(NULL)
        x
    })

    if (!is.null(nrow)) {
        out2 <- lapply(out2, function(x){
            if (is.null(x) || nrow(x) <= nrow) return(x)
            x[1:nrow]
        })
    }
    class(out2) <- c("get_terms", class(out2))
    attributes(out2)[['assignment']] <- assignment
    out2

}






#' @export
#' @rdname get_terms
#' @method get_terms assign_cluster_kmeans
get_terms.assign_cluster_kmeans <- function(x, min.weight = .6, nrow = NULL, ...){


    weight <- term <- NULL

    assignment <- x
    x <- attributes(x)[['model']]
    nms <- seq_along(x[['size']])
    x <- x[['centers']]

    out <- stats::setNames(lapply(1:nrow(x), function(i){
        vals <- min_max(sort(x[i, ], decreasing=TRUE))
        as.data.frame(textshape::bind_vector(vals, 'term', 'weight'), stringsAsFactors = FALSE)
    }), nms)

    out2 <- lapply(out, function(x) {
        rownames(x) <- NULL
        x <- dplyr::filter(x, weight >= min.weight)
        if (!is.null(nrow)) {
            x <- x[1:nrow, ]
        }
        x <- dplyr::filter(x, !is.na(term))

        if (nrow(x) == 0) return(NULL)
        x
    })

    if (!is.null(nrow)) {
        out2 <- lapply(out2, function(x){
            if (is.null(x) || nrow(x) <= nrow) return(x)
            x[1:nrow]
        })
    }
    class(out2) <- c("get_terms", class(out2))
    attributes(out2)[['assignment']] <- assignment
    out2

}
#' Prints a get_terms Object
#'
#' Prints a get_terms object
#'
#' @param x A get_terms object.
#' @param \ldots ignored.
#' @method print get_terms
#' @export
print.get_terms <- function(x, ...){

    lens <- unlist(lapply(split(names(attributes(x)[['assignment']]), attributes(x)[['assignment']]), length))
    attributes(x)[['assignment']] <- NULL

    class(x) <- "list"
    names(x) <- sprintf("%s (n=%s)", names(x), lens)
    print(x)
}





