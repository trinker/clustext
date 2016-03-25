#' Data Structure for \pkg{hclusttext}
#'
#' A data structure which stores the text, DocumentTermMatrix, and information
#' regarding removed text elements which can not be handled by the
#' \code{hierarchical_cluster} function.  This structure is required because it
#' documents important meta information, including removed elements, required by
#' other \pkg{hclustext} functions.  If the user wishes to combine documents
#' (say by a common grouping variable) it is recomended this be handled by
#' \code{\link[textshape]{combine}} prior to using \code{data_store}.
#'
#' @param text A character vector.
#' @param doc.names An optional vector of document names corresponding to the
#' length of \code{text}.
#' @param min.term.freq The minimum times a term must appear to be included in
#' the \code{\link[tm]{DocumentTermMatrix}}.
#' @param min.doc.len The minimum words a document must contain to be included
#' in the data structure (other wise it is stored as a \code{removed} element).
#' @param stopwords A vector of stopwords to remove.
#' @param min.char The minial length character for retained words.
#' @param max.char The maximum length character for retained words.
#' @param stem Logical.  If \code{TRUE} the \code{stopwords} will be stemmed.
#' @param denumber Logical.  If \code{TRUE} numbers will be excluded.
#' @return Returns a list containing:
#' \describe{
#'   \item{dtm}{A tf-idf weighted \code{\link[tm]{DocumentTermMatrix}}}
#'   \item{text}{The text vector with unanalyzable elements removed}
#'   \item{removed}{The indices of the removed text elements, i.e., documents not meeting \code{min.doc.len}}
#'   \item{n.nonsparse}{The length of the non-zero elements}
#' }
#' @keywords data structure
#' @export
#' @examples
#' data_store(presidential_debates_2012[["dialogue"]])
#'
#' ## Use `combine` to merge text prior to `data_stare`
#' library(textshape)
#' library(dplyr)
#'
#' dat <- presidential_debates_2012 %>%
#'     dplyr::select(person, time, dialogue) %>%
#'     textshape::combine()
#'
#' ## Elements in `ds` correspond to `dat` grouping vars
#' (ds <- with(dat, data_store(dialogue)))
#' dplyr::select(dat, -3)
#'
#' ## Add row names
#' (ds2 <- with(dat, data_store(dialogue, paste(person, time, sep = "_"))))
#' rownames(ds2[["dtm"]])
#'
#' ## Get a DocumentTermMatrix
#' get_dtm(ds2)
data_store <- function(text, doc.names, min.term.freq = 1, min.doc.len = 1,
    stopwords = tm::stopwords("english"), min.char = 3, max.char = NULL,
    stem = FALSE, denumber = TRUE){

    stopifnot(is.atomic(text))
    if (missing(doc.names)) doc.names <- seq_len(length(text))
    stopifnot(length(text) == length(doc.names))

    if (isTRUE(stem)){
        dtm <- gofastr::q_dtm_stem(text, docs = doc.names)
    } else {
        dtm <- gofastr::q_dtm(text, docs = doc.names)
    }

    dtm <- gofastr::remove_stopwords(dtm, stopwords = stopwords, min.char = min.char,
        max.char = max.char, stem = stem, denumber = denumber)

    if (nrow(dtm) != length(text)){
        text <- dplyr::group_by_(dplyr::data_frame(text=text, doc.names=doc.names), "doc.names")
        text <- dplyr::summarise(text, text = paste(text, collapse = " "))
        text <- text[match(text[["doc.names"]], rownames(dtm)),][["text"]]
    }

    names(text) <- text_seq <- seq_len(length(text))

    # remove terms
    dtm <- dtm[, slam::col_sums(dtm) >= min.term.freq]

    # remove short docs
    long_docs <- slam::row_sums(dtm) >= min.doc.len
    text <- text[long_docs]
    dtm <- dtm[long_docs,]

    # remove terms/docs again (ensure no zero lengths)
    # Eventually determine which elements were kept removed
    dtm <- dtm[, slam::col_sums(dtm) > 0]

    long_docs <- slam::row_sums(dtm) > 0
    text <- text[long_docs]
    dtm <- dtm[long_docs,]

    ## Add tf-idf
    dtm <- tm::weightTfIdf(dtm)

    out <- list(dtm = dtm, text = unname(text),
        removed = setdiff(text_seq, names(text)), n.nonsparse = length(dtm[["v"]]))

    class(out) <- "data_store"
    attributes(out)[["guid"]] <-rguid()
    out
}


#' Prints a data_store Object
#'
#' Prints a data_store object
#'
#' @param x A data_store object.
#' @param \ldots ignored.
#' @method print data_store
#' @export
print.data_store <- function(x, ...){
    cat(sprintf("<<Data Store (documents: %s, terms: %s)>>\n", pn2(nrow(x[["dtm"]])), pn2(ncol(x[["dtm"]]))  ))
    cat(sprintf("Text Elements      : %s\n", pn2(length(x[["text"]]))  ))
    cat(sprintf("Elements Removed   : %s\n", pn2(length(x[["removed"]])) ))
    #cat(sprintf("Documents          : %s\n", pn2(nrow(x[["dtm"]]))  ))
    #cat(sprintf("Terms              : %s\n", pn2(ncol(x[["dtm"]]))  ))
    cat(sprintf("Non-/sparse entries: %d/%.0f\n", x[["n.nonsparse"]],
        prod(dim(x[["dtm"]])) - x[["n.nonsparse"]]))
    if (!prod(dim(x))) {
        sparsity <- 100
    } else {
        sparsity <- round((1 - x[["n.nonsparse"]]/prod(dim(x[["dtm"]]))) * 100)
    }
    cat(sprintf("Sparsity           : %s%%\n", sparsity))
    cat(sprintf("Maximal term length: %s\n", max(nchar(colnames(x[["dtm"]])))))
    cat(sprintf("Minimum term length: %s\n", min(nchar(colnames(x[["dtm"]])))))
}



#
# data_store_map <- function(stopwords = tm::stopwords("english"), min.char = 3,
#     max.char = NULL, stem = FALSE, denumber = TRUE) {
#
# }
