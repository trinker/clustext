#' Write/Read Cluster Text for Human Categorization
#' 
#' Write cluster text from \code{get_text(assign_cluster(myfit))} to an external 
#' file for categorization.  After file has been written with 
#' \code{write_cluster_text} a human coder can assign categories to each cluster.
#' Simple write the category after the \code{Cluster #:}.  To set a cluster category 
#' equal to another simply write and equal sign follwed by the other cluster to set
#' as the same category (e.g., \code{Cluster 10: =5} to set cluster #10 the same as 
#' cluster #5).  See \code{readLines(system.file("additional/foo_turk.txt", package = "clustext"))} 
#' for an example.
#' 
#' @param x An \code{assign_cluster} object.
#' @param path A pather to the file (.txt) is recommended.
#' @param n.sample The length to limit the sample to (default gives all text in the cluster).  
#' Setting this to an integer uses this as the number to randomly sample from.
#' @param lead A leading character string prefix to give the cluster text.
#' @param \ldots ignored.
#' @rdname write_cluster_text
#' @export
#' @seealso \code{\link[clustext]{categorize}}
#' @examples 
#' library(dplyr)
#' 
#' ## Assign Clusters
#' ca <- presidential_debates_2012 %>%
#'     with(data_store(dialogue)) %>%
#'     hierarchical_cluster() %>%
#'     assign_cluster(k = 7)
#' 
#' ## Write Cluster Text for Human Categorization
#' write_cluster_text(ca)
#' write_cluster_text(ca, n.sample=10)
#' write_cluster_text(ca, lead="  -", n.sample=10)
#' 
#' ## Read Human Coded Categories Back In
#' categories_file <- system.file("additional/foo_turk.txt", package = "clustext")
#' readLines(categories_file)
#' (categories_key <- read_cluster_text(categories_file))
#' 
#' ## Add Categories Back to Original Data Set
#' categorize(
#'     data = presidential_debates_2012,
#'     assign.cluster = ca,
#'     cluster.key = categories_key
#' )
write_cluster_text <- function(x, path, n.sample = NULL, lead = " * ", ...){ 
    
    stopifnot(methods::is(x, 'assign_cluster'))
    if (missing(path)) path <- ""
    
    
    y <- get_text(x)
    lens <- paste0("n = ", pn2(unlist(lapply(y, length))))
    
    # get sample of text in each cluster
    if (!is.null(n.sample)){
        y <- lapply(y, function(x) sample(x, min(length(x), n.sample)))
    }
    
    cls <- paste0("Cluster ", seq_along(y), ":")
    brd <- sapply(nchar(cls), function(x) paste(rep("=", x), collapse=""))
    
    if (!is.null(n.sample)){
        cls <- paste(cls, lens, sep = "\n")
    }
    
    cat(
        gsub("^\\s+", "", 
             paste(
                 paste(
                     paste("\n", brd, cls, brd, sep="\n"),
                     sapply(lapply(y, function(z) paste0(lead, z)), paste, collapse="\n"), sep="\n"
                 ),
                 collapse="\n\n\n"
             )
        ), "\n",
        file=path
    )
}


#' @rdname write_cluster_text
#' @export
read_cluster_text <- function(path, ...){
    
    x <- suppressWarnings(readLines(path))
    y <- sub("\\s*:\\s*", "splitherenow", gsub("Cluster\\s*", "",  grep("^Cluster", x, value=TRUE)))
    content <- lapply(strsplit(y, "splitherenow"), function(x){
        out <- trimws(x)
        if (length(out) == 1) out <- c(out, NA)
        out
    })
    
    key <- stats::setNames(data.frame(do.call(rbind, content), stringsAsFactors = FALSE), c('cluster', 'category'))
    
    repl <- sub("^=", "", grep("=\\d+", key[['category']], value=TRUE))
    if (length(repl) > 0){
        key[['category']][grep("=\\d+", key[['category']])] <- key[match(repl, key[["cluster"]]), "category"]
    }
    key[["cluster"]] <- as.integer(key[["cluster"]])
    class(key) <- c('cluster_key', class(key))
    key
}










