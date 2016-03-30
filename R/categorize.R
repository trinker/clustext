#' Merge Clusters & Cluster Categories Back to Original Data
#' 
#' Merge clusters, categories, and the original data back together.
#' 
#' @param data A data set that was fit with a cluster model.
#' @param assign.cluster An \code{\link[clustext]{assign_cluster}} object.
#' @param cluster.key An \code{\link[clustext]{assign_cluster}} object.
#' @return Returns a \code{\link[base]{data.frame}} key of clusters and categories.
#' @export
#' @seealso \code{\link[clustext]{write_cluster_text}},
#' \code{\link[clustext]{read_cluster_text}}
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
categorize <- function(data, assign.cluster, cluster.key) {
    
    stopifnot(methods::is(assign.cluster, 'assign_cluster'))
    stopifnot(methods::is(cluster.key, 'cluster_key'))
    
    data <- attributes(assign.cluster)[["join"]](data)
    dplyr::left_join(data, cluster.key, by = 'cluster')
}