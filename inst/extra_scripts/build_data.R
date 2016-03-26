pacman::p_load(clustext, dplyr)

x <- presidential_debates_2012 %>%
    mutate(tot = gsub("\\..+$", "", tot)) %>%
    textshape::combine() %>%
    filter(person %in% c("ROMNEY", "OBAMA")) %>%
    with(data_store(dialogue, stopwords = tm::stopwords("english"), min.char = 3))

set.seed(10)
kmeans_assignment  <- kmeans_cluster(x, 50) %>%
    assign_cluster(myfit2)

set.seed(10)
nmf_assignment  <- nmf_cluster(x, 50) %>%
    assign_cluster(myfit2)

set.seed(10)
skmeans_assignment  <- skmeans_cluster(x, 50) %>%
    assign_cluster(myfit2)

hierarchical_assignment <- hierarchical_cluster(x) %>%
    assign_cluster(k=50)

assignments <- list(
    hierarchical_assignment = hierarchical_assignment,
    kmeans_assignment = kmeans_assignment,
    skmeans_assignment = skmeans_assignment,
    nmf_assignment =nmf_assignment
)


assignments <- lapply(assignments, function(x) {
    attributes(x)[['data_store']] <- NULL
    attributes(x)[['model']] <- NULL
    attributes(x)[['join']] <- NULL
    x
})

lapply(assignments, function(x) {names(attributes(x))})

pax::new_data(assignments)
