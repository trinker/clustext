clustext   [![Follow](https://img.shields.io/twitter/follow/tylerrinker.svg?style=social)](https://twitter.com/intent/follow?screen_name=tylerrinker)
============


[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build
Status](https://travis-ci.org/trinker/clustext.svg?branch=master)](https://travis-ci.org/trinker/clustext)
[![Coverage
Status](https://coveralls.io/repos/trinker/clustext/badge.svg?branch=master)](https://coveralls.io/r/trinker/clustext?branch=master)
<a href="https://img.shields.io/badge/Version-0.0.1-orange.svg"><img src="https://img.shields.io/badge/Version-0.0.1-orange.svg" alt="Version"/></a>
</p>
<img src="inst/clustext_logo/r_clustext.png" width="150" alt="readability Logo">

**clustext** is a collection of optimized tools for clustering text data
via various text appropriate clustering algorithms. There are many great
R [clustering tools](https://cran.r-project.org/web/views/Cluster.html)
to locate topics within documents. I have had success with hierarchical
clustering for topic extraction. This initial success birthed the
[**hclustext**](https://github.com/trinker/hclustext) package.
Additional techniques such as kmeans and non-negative matrix
factorization also proved useful. These algorithms began to be collected
in a consistent manor of use in the **clustext** package. This package
wraps many of the great R tools for clustering and working with sparse
matrices to aide in the workflow associated with topic extraction.

The general idea is that we turn the documents into a matrix of words.
After this we weight the terms by importance using
[tf-idf](http://nlp.stanford.edu/IR-book/html/htmledition/tf-idf-weighting-1.html).
This helps the more salient words to rise to the top. Some clustering
algorithms require a similarity matrix while others require just the
tf-idf weighted DocumentTermMatrices. Likewise, some algorithms require
`k` terms to be specified before the model fit while others allow `k`
topics to be determined after the model has been fit.

With algorithms that require a similarity matrix (e.g., hierarchical
clustering) we apply cosine distance measures to compare the terms (or
features) of each document. I have found cosine distance to work well
with sparse matrices to produce distances metrics between the documents.
The clustering model is fit to separate the documents into clusters. In
the case of some clustering techniques (e.g., hierarchical clustering)
the user then may apply k clusters to the fit, clustering documents with
similar important text features. Other techniques require that `k` be
specified prior to fitting the model. The documents can then be grouped
by clusters and their accompanying salient words extracted as well.


Table of Contents
============

-   [Functions](#functions)
-   [Installation](#installation)
-   [Contact](#contact)
-   [Demonstration](#demonstration)
    -   [Load Packages and Data](#load-packages-and-data)
    -   [Data Structure](#data-structure)
    -   [Fit the Model: Hierarchical Cluster](#fit-the-model-hierarchical-cluster)
    -   [Assigning Clusters](#assigning-clusters)
        -   [Cluster Loading](#cluster-loading)
        -   [Cluster Text](#cluster-text)
        -   [Cluster Frequent Terms](#cluster-frequent-terms)
        -   [Clusters, Terms, and Docs Plot](#clusters-terms-and-docs-plot)
        -   [Cluster Documents](#cluster-documents)
    -   [Putting it Together](#putting-it-together)
    -   [An Experiment](#an-experiment)

Functions
============


The main functions, task category, & descriptions are summarized in the
table below:

<table style="width:161%;">
<colgroup>
<col width="34%" />
<col width="23%" />
<col width="102%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Function</th>
<th align="left">Category</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>data_store</code></td>
<td align="left">data structure</td>
<td align="left"><strong>clustext</strong>'s data structure (list of dtm + text)</td>
</tr>
<tr class="even">
<td align="left"><code>hierarchical_cluster</code></td>
<td align="left">cluster fit</td>
<td align="left">Fits a hierarchical cluster model</td>
</tr>
<tr class="odd">
<td align="left"><code>kmeans_cluster</code></td>
<td align="left">cluster fit</td>
<td align="left">Fits a kmeans cluster model</td>
</tr>
<tr class="even">
<td align="left"><code>skmeans_cluster</code></td>
<td align="left">cluster fit</td>
<td align="left">Fits an skmeans cluster model</td>
</tr>
<tr class="odd">
<td align="left"><code>nfm_cluster</code></td>
<td align="left">cluster fit</td>
<td align="left">Fits a non-negative matrix factorization cluster model</td>
</tr>
<tr class="even">
<td align="left"><code>assign_cluster</code></td>
<td align="left">assignment</td>
<td align="left">Assigns cluster to document/text element</td>
</tr>
<tr class="odd">
<td align="left"><code>get_text</code></td>
<td align="left">extraction</td>
<td align="left">Get text from various <strong>clustext</strong> objects</td>
</tr>
<tr class="even">
<td align="left"><code>get_dtm</code></td>
<td align="left">extraction</td>
<td align="left">Get <code>tm::DocumentTermMatrix</code> from various <strong>clustext</strong> objects</td>
</tr>
<tr class="odd">
<td align="left"><code>get_removed</code></td>
<td align="left">extraction</td>
<td align="left">Get removed text elements from various <strong>clustext</strong> objects</td>
</tr>
<tr class="even">
<td align="left"><code>get_terms</code></td>
<td align="left">extraction</td>
<td align="left">Get clustered weighted important terms from an <strong>assign_cluster</strong> object</td>
</tr>
<tr class="odd">
<td align="left"><code>get_documents</code></td>
<td align="left">extraction</td>
<td align="left">Get clustered documents from an <strong>assign_cluster</strong> object</td>
</tr>
</tbody>
</table>

Installation
============

To download the development version of **clustext**:

Download the [zip
ball](https://github.com/trinker/clustext/zipball/master) or [tar
ball](https://github.com/trinker/clustext/tarball/master), decompress
and run `R CMD INSTALL` on it, or use the **pacman** package to install
the development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh(
        "trinker/textshape", 
        "trinker/gofastr", 
        "trinker/termco",    
        "trinker/clustext"
    )

Contact
=======

You are welcome to:    
- submit suggestions and bug-reports at: <https://github.com/trinker/clustext/issues>    
- send a pull request on: <https://github.com/trinker/clustext/>    
- compose a friendly e-mail to: <tyler.rinker@gmail.com>    

Demonstration
=============

Load Packages and Data
----------------------

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(clustext, dplyr, textshape, ggplot2, tidyr)

    data(presidential_debates_2012)

Data Structure
--------------

The data structure for **clustext** is very specific. The `data_storage`
produces a `DocumentTermMatrix` which maps to the original text. The
empty/removed documents are tracked within this data structure, making
subsequent calls to cluster the original documents and produce weighted
important terms more robust. Making the `data_storage` object is the
first step to analysis.

We can give the `DocumentTermMatrix` rownames via the `doc.names`
argument. If these names are not unique they will be combined into a
single document as seen below. Also, if you want to do stemming, minimum
character length, stopword removal or such this is when/where it's done.

    ds <- with(
        presidential_debates_2012,
        data_store(dialogue, doc.names = paste(person, time, sep = "_"))
    )

    ds

    ## <<Data Store (documents: 10, terms: 3,180)>>
    ## Text Elements      : 10
    ## Elements Removed   : 0
    ## Non-/sparse entries: 6916/24884
    ## Sparsity           : 78%
    ## Maximal term length: 16
    ## Minimum term length: 3

Fit the Model: Hierarchical Cluster
-----------------------------------

Next we can fit a hierarchical cluster model to the `data_store` object
via `hierarchical_cluster`.

    myfit <- hierarchical_cluster(ds)

    myfit

    ## 
    ## Call:
    ## fastcluster::hclust(d = cosine_distance(x[["dtm"]]), method = method)
    ## 
    ## Cluster method   : ward.D 
    ## Number of objects: 10

This object can be plotted with various `k` or `h` parameters specified
to experiment with cutting the dendrogram. This cut will determine the
number of clusters or topics that will be generated in the next step.
The visual inspection allows for determining how to cluster the data as
well as determining if a tf-idf, cosine, hierarchical cluster model is a
right fit for the data and task. By default `plot` uses an approximation
of `k` based on Can & Ozkarahan's (1990) formula (*m* \* *n*)/*t* where
*m* and *n* are the dimensions of the matrix and *t* is the length of
the non-zero elements in matrix *A*.

-   Can, F., Ozkarahan, E. A. (1990). Concepts and effectiveness of the
    cover-coefficient-based clustering methodology for text databases.
    *ACM Transactions on Database Systems 15* (4): 483.
    <doi:10.1145/99935.99938>

Interestingly, in the plots below where `k = 6` clusters, the model
groups each of the candidates together at each of the debate times.

    plot(myfit)

    ## 
    ## k approximated to: 5

![](inst/figure/unnamed-chunk-6-1.png)

    plot(myfit, k=6)

![](inst/figure/unnamed-chunk-6-2.png)

    plot(myfit, h = .75)

![](inst/figure/unnamed-chunk-6-3.png)

Assigning Clusters
------------------

The `assign_cluster` function allows the user to dictate the number of
clusters. Because the model has already been fit the cluster assignment
is merely selecting the branches from the dendrogram, and is thus very
quick. Unlike many clustering techniques the number of clusters is done
after the model is fit, this allows for speedy cluster assignment,
meaning the user can experiment with the number of clusters.

    ca <- assign_cluster(myfit, k = 6)

    ca

    ##   CROWLEY_time 2    LEHRER_time 1     OBAMA_time 1     OBAMA_time 2 
    ##                1                2                3                4 
    ##     OBAMA_time 3  QUESTION_time 2    ROMNEY_time 1    ROMNEY_time 2 
    ##                5                6                3                4 
    ##    ROMNEY_time 3 SCHIEFFER_time 3 
    ##                5                2

### Cluster Loading

To check the number of documents loading on a cluster there is a
`summary` method for `assign_cluster` which provides a descending data
frame of clusters and counts. Additionally, a horizontal bar plot shows
the document loadings on each cluster.

    summary(ca)

![](inst/figure/unnamed-chunk-8-1.png)

    ##   cluster count
    ## 1       2     2
    ## 2       3     2
    ## 3       4     2
    ## 4       5     2
    ## 5       1     1
    ## 6       6     1

### Cluster Text

The user can grab the texts from the original documents grouped by
cluster using the `get_text` function. Here I demo a 40 character
substring of the document texts.

    get_text(ca) %>%
        lapply(substring, 1, 40)

    ## $`1`
    ## [1] "Good evening from Hofstra University in "
    ## 
    ## $`2`
    ## [1] "We'll talk about specifically about heal"
    ## [2] "Good evening from the campus of Lynn Uni"
    ## 
    ## $`3`
    ## [1] "Jim, if I if I can just respond very qui"
    ## [2] "What I support is no change for current "
    ## 
    ## $`4`
    ## [1] "Jeremy, first of all, your future is bri"
    ## [2] "Thank you, Jeremy. I appreciate your you"
    ## 
    ## $`5`
    ## [1] "Well, my first job as commander in chief"
    ## [2] "Thank you, Bob. And thank you for agreei"
    ## 
    ## $`6`
    ## [1] "Mister President, Governor Romney, as a "

### Cluster Frequent Terms

As with many topic clustering techniques, it is useful to get the to
salient terms from the model. The `get_terms` function uses the
[min-max](https://en.wikipedia.org/wiki/Feature_scaling#Rescaling)
scaled, [tf-idf weighted](https://en.wikipedia.org/wiki/Tf%E2%80%93idf),
`DocumentTermMatrix` to extract the most frequent salient terms. These
terms can give a sense of the topic being discussed. Notice the absence
of clusters 1 & 6. This is a result of only a single document included
in each of the clusters. The `term.cutoff` hyperparmeter sets the lower
bound on the min-max scaled tf-idf to accept. If you don't get any terms
you may want to lower this or reduce `min.n`. Likewise, these two
parameters can be raised to eliminate noise.

    get_terms(ca)

    ## $`1 (n=1)`
    ##         term    weight
    ## 1     mister 1.0000000
    ## 2      along 0.7086841
    ## 3       sort 0.6678306
    ## 4 unemployed 0.6223915
    ## 
    ## $`2 (n=2)`
    ##      term    weight
    ## 1 segment 1.0000000
    ## 2 minutes 0.9091730
    ## 3  minute 0.6648988
    ## 
    ## $`3 (n=2)`
    ##        term    weight
    ## 1 insurance 1.0000000
    ## 2    health 0.6200389
    ## 
    ## $`4 (n=2)`
    ##           term    weight
    ## 1         coal 1.0000000
    ## 2         jobs 0.9439400
    ## 3         sure 0.9330092
    ## 4  immigration 0.9134630
    ## 5          oil 0.9014907
    ## 6        issue 0.7352300
    ## 7        candy 0.7303597
    ## 8   production 0.7291683
    ## 9        women 0.7073096
    ## 10     million 0.6792076
    ## 11      settle 0.6056192
    ## 12   illegally 0.6055244
    ## 
    ## $`5 (n=2)`
    ##         term    weight
    ## 1    nuclear 1.0000000
    ## 2       iran 0.9511527
    ## 3  sanctions 0.8585336
    ## 4     israel 0.7895173
    ## 5       sure 0.7698270
    ## 6     region 0.7608304
    ## 7   military 0.7272537
    ## 8     troops 0.6768143
    ## 9   pakistan 0.6766784
    ## 10     world 0.6716568
    ## 11    threat 0.6520238
    ## 12      iraq 0.6467488
    ## 
    ## $`6 (n=1)`
    ##            term    weight
    ## 1    department 1.0000000
    ## 2           chu 0.6666667
    ## 3        stated 0.6666667
    ## 4 misperception 0.6666667

### Clusters, Terms, and Docs Plot

Here I plot the clusters, terms, and documents (grouping variables)
together as a combined heatmap. This can be useful for viewing &
comparing what documents are clustering together in the context of the
cluster's salient terms. This example also shows how to use the cluster
terms as a lookup key to extract probable salient terms for a given
document.

    key <- data_frame(
        cluster = 1:6,
        labs = get_terms(ca) %>%
            bind_list("cluster") %>%
            select(-weight) %>%
            group_by(cluster) %>%
            summarize(term=paste(term, collapse=", ")) %>%
            apply(1, paste, collapse=": ")
    )

    ca %>%
        bind_vector("id", "cluster") %>%
        separate(id, c("person", "time"), sep="_") %>%
        tbl_df() %>%
        left_join(key) %>%
        mutate(n = 1) %>%
        mutate(labs = factor(labs, levels=rev(key[["labs"]]))) %>%
        unite("time_person", time, person, sep="\n") %>%
        select(-cluster) %>%
        complete(time_person, labs) %>%  
        mutate(n = factor(ifelse(is.na(n), FALSE, TRUE))) %>%
        ggplot(aes(time_person, labs, fill = n)) +
            geom_tile() +
            scale_fill_manual(values=c("grey90", "red"), guide=FALSE) +
            labs(x=NULL, y=NULL) 

    ## Joining by: "cluster"

![](inst/figure/unnamed-chunk-11-1.png)

### Cluster Documents

The `get_documents` function grabs the documents associated with a
particular cluster. This is most useful in cases where the number of
documents is small and they have been given names.

    get_documents(ca)

    ## $`1`
    ## [1] "CROWLEY_time 2"
    ## 
    ## $`2`
    ## [1] "LEHRER_time 1"    "SCHIEFFER_time 3"
    ## 
    ## $`3`
    ## [1] "OBAMA_time 1"  "ROMNEY_time 1"
    ## 
    ## $`4`
    ## [1] "OBAMA_time 2"  "ROMNEY_time 2"
    ## 
    ## $`5`
    ## [1] "OBAMA_time 3"  "ROMNEY_time 3"
    ## 
    ## $`6`
    ## [1] "QUESTION_time 2"

Putting it Together
-------------------

I like working in a chain. In the setup below we work within a
**magrittr** pipeline to fit a model, select clusters, and examine the
results. In this example I do not condense the 2012 Presidential Debates
data by speaker and time, rather leaving every sentence as a separate
document. On my machine the initial `data_store` and model fit take ~5-8
seconds to run. Note that I do restrict the number of clusters (for
texts and terms) to a random 5 clusters for the sake of space.

    .tic <- Sys.time()

    myfit2 <- presidential_debates_2012 %>%
        with(data_store(dialogue)) %>%
        hierarchical_cluster()

    difftime(Sys.time(), .tic)

    ## Time difference of 5.794089 secs

    ## View Document Loadings
    ca2 <- assign_cluster(myfit2, k = 100)
    summary(ca2) %>% 
        head(12)

![](inst/figure/unnamed-chunk-13-1.png)

    ##    cluster count
    ## 1        2  1409
    ## 2       25    54
    ## 3       15    50
    ## 4       39    46
    ## 5       61    39
    ## 6       36    37
    ## 7       40    33
    ## 8       17    31
    ## 9       31    29
    ## 10      37    28
    ## 11      27    25
    ## 12      46    23

    ## Split Text into Clusters
    set.seed(3); inds <- sort(sample.int(100, 5))

    get_text(ca2)[inds] %>%
        lapply(head, 10)

    ## $`17`
    ##  [1] "But my experience my experience the private sector typically is able to provide a better product at a lower cost."                                                                                     
    ##  [2] "Well, in part, it comes, again, from my experience."                                                                                                                                                   
    ##  [3] "Which is which is my experience as a governor is if I come in and and lay down a piece of legislation and say, It's my way or the highway, I don't get a lot done."                                    
    ##  [4] "And by the way, I've had that experience."                                                                                                                                                             
    ##  [5] "But, ultimately, part of being principled, part of being a leader is, A, being able to describe exactly what it is that you intend to do, not just saying, I'll sit down, but you have to have a plan."
    ##  [6] "And so part of leadership and governing is both saying what it is that you are for, but also being willing to say no to some things."                                                                  
    ##  [7] "I've got three part time jobs."                                                                                                                                                                        
    ##  [8] "So he's got the oil and gas part, but he doesn't have the clean energy part."                                                                                                                          
    ##  [9] "That's part that's part one."                                                                                                                                                                          
    ## [10] "That's part of what I'm fighting for as president of the United States."                                                                                                                               
    ## 
    ## $`32`
    ##  [1] "It's hurt the housing market because Dodd Frank didn't anticipate putting in place the kinds of regulations you have to have."                         
    ##  [2] "And we face this deficit could crush the future generations."                                                                                          
    ##  [3] "Jeremy, first of all, your future is bright."                                                                                                          
    ##  [4] "And there are a bunch of things we can do to make sure your future is bright."                                                                         
    ##  [5] "We do those things, not only is your future going to be bright but America's future is going to bright as well."                                       
    ##  [6] "We've also got to look to the future."                                                                                                                 
    ##  [7] "It's critical to our future."                                                                                                                          
    ##  [8] "I'm not that optimistic as I was in two thousand twelve."                                                                                              
    ##  [9] "It's about who can get the middle class in this country a bright and prosperous future and assure our kids the kind of hope and optimism they deserve."
    ## [10] "I want one hundred percent of the American people to have a bright and prosperous future."                                                             
    ## 
    ## $`38`
    ##  [1] "I just don't know how the president could have come into office, facing twenty three million people out of work, rising unemployment, an economic crisis at the at the kitchen table, and spend his energy and passion for two years fighting for Obamacare instead of fighting for jobs for the American people."
    ##  [2] "And the proof of that is twenty three million people out of work."                                                                                                                                                                                                                                                
    ##  [3] "We've got we've got barely have three minutes left."                                                                                                                                                                                                                                                              
    ##  [4] "So we only have three three minutes left in the in the debate before we go to your closing statements."                                                                                                                                                                                                           
    ##  [5] "Well what you're seeing in this country is twenty three million people struggling to find a job."                                                                                                                                                                                                                 
    ##  [6] "There are three."                                                                                                                                                                                                                                                                                                 
    ##  [7] "An economy that has twenty three million people looking for work is not a strong economy."                                                                                                                                                                                                                        
    ##  [8] "Median income is down dollar four thousand three hundred a family and twenty three million Americans out of work."                                                                                                                                                                                                
    ##  [9] "We don't have to settle for twenty three million people struggling to find a good job."                                                                                                                                                                                                                           
    ## [10] "You can't have twenty three million people struggling to get a job."                                                                                                                                                                                                                                              
    ## 
    ## $`58`
    ## [1] "And the key to great schools, great teachers."
    ## [2] "You've done a great job."                     
    ## [3] "You're doing great."                          
    ## [4] "Great."                                       
    ## [5] "Great to see you."                            
    ## 
    ## $`80`
    ## [1] "And the president's right in terms of the additional oil production, but none of it came on federal land."                            
    ## [2] "As a matter of fact, oil production is down fourteen percent this year on federal land, and gas production was down nine percent."    
    ## [3] "Oil production is up, natural gas production is up, and, most importantly, we're also starting to build cars that are more efficient."
    ## [4] "And production on private on government land."                                                                                        
    ## [5] "Production is up."                                                                                                                    
    ## [6] "Production on government land of oil is down fourteen percent."                                                                       
    ## [7] "And production on gas."                                                                                                               
    ## [8] "I'm all for oil production."

    ## Get Associated Terms
    get_terms(ca2, term.cutoff = .07)[inds]

    ## $`17`
    ##         term    weight
    ## 1     israel 1.0000000
    ## 2       part 0.9239009
    ## 3 experience 0.7560640
    ## 
    ## $`32`
    ##     term weight
    ## 1 future      1
    ## 
    ## $`38`
    ##    term weight
    ## 1 three      1
    ## 
    ## $`58`
    ##    term weight
    ## 1 great      1
    ## 
    ## $`80`
    ##         term weight
    ## 1 production      1

An Experiment
-------------

It seems to me that if the hierarchical clustering is function as
expected we'd see topics clustering together within a conversation as
the natural eb and flow of a conversation is to talk around a topic for
a while and then move on to the next related topic. A Gantt style plot
of topics across time seems like an excellent way to observe clustering
across time. In the experiment I first ran the hierarchical clustering
at the sentence level for all participants in the 2012 presidential
debates data set. I then decided to use turn of talk as the unit of
analysis. Finally, I pulled out the two candidates (President Obama and
Romney) and faceted n their topic use over time.

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(dplyr, clustext, textshape, ggplot2, stringi)

    myfit3 <- presidential_debates_2012 %>%
        mutate(tot = gsub("\\..+$", "", tot)) %>%
        with(data_store(dialogue)) %>%
        hierarchical_cluster()

    plot(myfit3, 75)

![](inst/figure/unnamed-chunk-14-1.png)

Can & Ozkarahan's (1990) formula indicated a `k = 259`. This umber
seemed overly large. I used `k = 75` for the number of topics as it
seemed unreasonable that there'd be more topics than this but with
`k = 75` over half of the sentences loaded on one cluster. Note the use
of the `attribute` `join` from `assign_cluster` to make joining back to
the original data set easier.

    k <- 75
    ca3 <- assign_cluster(myfit3, k = k)

    presidential_debates_2012 %>%
        mutate(tot = gsub("\\..+$", "", tot)) %>%
        tbl_df() %>%
        attributes(ca3)$join() %>% 
        group_by(time) %>%
        mutate(
            word_count = stringi::stri_count_words(dialogue),
            start = starts(word_count),
            end = ends(word_count)
        ) %>%
        na.omit() %>%
        mutate(cluster = factor(cluster, levels = k:1)) %>%
        ggplot2::ggplot(ggplot2::aes(x = start-2, y = cluster, xend = end+2, yend = cluster)) +
            ggplot2::geom_segment(ggplot2::aes(position="dodge"), color = 'white', size = 3) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'grey20'),
                panel.grid.minor.x = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.y = ggplot2::element_blank(),
                panel.grid.major.y = ggplot2::element_line(color = 'grey35'),
                strip.text.y = ggplot2::element_text(angle=0, hjust = 0),
                strip.background = ggplot2::element_blank())  +
                ggplot2::facet_wrap(~time, scales='free', ncol=1) +
                ggplot2::labs(x="Duration (words)", y="Cluster")

    ## Joining by: "id_temporary"

![](inst/figure/unnamed-chunk-15-1.png)

Right away we notice that not all topics are used across all three
times. This is encouraging that the clustering is working as expected as
we'd expect some overlap in debate topics as well as some unique topics.
However, there were so many topics clustering on cluster 3 that I had to
make some decisions. I could (a) ignore this mass and essentially throw
out half the data that loaded on a single cluster, (b) increase `k` to
split up the mass loading on cluster 3, (c) change the unit of analysis.
It seemed the first option was wasteful of data and could miss
information. The second approach could lead to a model that had so many
topics it wouldn't be meaningful. The last approach seemed reasonable,
inspecting the cluster text showed that many were capturing functions of
language rather than content. For example, people use *"Oh."* to
indicate agreement. This isn't a topic but the clustering would group
sentences that use this convention together. Combining this sentence
with other sentences in the turn of talk are more likely to get the
content we're after.

Next I used the `textshape::combine` function to group turns of talk
together.

    myfit4 <- presidential_debates_2012 %>%
        mutate(tot = gsub("\\..+$", "", tot)) %>%
        textshape::combine() %>% 
        with(data_store(dialogue, stopwords = tm::stopwords("english"), min.char = 3)) %>%
        hierarchical_cluster()

    plot(myfit4, k = 80)

![](inst/figure/unnamed-chunk-16-1.png)

The distribution of turns of talk looked much more dispersed across
clusters. I used `k = 60` for the number of topics.

    k <- 80
    ca4 <- assign_cluster(myfit4, k = k)

    presidential_debates_2012 %>%
        mutate(tot = gsub("\\..+$", "", tot)) %>%
        textshape::combine() %>% 
        tbl_df() %>%
        attributes(ca4)$join() %>% 
        group_by(time) %>%
        mutate(
            word_count = stringi::stri_count_words(dialogue),
            start = starts(word_count),
            end = ends(word_count)
        ) %>%
        na.omit() %>%
        mutate(cluster = factor(cluster, levels = k:1)) %>%
        ggplot2::ggplot(ggplot2::aes(x = start-2, y = cluster, xend = end+2, yend = cluster)) +
            ggplot2::geom_segment(ggplot2::aes(position="dodge"), color = 'white', size = 3) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'grey20'),
                panel.grid.minor.x = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.y = ggplot2::element_blank(),
                panel.grid.major.y = ggplot2::element_line(color = 'grey35'),
                strip.text.y = ggplot2::element_text(angle=0, hjust = 0),
                strip.background = ggplot2::element_blank())  +
                ggplot2::facet_wrap(~time, scales='free', ncol=1) +
                ggplot2::labs(x="Duration (words)", y="Cluster")

    ## Joining by: "id_temporary"

![](inst/figure/unnamed-chunk-17-1.png)

The plots looked less messy and indeed topics do appear to be clustering
around one another. I wanted to see how the primary participants, the
candidates, compared to each other in topic use.

In this last bit of analysis I filter out all participants except Obama
and Romeny and facet by participant across time.

    myfit5 <- presidential_debates_2012 %>%
        mutate(tot = gsub("\\..+$", "", tot)) %>%
        textshape::combine() %>% 
        filter(person %in% c("ROMNEY", "OBAMA")) %>%
        with(data_store(dialogue, stopwords = tm::stopwords("english"), min.char = 3)) %>%
        hierarchical_cluster()


    plot(myfit5, 50)

![](inst/figure/unnamed-chunk-18-1.png)

Based on the dendrogram, I used `k = 50` for the number of topics.

    k <- 50
    ca5 <- assign_cluster(myfit5, k = k)

    presidential_debates_2012 %>%
        mutate(tot = gsub("\\..+$", "", tot)) %>%
        textshape::combine() %>% 
        filter(person %in% c("ROMNEY", "OBAMA")) %>%
        tbl_df() %>%
        attributes(ca5)$join() %>% 
        group_by(time) %>%
        mutate(
            word_count = stringi::stri_count_words(dialogue),
            start = starts(word_count),
            end = ends(word_count)
        ) %>%
        na.omit() %>%
        mutate(cluster = factor(cluster, levels = k:1)) %>%
        ggplot2::ggplot(ggplot2::aes(x = start-10, y = cluster, xend = end+10, yend = cluster)) +
            ggplot2::geom_segment(ggplot2::aes(position="dodge"), color = 'white', size = 3) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'grey20'),
                panel.grid.minor.x = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.y = ggplot2::element_blank(),
                panel.grid.major.y = ggplot2::element_line(color = 'grey35'),
                strip.text.y = ggplot2::element_text(angle=0, hjust = 0),
                strip.background = ggplot2::element_blank())  +
                ggplot2::facet_grid(person~time, scales='free', space='free') +
                ggplot2::labs(x="Duration (words)", y="Cluster")

    ## Joining by: "id_temporary"

![](inst/figure/unnamed-chunk-19-1.png)

If you're curious about the heaviest weighted tf-idf terms in each
cluster the next code chunk provides the top five weighted terms used in
each cluster. If a cluster has `...` no terms met the minimum tf-idf
cut-off. Below this I provide a bar plot of the frequencies of clusters
to help put the other information into perspective.

    invisible(Map(function(x, y){

        if (is.null(x)) {
            cat(sprintf("Cluster %s: ...\n", y))
        } else {
            m <- dplyr::top_n(x, 5, n)
            o <- paste(paste0(m[[1]], " (", m[[2]], ")"), collapse="; ")
            cat(sprintf("Cluster %s: %s\n", y, o))       
        }

    }, get_terms(ca5, .02), names(get_terms(ca5, .02))))

    ## Cluster 1: topic (1); sixteen (0.0996292336581667); going (0.0274679140836945); just (0.0235451854151687); rather (0.0232891499290035); generally (0.0200884564211926)
    ## Cluster 2: mary (1); second (0.324762078257752); president (0.095790768616159); provide (0.0489200251164163); note (0.0278525601747039); employers (0.0227390610675377)
    ## Cluster 3: sorry (1); name (0.244650336086723)
    ## Cluster 4: absolutely (1)
    ## Cluster 5: regulation (1); qualified (0.172411695500637); provisions (0.0691145479941463); people (0.0523475584092218); back (0.0336941892284982); qualify (0.0302658478144301)
    ## Cluster 6: yes (1); places (0.333471616628349)
    ## Cluster 7: bob (1); let (0.896260157972089); advice (0.253723310105413); give (0.172443079640244); well (0.0920861957021324); one (0.064256490734986)
    ## Cluster 8: matter (1); obamacare (0.0600314685730801); cause (0.038733435995732); ask (0.034522787435493); things (0.0275473541985628); condition (0.0265936366625109)
    ## Cluster 9: gets (1); way (0.417261226412456); america (0.0807917267588479); nobody's (0.0441470156737316); efficiently (0.0437646914633211); planes (0.0297316636170029)
    ## Cluster 10: time (1); issue (0.571831290878595); vitally (0.552968770339923); seen (0.133271062383573); get (0.0640555792501928)
    ## Cluster 11: well (1); speak (0.266383959699891); moment (0.244870652471098); need (0.102378365564775)
    ## Cluster 12: respond (1)
    ## Cluster 13: small (1); jobs (0.630570915550014); met (0.246240963763578); live (0.12218021918765); filing (0.0650829272143366)
    ## Cluster 14: government (1); people (0.389636989475016); place (0.109147223494582); disturbed (0.0517674122058484); complain (0.0282826837417318)
    ## Cluster 15: company (1); grow (0.0702089677871267); tesla (0.0310456251213221); basic (0.0250763041137348); choices (0.0249768152575664); lead (0.0218974299972695)
    ## Cluster 16: great (1); see (0.304062792606493); job (0.181818181818182); done (0.159199790413024)
    ## Cluster 17: role (1); syrian (0.103077892913994); council (0.0439348396026862); just (0.0300828652158312); responsible (0.029016692679092)
    ## Cluster 18: damage (1); got (0.336507749488007); provide (0.26906013814029); willingness (0.0909090909090909); parts (0.0734294768242195); course (0.0396947473356813)
    ## Cluster 19: energy (1); taking (0.212491892594422); just (0.075666785196478); europe (0.0367723848426351); given (0.0222784387070814)
    ## Cluster 20: yeah (1); guns (0.237024987145956); pieces (0.0899162996743652); legislation (0.061719604647929); presume (0.0615216787245657); two (0.0359775776398878)
    ## Cluster 21: detroit (1); answer (0.43234422555601); forget (0.216154821137528); governor (0.0673067781108058); people (0.0653948300063515)
    ## Cluster 22: oil (1); policy (0.223156189220638); true (0.0914738683287482); country (0.0893095101779236); it'll (0.0211625028109402)
    ## Cluster 23: federal (1); waters (0.703722795839302); land (0.591004041291982); problem (0.197001347097327); half (0.175022598917936)
    ## Cluster 24: true (1); probably (0.190103482545232); just (0.114966059411124); romney (0.099755545761792); well (0.0821552657297301)
    ## Cluster 25: much (1); cut (0.969821274781247)
    ## Cluster 26: question (1); much (0.0979853342476531); cut (0.0950282617699255); got (0.0312565440804192); get (0.024065896422711)
    ## Cluster 27: right (1)
    ## Cluster 28: actually (1); mine (0.301416895402805); leases (0.267926129246938); fact (0.140871165488306); take (0.104197909345632)
    ## Cluster 29: production (1); gas (0.240805100887951); government (0.145287425686538); private (0.112462878026246); percent (0.0635985608237335)
    ## Cluster 30: governor (1); romney (0.151183780998314); said (0.0250734620815368)
    ## Cluster 31: believe (1); case (0.248772382632109); answer (0.181112647200164); think (0.120890325995243); people (0.0913148218137486)
    ## Cluster 32: candy (1); think (0.0725060294706801); going (0.0478004818421371)
    ## Cluster 33: balance (1); business (0.123245938885084); twenty (0.0967919715586346); deficit (0.0957551301066314); changes (0.0207584505965127)
    ## Cluster 34: women (1); care (0.0808616691387846); said (0.0611241498942465); need (0.0611082589520729); says (0.0205969166029487)
    ## Cluster 35: want (1); opportunities (0.185097125209257); part (0.115593900112195); believe (0.110780165853653); forth (0.0206926567799711)
    ## Cluster 36: china (1); overseas (0.221101940776537); buying (0.0495251400438136); ways (0.048936440804696); encouraging (0.0219713518695042)
    ## Cluster 37: lorraine (1)
    ## Cluster 38: pension (1); blind (0.0924758666730826); last (0.0340337282631181); years (0.0256788458594185); just (0.0237146394791628)
    ## Cluster 39: record (1); often (0.453157720410901); airbrush (0.0400735879831331); anybody (0.0264258371659704); trying (0.0254893071963349)
    ## Cluster 40: pakistan (1); region (0.277568295734401); greet (0.0794542162865311); democratically (0.0775280534674637); fact (0.0364812153815521)
    ## Cluster 41: terror (1); rose (0.744259979098371); said (0.346776584392684); president (0.220621908873599); make (0.0877915672479779)
    ## Cluster 42: foreign (1); certainly (0.209676845912997); sorry (0.124248137175689); got (0.123149231138356); terrorism (0.0211685172962943)
    ## Cluster 43: happy (1); conversation (0.181411692986489); longer (0.170103452689211)
    ## Cluster 44: indicated (1); weeks (0.132505525664538); place (0.104873847882916); still (0.0918287959562528); just (0.0495036699443204)
    ## Cluster 45: syria (1); anticipate (0.215051053403125); libya (0.182119421762312); friends (0.182119421762312); well (0.042874597932434)
    ## Cluster 46: ten (1); earlier (0.778028980787231); years (0.550802881805172); took (0.428571428571429); office (0.365356164848883)
    ## Cluster 47: iran (1); said (0.221887262127047); mullahs (0.0690796816284301); probably (0.0592997833527262); everything (0.0592997833527262); thing (0.0562586400486147); capable (0.0556691116998602); tried (0.0434875108306748)
    ## Cluster 48: liquidate (1); said (0.137965532084574); bankruptcy (0.117256789654769); circumstances (0.0506846117528302); suggested (0.0480710692590781); nothing (0.0298719772403983)
    ## Cluster 49: look (1); people (0.444223972703424); can (0.440009921054686); take (0.354977414280176); will (0.300573987586919)
    ## Cluster 50: wrong (1); way (0.0799623598858537)

    invisible(summary(ca5))

![](inst/figure/unnamed-chunk-21-1.png)

It appears that in fact the topics do cluster within segments of time as
we'd expect. This is more apparent when turn of talk is used as the unit
of analysis (document level) rather than each sentence.