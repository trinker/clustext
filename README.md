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
topics to be determined after the odel has been fit.

With algorithms that require a similarity matrix (e.g., hierarchical
clustering) we apply cosine or jaccard distance measures to compare the
terms (or features) of each document. I have found cosine distance to
work well with sparse matrices to produce distances metrics between the
documents. The clustering model is fit to separate the documents into
clusters. In the case of some clustering techniques (e.g., hierarchical
clustering) the user then may apply k clusters to the fit, clustering
documents with similar important text features. Other techniques require
that `k` be specified prior to fitting the model. The documents can then
be grouped by clusters and their accompanying salient words extracted as
well.


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
<td align="left"><code>nfm_cluster</code></td>
<td align="left">cluster fit</td>
<td align="left">Fits a non-negative matrix factorization cluster model</td>
</tr>
<tr class="odd">
<td align="left"><code>assign_cluster</code></td>
<td align="left">assignment</td>
<td align="left">Assigns cluster to document/text element</td>
</tr>
<tr class="even">
<td align="left"><code>get_text</code></td>
<td align="left">extraction</td>
<td align="left">Get text from various <strong>clustext</strong> objects</td>
</tr>
<tr class="odd">
<td align="left"><code>get_dtm</code></td>
<td align="left">extraction</td>
<td align="left">Get <code>tm::DocumentTermMatrix</code> from various <strong>clustext</strong> objects</td>
</tr>
<tr class="even">
<td align="left"><code>get_removed</code></td>
<td align="left">extraction</td>
<td align="left">Get removed text elements from various <strong>clustext</strong> objects</td>
</tr>
<tr class="odd">
<td align="left"><code>get_terms</code></td>
<td align="left">extraction</td>
<td align="left">Get clustered weighted important terms from an <strong>assign_cluster</strong> object</td>
</tr>
<tr class="even">
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

    ## Text Elements      : 10
    ## Elements Removed   : 0
    ## Documents          : 10
    ## Terms              : 3,369
    ## Non-/sparse entries: 7713/25977
    ## Sparsity           : 77%
    ## Maximal term length: 16

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
    ## k approximated to: 4

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

    get_terms(ca, .075)

    ## $`2`
    ##          term n
    ## 1        each 2
    ## 2   gentlemen 2
    ## 3          go 2
    ## 4       leave 2
    ## 5     minutes 2
    ## 6      mister 2
    ## 7       night 2
    ## 8      romney 2
    ## 9     segment 2
    ## 10      segue 2
    ## 11        sir 2
    ## 12 statements 2
    ## 
    ## $`3`
    ##          term n
    ## 1       banks 2
    ## 2       board 2
    ## 3        care 2
    ## 4     federal 2
    ## 5      health 2
    ## 6   insurance 2
    ## 7    medicare 2
    ## 8        plan 2
    ## 9  republican 2
    ## 10     that's 2
    ## 11       they 2
    ## 
    ## $`4`
    ##          term n
    ## 1        coal 2
    ## 2 immigration 2
    ## 3        jobs 2
    ## 4         oil 2
    ## 5  production 2
    ## 6        sure 2
    ## 7      that's 2
    ## 8       women 2
    ## 
    ## $`5`
    ##         term n
    ## 1       home 2
    ## 2       iran 2
    ## 3     israel 2
    ## 4   military 2
    ## 5    nuclear 2
    ## 6  sanctions 2
    ## 7      stand 2
    ## 8       sure 2
    ## 9       they 2
    ## 10    threat 2
    ## 11    troops 2

### Clusters, Terms, and Docs Plot

Here I plot the clusters, terms, and documents (grouping variables)
together as a combined heatmap. This can be useful for viewing &
comparing what documents are clustering together in the context of the
cluster's salient terms. This example also shows how to use the cluster
terms as a lookup key to extract probable salient terms for a given
document.

    key <- data_frame(
        cluster = 1:6,
        labs = get_terms(ca, .085) %>%
            bind_list("cluster") %>%
            select(-n) %>%
            group_by(cluster) %>%
            summarize(term=paste(term, collapse=", ")) %>%
            apply(1, paste, collapse=": ") %>%
            c("1:", ., "6:")
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

    ## Time difference of 7.803249 secs

    ## View Document Loadings
    ca2 <- assign_cluster(myfit2, k = 100)
    summary(ca2) %>% 
        head(12)

![](inst/figure/unnamed-chunk-13-1.png)

    ##    cluster count
    ## 1        7   692
    ## 2        3   368
    ## 3       33   133
    ## 4        5   106
    ## 5       59    67
    ## 6        8    57
    ## 7       61    51
    ## 8       53    48
    ## 9       13    47
    ## 10      27    41
    ## 11      38    40
    ## 12      12    37

    ## Split Text into Clusters
    set.seed(3); inds <- sort(sample.int(100, 5))

    get_text(ca2)[inds] %>%
        lapply(head, 10)

    ## $`17`
    ##  [1] "One last point I want to make."                        
    ##  [2] "Now, the last point I'd make before|"                  
    ##  [3] "They put a plan out."                                  
    ##  [4] "They put out a plan, a bipartisan plan."               
    ##  [5] "Let me make one last point."                           
    ##  [6] "And Governor Romney's says he's got a five point plan?"
    ##  [7] "Governor Romney doesn't have a five point plan."       
    ##  [8] "He has a one point plan."                              
    ##  [9] "My five point plan does it."                           
    ## [10] "But the last point I want to make is this."            
    ## 
    ## $`32`
    ##  [1] "I think this is a great example."                                                                                                 
    ##  [2] "I think something this big, this important has to be done on a bipartisan basis."                                                 
    ##  [3] "Governor Romney said this has to be done on a bipartisan basis."                                                                  
    ##  [4] "This was a bipartisan idea."                                                                                                      
    ##  [5] "This is a this is an important election and I'm concerned about America."                                                         
    ##  [6] "I I know this is bigger than an election about the two of us as individuals."                                                     
    ##  [7] "It's an election about the course of America."                                                                                    
    ##  [8] "Well, think about what the governor think about what the governor just said."                                                     
    ##  [9] "This is not just a women's issue, this is a family issue, this is a middle class issue, and that's why we've got to fight for it."
    ## [10] "Mister President why don't you get in on this quickly, please?"                                                                   
    ## 
    ## $`38`
    ##  [1] "I will make sure we don't hurt the functioning of our of our marketplace and our business, because I want to bring back housing and get good jobs."                                                                                                                                                                                                                                                                                                                   
    ##  [2] "And hard pressed states right now can't all do that."                                                                                                                                                                                                                                                                                                                                                                                                                 
    ##  [3] "And everything that I've tried to do, and everything that I'm now proposing for the next four years in terms of improving our education system or developing American energy or making sure that we're closing loopholes for companies that are shipping jobs overseas and focusing on small businesses and companies that are creating jobs here in the United States, or closing our deficit in a responsible, balanced way that allows us to invest in our future."
    ##  [4] "But not just jobs, good paying jobs."                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ##  [5] "I want to do that in industries, not just in Detroit, but all across the country and that means we change our tax code so we're giving incentives to companies that are investing here in the United States and creating jobs here."                                                                                                                                                                                                                                  
    ##  [6] "I expect those new energy sources to be built right here in the United States."                                                                                                                                                                                                                                                                                                                                                                                       
    ##  [7] "This is about bringing good jobs back for the middle class of America, and that's what I'm going to do."                                                                                                                                                                                                                                                                                                                                                              
    ##  [8] "And that's creating jobs."                                                                                                                                                                                                                                                                                                                                                                                                                                            
    ##  [9] "When you've got thousands of people right now in Iowa, right now in Colorado, who are working, creating wind power with good paying manufacturing jobs, and the Republican senator in that in Iowa is all for it, providing tax breaks to help this work and Governor Romney says I'm opposed."                                                                                                                                                                       
    ## [10] "Candy, I don't have a policy of stopping wind jobs in Iowa and that they're not phantom jobs."                                                                                                                                                                                                                                                                                                                                                                        
    ## 
    ## $`58`
    ##  [1] "And the question is this."                                                                   
    ##  [2] "Your question your question is one that's being asked by college kids all over this country."
    ##  [3] "Mister President, the next question is going to be for you here."                            
    ##  [4] "and the next question."                                                                      
    ##  [5] "And the next question is for you."                                                           
    ##  [6] "Governor, this question is for you."                                                         
    ##  [7] "And Mister President, the next question is for you, so stay standing."                       
    ##  [8] "And it's Katherine Fenton, who has a question for you."                                      
    ##  [9] "Well, Katherine, that's a great question."                                                   
    ## [10] "But the president does get this question."                                                   
    ## 
    ## $`80`
    ##  [1] "Natural gas production is the highest it's been in decades."                                                                                                                                                                                                      
    ##  [2] "We have seen increases in coal production and coal employment."                                                                                                                                                                                                   
    ##  [3] "Look, I want to make sure we use our oil, our coal, our gas, our nuclear, our renewables."                                                                                                                                                                        
    ##  [4] "But what we don't need is to have the president keeping us from taking advantage of oil, coal and gas."                                                                                                                                                           
    ##  [5] "This has not been Mister Oil, or Mister Gas, or Mister Coal."                                                                                                                                                                                                     
    ##  [6] "I was in coal country."                                                                                                                                                                                                                                           
    ##  [7] "The head of the EPA said, You can't build a coal plant."                                                                                                                                                                                                          
    ##  [8] "And natural gas isn't just appearing magically."                                                                                                                                                                                                                  
    ##  [9] "And when I hear Governor Romney say he's a big coal guy, I mean, keep in mind, when Governor, when you were governor of Massachusetts, you stood in front of a coal plant and pointed at it and said, This plant kills, and took great pride in shutting it down."
    ## [10] "With respect to something like coal, we made the largest investment in clean coal technology, to make sure that even as we're producing more coal, we're producing it cleaner and smarter."

    ## Get Associated Terms
    get_terms(ca2, term.cutoff = .07)[inds]

    ## $`17`
    ##          term n
    ## 1       point 7
    ## 2        plan 6
    ## 3 president's 2
    ## 
    ## $`32`
    ##         term n
    ## 1      issue 4
    ## 2     please 4
    ## 3   election 3
    ## 4      think 3
    ## 5 bipartisan 2
    ## 6    mistake 2
    ## 
    ## $`38`
    ##          term n
    ## 1   investing 3
    ## 2 investments 3
    ## 3   companies 2
    ## 4        jobs 2
    ## 
    ## $`58`
    ##        term n
    ## 1  question 5
    ## 2 katherine 2
    ## 3      next 2
    ## 
    ## $`80`
    ##      term n
    ## 1    coal 4
    ## 2 natural 3

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

    ## Cluster 1: medicare (5); back (2); topic (2)
    ## Cluster 2: can (3); get (3); just (3); candy (2); chance (2); going (2); indicated (2); major (2); mister (2); president (2); private (2); product (2); second (2); someone (2); spontaneous (2); still (2)
    ## Cluster 3: sorry (3)
    ## Cluster 4: absolutely (3)
    ## Cluster 5: dodd (3); frank (3); regulation (3); banks (2)
    ## Cluster 6: yes (3)
    ## Cluster 7: let (4); bob (2)
    ## Cluster 8: ...
    ## Cluster 9: first (3); one (3); way (3); become (2); came (2); cut (2); governor (2); israel (2); nation (2); number (2); office (2); sunday (2)
    ## Cluster 10: time (3); issue (2); used (2)
    ## Cluster 11: well (3)
    ## Cluster 12: respond (2)
    ## Cluster 13: ...
    ## Cluster 14: choice (2); economy (2); election (2); forward (2); whether (2)
    ## Cluster 15: companies (2); investing (2)
    ## Cluster 16: great (3)
    ## Cluster 17: done (3); leadership (3); get (2); role (2)
    ## Cluster 18: say (3); party (2)
    ## Cluster 19: energy (3)
    ## Cluster 20: yeah (3); good (2); thanks (2)
    ## Cluster 21: detroit (3); answer (2)
    ## Cluster 22: coal (3); oil (3); bunch (2); governor (2); iowa (2); jobs (2); wind (2)
    ## Cluster 23: cut (2); federal (2); land (2); licenses (2); permits (2); waters (2)
    ## Cluster 24: true (4)
    ## Cluster 25: cut (3); much (3)
    ## Cluster 26: question (5); answer (3)
    ## Cluster 27: right (2)
    ## Cluster 28: actually (3); got (2)
    ## Cluster 29: production (4); government (2); land (2)
    ## Cluster 30: governor (9); romney (2)
    ## Cluster 31: believe (2)
    ## Cluster 32: candy (5)
    ## Cluster 33: balance (2); budget (2); military (2); trillion (2)
    ## Cluster 34: women (3)
    ## Cluster 35: want (5); make (4); sure (4); immigration (2)
    ## Cluster 36: ...
    ## Cluster 37: lorraine (2)
    ## Cluster 38: pension (4); chinese (2); investments (2); looked (2); mister (2); outside (2); trust (2)
    ## Cluster 39: check (3); record (3)
    ## Cluster 40: pakistan (2)
    ## Cluster 41: act (3); attack (3); terror (3); day (2); garden (2); rose (2); said (2)
    ## Cluster 42: troops (4); agreement (2); forces (2); thought (2); thousand (2)
    ## Cluster 43: happy (3)
    ## Cluster 44: indicated (3)
    ## Cluster 45: syria (3)
    ## Cluster 46: ten (2); years (2)
    ## Cluster 47: iran (2)
    ## Cluster 48: industry (3); liquidate (3)
    ## Cluster 49: look (3); can (2); people (2)
    ## Cluster 50: wrong (4)

    invisible(summary(ca5))

![](inst/figure/unnamed-chunk-21-1.png)

It appears that in fact the topics do cluster within segments of time as
we'd expect. This is more apparent when turn of talk is used as the unit
of analysis (document level) rather than each sentence.