NEWS
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor
  and patch)
* New additions without breaking backward compatibility bumps the minor
  (and resets the patch)
* Bug fixes and misc changes bumps the patch



clustext 0.1.0-
----------------------------------------------------------------

**BUG FIXES**

* `join` from an `assign_cluster`'s `attributes`, now explicitly joins on (`by`)
  `'id_temporary'`.

**NEW FEATURES**

* `as_topic` added to convert `get_terms` to more print friendly cluster-topic-terms
  display.
  
* `write_cluster_text` & `read_cluster_text` added to write/read cluster text for 
  human categorization.
  
* `categorize` added to join original data, clusters, and human categories.

**MINOR FEATURES**

**IMPROVEMENTS**

**CHANGES**

clustext 0.0.1
----------------------------------------------------------------

This package is a collection of tools for optimized, consistent clustering of text data.