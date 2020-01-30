Building a Mary site
--------------------

A Mary site is organised using the filesystem. Each page sits in
its own subdirectory. For instance, the page `events/lectures/2020-01-20`
is defined by the content of the directory `events/lectures/2020-01-20`.
We expect:

* A `blah.mary` file
* (optional) a `pub/` directory for public content (images, pdfs, etc.) that
  can be embedded into `blah.mary`


Administering a Mary page
-------------------------

From Mary's point of view, every page comes with extra information:

* a `config` file spelling out what the access control policies are
* a `log/` directory for user interactions

The `log/` directory is also structured in a hierarchical manner:
it contains a number of `sessionNNNN` subdirectory (one per session),
each of which contains a number of `uid` subdirectories (one per user).
