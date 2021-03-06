# bibsearch 0.2.1

* Bug fix when empty doi is included in a retrieved record
* Added some tests

# bibsearch 0.2.0

* Refactored searches for PubMed and Scopus, added main tests

* Updated `README`

* Added `search_on_scopus()` to retrieve a bibliography from a search 
  on Scopus, with option of select the amount of records to retrieve;

# bibsearch 0.1.0

* Removed teh dependency from the package **depigner** (because it is
  on GitHub only);

* Added `write_bibliography()` to write a bibliography on disk, with
  options to split the file accordingly to the number of references into
  each one;

* Added `search_on_pubmed()` to retrieve a bibliography from a search 
  on PubMed, with option of select the amount of records to retrieve;

* Added support for usethis' UI.

# bibsearch (development version)

* Added basic development support:

  - git + GitHub +
    * `.github/CODE_OF_CONDUCT.md`
    * `.github/CONTRIBUTING.md`
    * `.github/ISSUE_TEMPLATE.md`
    * `.github/SUPPORT.md`

  - appVeyor + Travis + codecov;

  - gpl3 license;

  - testthat + roxygen2 + spellcheck;

  - `` magrittr::`%>%` ``;

  - `README.Rmd` + `README.md`;

  - `cran0comments.md`.

* Added a `NEWS.md` file to track changes to the package.

# bibsearch 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
