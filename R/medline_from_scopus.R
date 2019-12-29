medline_from_scopus <- function(x) {
    UseMethod(medline_from_scopus, x)
}

medline_from_scopus.scopus_complete <- function(x) {
    list("0" = character())
}

medline_from_scopus.scopus_standard <- function(x) {
    NULL
}
