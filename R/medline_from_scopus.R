#' Convert Scopus search to medline format
#'
#' Convert scopus searches by [complete_scopus_search] or
#' [standard_scopus_search] to medline bibliography format.
#'
#' Methods are provided for both "STANDARD" and "COMPLETE" views on
#' Scopus.
#'
#' @param x An object of class `scopus`
#'
#' @return an object of class `medline`
#' @export
#'
#' @examples
#' \dontrun{
#'     medline_from_scopus(scopus_search("machine learning", 2))
#' }
medline_from_scopus <- function(x) {
    UseMethod("medline_from_scopus", x)
}



#' @rdname medline_from_scopus
#' @export
medline_from_scopus.scopus_complete <- function(x) {
    med <- purrr::map_chr(x[["entries"]], complete_medline)
    med <- list(
        "0" = purrr::flatten_chr(stringr::str_split(med, "\\n"))
    )
    class(med) <- c("medline")
    med
}



#' @rdname medline_from_scopus
#' @export
medline_from_scopus.scopus_standard <- function(x) {
    med <- purrr::map_chr(x[["entries"]], standard_medline)
    med <- list(
        "0" = purrr::flatten_chr(stringr::str_split(med, "\\n"))
    )
    class(med) <- c("medline")
    med
}






complete_medline <- function(self) {
    self <- fix_null_entries(self)

    # article ID (AID), source ID (SI)
    aid <- self$eid
    si <- self$`source-id`

    # issn (IS)
    is_link <- paste(self$`prism:issn`, "(Linking)")
    is_e <- paste(self$`prism:eIssn`, "(Electronic)")

    # date of pubblication (DP)
    dp   <- self$`prism:coverDate`

    # title
    ti <- self$`dc:title`

    # doi
    lid <- self$`prism:doi`

    # Other ID (OID)
    split_title <- stringr::str_split(ti, " ")[[1]]
    first <- split_title[[1]]
    last  <- split_title[length(split_title)]
    year <- stringr::str_sub(dp, 1, 4)
    first_auth_last_name <- self$author[[1]]$`surname`[[1]]
    oid <- paste0(first_auth_last_name, year, first, last)

    # abstract (AB)
    ab <- self$`dc:description`

    # Copyright informatyion (CI)
    ci <- paste(
        purrr::map_chr(self$affiliation, "affilname"),
        collapse = "; "
    )


    # authors
    au <- purrr::map_chr(self$author, "authname")
    authors_given   <- purrr::map_chr(self$author, "given-name")
    authors_surname <- purrr::map_chr(self$author, "surname")
    fau <- paste(authors_surname, authors_given, sep = ", ")
    auid <- purrr::map_chr(self$author, "authid")


    # pages
    pages <- self$`prism:pageRange`
    if (is.null(pages)) {
        if (!is.null(self$`prism:startingPage`) & !is.null(self$`prism:endingPage`)) {
            pages <- paste0(self$`prism:startingPage`, "-", self$`prism:endingPage`)
        }
        else {
            pages <- "-"
        }
    }


    # Publicetion type (PT)
    pt <- paste(self$`prism:aggregationType`, self$subtypeDescription)


    # Journal title (JT)
    jt <- self$`prism:publicationName`

    # keywords (OT)
    ot <- stringr::str_split(self$authkeywords, " \\| ")[[1]]

    # source (SO)
    so <- self$`prism:url`

    # issue (IP) and volume (VI)
    ip <- self$`prism:issueIdentifier`
    vi <- self$`prism:volume`

    glue::glue("
    PT  - {pt}
    AID - {aid}
    SI  - {si}
    IS  - {is_link}
    IS  - {is_e}
    DP  - {dp}
    TI  - {ti}
    LID - {lid}
    OID - {oid}
    AB  - {ab}
    CI  - {ci}
    {paste(paste('AU  -', au), paste('FAU -', fau), paste('AUID-', auid), sep = '\\n', collapse = '\\n')}
    JT  - {jt}
    {paste('OT  -', ot, collapse = '\\n')}
    SO  - {so}
    IP  - {ip}
    VI  - {vi}
    PG  - {pages}

    ")
}


standard_medline <- function(self) {
    self <- fix_null_entries(self)

    # article ID (AID), source ID (SI)
    aid <- self$eid
    si <- self$`source-id`

    # issn (IS)
    is_link <- paste(self$`prism:issn`, "(Linking)")
    is_e <- paste(self$`prism:eIssn`, "(Electronic)")

    # date of pubblication (DP)
    dp   <- self$`prism:coverDate`

    # title
    ti <- self$`dc:title`

    # doi
    lid <- self$`prism:doi`

    # Other ID (OID)
    split_title <- stringr::str_split(ti, " ")[[1]]
    first <- split_title[[1]]
    last  <- split_title[length(split_title)]
    year  <- stringr::str_sub(dp, 1, 4)
    au    <- self$`dc:creator`
    first_auth_last_name <- stringr::str_extract(au, "\\S+")
    oid   <- paste0(first_auth_last_name, year, first, last)
    auid  <- ""


    # abstract (AB)
    ab <- ""

    # Copyright informatyion (CI)
    ci <- paste(
        purrr::map_chr(self$affiliation, "affilname"),
        collapse = "; "
    )


    # author (first only in STANDARD view)
    authors_given   <- stringr::str_extract(au, "\\S+$")
    authors_surname <- first_auth_last_name
    fau <- paste(authors_surname, authors_given, sep = ", ")


    # pages
    pages <- self$`prism:pageRange`
    if (is.null(pages)) {
        if (!is.null(self$`prism:startingPage`) & !is.null(self$`prism:endingPage`)) {
            pages <- paste0(self$`prism:startingPage`, "-", self$`prism:endingPage`)
        } else {
            pages <- "-"
        }
    }


    # Publicetion type (PT)
    pt <- paste(self$`prism:aggregationType`, self$subtypeDescription)


    # Journal title (JT)
    jt <- self$`prism:publicationName`

    # keywords (OT)
    ot <- ""

    # source (SO)
    so <- self$`prism:url`

    # issue (IP) and volume (VI)
    ip <- self$`prism:issueIdentifier`
    vi <- self$`prism:volume`

    glue::glue("
    PT  - {pt}
    AID - {aid}
    SI  - {si}
    IS  - {is_link}
    IS  - {is_e}
    DP  - {dp}
    TI  - {ti}
    LID - {lid}
    OID - {oid}
    AB  - {ab}
    CI  - {ci}
    {paste(paste('AU  -', au), paste('FAU -', fau), paste('AUID-', auid), sep = '\\n', collapse = '\\n')}
    JT  - {jt}
    {paste('OT  -', ot, collapse = '\\n')}
    SO  - {so}
    IP  - {ip}
    VI  - {vi}
    PG  - {pages}

    ")
}
