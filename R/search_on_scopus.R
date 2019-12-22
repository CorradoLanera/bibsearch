#' Perform a search on Scopus
#'
#' This function wraps
#' \code{rscopus::\link[rscopus]{scopus_search}} to collect
#' results of a query to
#' [Scopus](https://www.scopus.com/)
#'
#' @param search_string (character vector of length 1) that is used for
#'   querying PubMed (standard Scopus syntax, see reference for
#'   details).
#' @param max_to_get Integer (>= 1): maximum number of records to
#'   retrieve. Maximum 20000/week overall!
#' @param batch_size Integer (>= 1): size of the batch of Scopus records
#'   to be retrieved at each time. Maximum is 200 for "STANDARD" type of
#'   search, and 25 for "COMPLETE" one.
#' @param type_of_view (character vector of length 1) one of "STANDARD"
#'   or "COMPLETE", see reference for details.
#'
#' @return An object of class
#'   \code{\link[revtools]{bibliography-class}}.
#' @export
#'
#' @references
#' \url{http://schema.elsevier.com/dtds/document/bkapi/search/SCOPUSSearchTips.htm}
#'
#' \url{https://dev.elsevier.com/api_key_settings.html}
#'
#' \url{https://dev.elsevier.com/tecdoc_developer_faq.html}
#'
#' @examples
#' \dontrun{
#'     # This retrieve a (short) set of results (batch size auto-resize
#'     # itself).
#'     search_on_scopus("ALL(machine AND learning) AND AUTHOR-NAME (lanera)")
#' }
search_on_scopus <- function(
    search_string,
    max_to_get = 25,
    batch_size = 25,
    type_of_view = "STANDARD"

) {

# formal checks ---------------------------------------------------
check_scopus_pat()

    assertive::assert_is_a_string(search_string)

    assertive::assert_is_a_number(batch_size)
    assertive::assert_all_are_whole_numbers(batch_size)
    assertive::assert_all_are_positive(batch_size)
    assertive::assert_all_are_less_than_or_equal_to(batch_size, 200)

    assertive::assert_is_a_number(max_to_get)
    assertive::assert_all_are_whole_numbers(max_to_get)
    assertive::assert_all_are_positive(max_to_get)
    assertive::assert_all_are_less_than_or_equal_to(batch_size, 20000)

    type_of_view <- match.arg(type_of_view)



# retrieve records ------------------------------------------------

    retrived_lists <- rscopus::scopus_search(
        query = search_string,
        count = batch_size,
        view  = type_of_view,
        max_count = max_to_get,
        wait_time = 0.2   # max 6 requests/second
    )

    ui_oops("Returned object must be converted to class bibliography!")
    ui_info("Retrieved list is returned invisibly")
    invisible(retrived_lists)


# Set-up the bibliography -----------------------------------------


}
