#' Scopus search
#'
#' @param search_string (character vector of length 1) that is used for
#'   querying PubMed (standard Scopus syntax, see reference for
#'   details).
#' @param max_to_get Integer (>= 1): maximum number of records to
#'   retrieve. Maximum 20000/week overall!
#'
#' @return an object of class `scopus`
#'
#' @seealso [create_bibliography].
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     scopus_search("machine learning", 2)
#' }
scopus_search <- function(search_string, max_to_get = 25) {
    if (complete_search_works()) {
        complete_scopus_search(search_string, max_to_get)
    } else {
        standard_scopus_search(search_string, max_to_get)
    }
}

#' Complete Scopus search
#'
#' @describeIn scopus_search Complete Search
#' @return an object of class `scopus-complete`
#' @export
#'
#' @examples
#' \dontrun{
#'     complete_scopus_search("machine learning", 2)
#' }
complete_scopus_search <- function(search_string, max_to_get = 25) {

# formal checks ---------------------------------------------------
    check_scopus_pat()

    assertive::assert_is_a_string(search_string)

    assertive::assert_is_a_number(max_to_get)
    assertive::assert_all_are_whole_numbers(max_to_get)
    assertive::assert_all_are_positive(max_to_get)


# perform the search ----------------------------------------------
    ui_todo("starting retrieving records from Scopus.")
    ui_info("'COMPLETE' view is used")

    res <- rscopus::scopus_search(
        query = search_string,
        count = min(25, max_to_get),
        view  = "COMPLETE",
        max_count = max_to_get,
        wait_time = 0.12,   # max 9 requests/second
        verbose = FALSE
    )

    class(res) <- c("scopus", "scopus_complete", class(res))
    ui_done("Search on Scopus completed")
    res

}





#' Standard scopus search
#'
#' @describeIn scopus_search Complete Search
#'
#' @return an object of class `scopus-standard`
#' @export
#'
#' @examples
#' \dontrun{
#'     standard_scopus_search("machine learning", 2)
#' }
standard_scopus_search <- function(search_string, max_to_get = 25) {

# formal checks ---------------------------------------------------
    check_scopus_pat()

    assertive::assert_is_a_string(search_string)

    assertive::assert_is_a_number(max_to_get)
    assertive::assert_all_are_whole_numbers(max_to_get)
    assertive::assert_all_are_positive(max_to_get)


# perform the search ----------------------------------------------
    ui_todo("starting retrieving records from Scopus.")
    ui_info("'STANDARD' view is used")

    res <- rscopus::scopus_search(
        query = search_string,
        count = min(200, max_to_get),
        view  = "STANDARD",
        max_count = max_to_get,
        wait_time = 0.12,   # max 9 requests/second
        verbose = FALSE
    )

    class(res) <- c("scopus", "scopus_standard", class(res))
    ui_done("Search on Scopus completed")
    res
}
