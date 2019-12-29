#' Perform a search on PubMed
#'
#' This function wraps
#' \code{easyPubMed::\link[easyPubMed]{get_pubmed_ids}} and
#' \code{easyPubMed::\link[easyPubMed]{fetch_pubmed_data}} to collect
#' results of a query to
#' [PubMed](https://www.ncbi.nlm.nih.gov/pubmed/)
#'
#'
#' @param search_string (character vector of length 1) that is used for
#'   querying PubMed (standard PubMed syntax, see reference for
#'   details).
#' @param max_to_get Integer (>= 1): maximum number of records to
#'   retrieve. Note that, if they are not finished, a complete batch
#'   will be always retrieved; hence, more than `max_to_get` records
#'   will be retrieved Set it to `Inf` to be sure to retrieve all
#'   the possible results.
#' @param api_key (character vector of length 1): user-specific API key
#'   to increase the limit of queries per second above three. You can
#'   obtain your key from NCBI (see
#'   \url{https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/}
#'
#' @return an object of class `pubmed`
#' @export
#'
#' @examples
#' \dontrun{
#'     pubmed_search("machine learning", 1)
#'     pubmed_search("aaaa", 1) # no result
#' }
pubmed_search <- function(
    search_string,
    max_to_get = 25,
    api_key = NULL
) {

# formal checks ---------------------------------------------------
    assertive::assert_is_a_non_empty_string(search_string)

    (max_to_get == Inf) || (
        assertive::assert_is_a_number(max_to_get) &&
            assertive::assert_all_are_whole_numbers(max_to_get) &&
            assertive::assert_all_are_positive(max_to_get)
    )

    if (!is.null(api_key)) assertive::assert_is_a_string(api_key)


# perform the search ----------------------------------------------
    entrez_ids <- easyPubMed::get_pubmed_ids(search_string, api_key)

    # We need to iterated over steps of size = batch_size to retrieve
    # all the records (with a maximum of 5000)
    n_records <- min(as.integer(entrez_ids[["Count"]]), max_to_get)

    if (!n_records) {
        ui_warn("The search produced no result.")
        res <- list("0" = character(0L))
        class(res) <- c("pubmed", "medline")
        return(res)
    }

    batch_size <- min(n_records, 5000L)

    steps <- purrr::set_names(
        seq(from = 0L, to = n_records - 1, by = batch_size)
    )

    ui_todo("starting retrieving records from PubMed.")
    pb <- pb_len(length(steps))

    res <- purrr::imap(steps, ~ {
        res <- easyPubMed::fetch_pubmed_data(
            entrez_ids,
            retstart = .x,
            retmax = batch_size,
            format = "medline"
        )
        tick(pb, .y)
        res
    })

    class(res) <- c("pubmed", "medline")
    ui_done("Search on PubMed completed")
    res

}
